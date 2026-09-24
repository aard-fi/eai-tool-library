;;; eai-code-confirm.el --- Tool call confirmation handling for eai-code -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;; Keywords: tools
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;;; Commentary:
;;
;; Confirmation handling for eai-code tool calls.
;;
;; Features:
;; - Inline confirmation in the chat buffer (no popup dialogs)
;; - Auto-allow based on session or project settings
;; - Tool grouping (file-edit, shell, eval, etc.)
;; - Alert notification when confirmation is needed while buffer is hidden
;;
;; Key commands (on tool call confirmation overlays):
;; - C-c C-c   Accept tool call(s)
;; - C-c C-k   Cancel tool call(s)
;; - C-c C-i   Inspect/edit tool call(s)
;; - C-c C-a   Allow this tool for the current session
;; - C-c C-p   Allow this tool for the current project
;;
;;; Code:

(require 'cl-lib)
(require 'project)

;; Optional: alert for background notifications
(declare-function alert "alert" (message &rest args &key severity title icon category buffer mode data style persistent never-persist id))

(defgroup eai-code-confirm nil
  "Tool call confirmation handling for eai-code."
  :group 'eai-code)

(defcustom eai-code-confirm-style 'inline
  "How to display tool call confirmation prompts.
`inline'  – show in chat buffer with overlay (default)
`minibuffer' – use minibuffer prompt
`popup'   – use a popup frame/dialog"
  :type '(choice (const :tag "Inline in chat buffer" inline)
                 (const :tag "Minibuffer prompt" minibuffer)
                 (const :tag "Popup dialog" popup))
  :group 'eai-code-confirm)

(defcustom eai-code-confirm-alert t
  "When non-nil, send an alert notification if a tool call needs
confirmation while the eai-code buffer is not visible."
  :type 'boolean
  :group 'eai-code-confirm)

(defcustom eai-code-confirm-project-read-write nil
  "When non-nil, auto-allow all file-editing tools for files in
the current project.  This effectively makes the project writable
by the agent without per-call confirmation."
  :type 'boolean
  :group 'eai-code-confirm)

(defvar eai-code-confirm-session-tools nil
  "List of tool names auto-allowed for the current Emacs session.
Populated interactively via `eai-code-confirm-allow-session'.")

(defvar eai-code-confirm-project-tools nil
  "List of tool names auto-allowed for the current project.
Persisted across sessions in `eai-code-confirm--project-file'.")

(defvar eai-code-confirm--project-file nil
  "File path where project-level tool allow-list is stored.
Computed from the current project root.")

(defconst eai-code-confirm--file-edit-tools
  '("write-file" "save-buffer"
    "replace-region" "remove-region" "insert-at" "replace-text"
    "outline-replace-section" "outline-insert-before" "outline-insert-after"
    "org-replace-subtree" "org-archive-subtree" "org-todo"
    "org-set-property"
    "smerge-replace-defun-region")
  "Tool names that modify files.
When `eai-code-confirm-project-read-write' is non-nil, these are
auto-allowed for files within the current project.")

(defconst eai-code-confirm--shell-tools
  '("run-shell")
  "Tool names that execute shell commands.")

(defconst eai-code-confirm--eval-tools
  '("eval")
  "Tool names that evaluate Elisp code.")

;;; Project persistence

(defun eai-code-confirm--project-file (&optional project)
  "Return the confirmation settings file for PROJECT.
Defaults to the current project."
  (let* ((proj (or project (project-current)))
         (root (and proj (project-root proj))))
    (when root
      (expand-file-name ".eai-code-confirm" root))))

(defun eai-code-confirm--load-project-tools ()
  "Load `eai-code-confirm-project-tools' from the project file."
  (setq eai-code-confirm-project-tools nil)
  (when-let* ((file (eai-code-confirm--project-file))
              ((file-exists-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (setq eai-code-confirm-project-tools (read (current-buffer))))
      (error nil))))

(defun eai-code-confirm--save-project-tools ()
  "Save `eai-code-confirm-project-tools' to the project file."
  (when-let* ((file (eai-code-confirm--project-file)))
    (with-temp-file file
      (prin1 eai-code-confirm-project-tools (current-buffer))
      (insert "\n"))))

;;; Core predicates

(defun eai-code-confirm--tool-name (call)
  "Extract the tool name from a CALL entry.
CALL is a list (TOOL-SPEC ARG-PLIST PROCESS-RESULT)."
  (gptel-tool-name (car call)))

(defun eai-code-confirm--auto-allow-p (tool-name args)
  "Return non-nil if a call to TOOL-NAME with ARGS needs no confirmation."
  (or (member tool-name eai-code-confirm-session-tools)
      (member tool-name eai-code-confirm-project-tools)
      ;; Project read-write auto-allows file edits in project files
      (and eai-code-confirm-project-read-write
           (member tool-name eai-code-confirm--file-edit-tools)
           (eai-code-confirm--in-project-file-p args))
      ;; Always confirm if none of the above matched
      nil))

(defun eai-code-confirm--in-project-file-p (args)
  "Check whether ARGS reference a file inside the current project.
ARGS is the plist of arguments for a tool call."
  (when-let* ((proj (project-current))
              (root (project-root proj))
              (file (or (plist-get args :filename)
                        (plist-get args :file)
                        (plist-get args :path)
                        (plist-get args :buffer)
                        (car-safe args)))
              (file-str (and file (format "%s" file)))
              (expanded (expand-file-name file-str root)))
    (string-prefix-p root expanded)))

;;; Direct execution

(defun eai-code-confirm--execute-call (call)
  "Execute a single tool CALL directly, bypassing confirmation.
CALL is (TOOL-SPEC ARG-PLIST PROCESS-TOOL-RESULT)."
  (let* ((tool-spec (car call))
         (arg-plist (cadr call))
         (process-tool-result (caddr call))
         (arg-values (gptel--map-tool-args tool-spec arg-plist)))
    (if (gptel-tool-async tool-spec)
        (apply (gptel-tool-function tool-spec) process-tool-result arg-values)
      (let ((result (condition-case errdata
                        (apply (gptel-tool-function tool-spec) arg-values)
                      (error (mapconcat #'gptel--to-string errdata " ")))))
        (funcall process-tool-result result)))))

;;; Alert integration

(defun eai-code-confirm--alert (calls buf)
  "Send an alert notification for CALLS in buffer BUF."
  (when eai-code-confirm-alert
    (let ((names (mapconcat (lambda (c) (eai-code-confirm--tool-name c)) calls ", ")))
      (if (fboundp 'alert)
          (alert (format "Tool calls need confirmation: %s" names)
                 :title "eai-code"
                 :buffer buf
                 :severity 'normal
                 :category 'eai-code)
        (message "eai-code: tool calls need confirmation: %s" names)))))

;;; Main entry point

(defun eai-code-confirm-display (calls info)
  "Handle tool call confirmation for CALLS with FSM INFO.
Auto-allows tools based on session/project settings.  Remaining
calls are shown inline in the chat buffer (or minibuffer if
`eai-code-confirm-style' is `minibuffer').  Sends an alert if the
buffer is hidden."
  (let ((auto-allowed nil)
        (need-confirm nil))
    ;; Separate auto-allowed from needs-confirm
    (pcase-dolist (`(,tool-spec ,arg-plist ,process-result) calls)
      (let ((tool-name (gptel-tool-name tool-spec))
            (args arg-plist))
        (if (eai-code-confirm--auto-allow-p tool-name args)
            (push (list tool-spec arg-plist process-result) auto-allowed)
          (push (list tool-spec arg-plist process-result) need-confirm))))
    ;; Execute auto-allowed calls immediately
    (dolist (call (nreverse auto-allowed))
      (eai-code-confirm--execute-call call))
    ;; Handle remaining calls that need confirmation
    (when need-confirm
      (let* ((buf (plist-get info :buffer))
             (visible (and buf (get-buffer-window buf 'visible)))
             (use-minibuffer (eq eai-code-confirm-style 'minibuffer)))
        (unless visible
          (eai-code-confirm--alert (nreverse need-confirm) buf))
        (if (eq eai-code-confirm-style 'popup)
            ;; Fallback: popup is not yet implemented, use minibuffer
            (gptel--display-tool-calls (nreverse need-confirm) info t)
          ;; Inline (default) or minibuffer
          (gptel--display-tool-calls (nreverse need-confirm) info use-minibuffer))))))

;;; Inline overlay helpers

(defun eai-code-confirm--overlay-calls ()
  "Return the pending tool calls at point.
Extracts from the `gptel-tool' text property."
  (let ((prop (get-char-property-and-overlay (point) 'gptel-tool)))
    (car-safe prop)))

(defun eai-code-confirm--overlay-tool-names ()
  "Return the list of tool names from the overlay at point."
  (let ((calls (eai-code-confirm--overlay-calls)))
    (when calls
      (delete-dups (mapcar (lambda (c) (eai-code-confirm--tool-name c)) calls)))))

;;;###autoload
(defun eai-code-confirm-allow-session-at-point ()
  "Allow the tool under point for the current session.
Intended as a keybinding in the tool confirmation overlay."
  (interactive)
  (if-let* ((names (eai-code-confirm--overlay-tool-names)))
      (if (= (length names) 1)
          (progn
            (eai-code-confirm-allow-session (car names))
            (call-interactively #'gptel--accept-tool-calls))
        (message "Multiple tools: %s" (mapconcat #'identity names ", ")))
    (message "No tool call at point")))

;;;###autoload
(defun eai-code-confirm-allow-project-at-point ()
  "Allow the tool under point for the current project.
Intended as a keybinding in the tool confirmation overlay."
  (interactive)
  (if-let* ((names (eai-code-confirm--overlay-tool-names)))
      (if (= (length names) 1)
          (progn
            (eai-code-confirm-allow-project (car names))
            (call-interactively #'gptel--accept-tool-calls))
        (message "Multiple tools: %s" (mapconcat #'identity names ", ")))
    (message "No tool call at point")))

;;; Interactive commands

;;;###autoload
(defun eai-code-confirm-allow-session (tool-name)
  "Add TOOL-NAME to the session allow list.
Future calls to this tool in the current Emacs session will not
require confirmation."
  (interactive (list (completing-read
                      "Allow tool for this session: "
                      (mapcar (lambda (t_)
                                (cons (gptel-tool-name t_) (gptel-tool-name t_)))
                              (gptel-get-tools))
                      nil t)))
  (add-to-list 'eai-code-confirm-session-tools tool-name)
  (message "Allowed `%s' for this session" tool-name))

;;;###autoload
(defun eai-code-confirm-allow-project (tool-name)
  "Add TOOL-NAME to the project allow list (persisted).
Future calls to this tool in the current project will not require
confirmation across Emacs sessions."
  (interactive (list (completing-read
                      "Allow tool for this project: "
                      (mapcar (lambda (t_)
                                (cons (gptel-tool-name t_) (gptel-tool-name t_)))
                              (gptel-get-tools))
                      nil t)))
  (add-to-list 'eai-code-confirm-project-tools tool-name)
  (eai-code-confirm--save-project-tools)
  (message "Allowed `%s' for this project" tool-name))

;;;###autoload
(defun eai-code-confirm-allow-group (group)
  "Allow all tools in GROUP for the current session.
GROUP is one of: file-edit, shell, eval, read."
  (interactive (list (completing-read
                      "Allow group for this session: "
                      '("file-edit" "shell" "eval" "read")
                      nil t)))
  (let ((tools (pcase group
                 ("file-edit" eai-code-confirm--file-edit-tools)
                 ("shell" eai-code-confirm--shell-tools)
                 ("eval" eai-code-confirm--eval-tools)
                 ("read" '("read-file-contents" "read-buffer-contents"
                           "read-buffer-region" "read-buffer-contents-since-last-read"
                           "read-lines" "outline-read-section"))
                 (_ nil))))
    (dolist (tool tools)
      (add-to-list 'eai-code-confirm-session-tools tool))
    (message "Allowed %s group (%d tools) for this session" group (length tools))))

;;;###autoload
(defun eai-code-confirm-toggle-project-read-write ()
  "Toggle `eai-code-confirm-project-read-write'.
When enabled, all file-editing tools are auto-allowed for files
within the current project."
  (interactive)
  (setq eai-code-confirm-project-read-write
        (not eai-code-confirm-project-read-write))
  (message "Project read-write mode: %s"
           (if eai-code-confirm-project-read-write "ON" "OFF")))

;;; Enable / Disable

;;;###autoload
(defun eai-code-confirm-enable ()
  "Enable eai-code confirmation handling."
  (interactive)
  (eai-code-confirm--load-project-tools)
  (message "eai-code confirmation handling enabled"))

;;;###autoload
(defun eai-code-confirm-disable ()
  "Disable eai-code confirmation handling."
  (interactive)
  (message "eai-code confirmation handling disabled"))

;;; Keymap additions
;; Add eai-code-confirm commands to gptel's tool call actions map.
;; These are available in any gptel buffer but gracefully degrade
;; when not in an eai-code context.

(with-eval-after-load 'gptel
  (define-key gptel-tool-call-actions-map (kbd "C-c C-a")
              #'eai-code-confirm-allow-session-at-point)
  (define-key gptel-tool-call-actions-map (kbd "C-c C-p")
              #'eai-code-confirm-allow-project-at-point))

(provide 'eai-code-confirm)
;;; eai-code-confirm.el ends here
