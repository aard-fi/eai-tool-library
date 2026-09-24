;;; eai-code-monitor.el --- Live monitor and external help bridge -*- lexical-binding: t; -*-
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
;; Provides a live monitor buffer and a bridge for "pair programming" with
;; external help (e.g., Claude Code or another LLM agent).
;;
;; Two mechanisms:
;;
;; 1. *eai-code monitor* buffer showing current status, recent turns, pending
;;    tools, and last error. Updated via hooks on gptel send/response/error.
;;
;; 2. Instruction injection: before each gptel-send, check for
;;    `eai-code-instruction.org' in the project root. If present and newer
;;    than last consumed, inject its contents as the next user prompt.
;;    External agents can drop instructions there to help the Emacs agent
;;    when it is stuck.
;;
;; 3. `eai-code-export-state' dumps full state to a temp file and opens a
;;    buffer with guidance for the external helper.
;;
;;; Code:

(require 'cl-lib)
(require 'project)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

(defgroup eai-code-monitor nil
  "Live monitor and external help bridge for eai-code."
  :group 'eai-code)

(defcustom eai-code-monitor-instruction-file "eai-code-instruction.org"
  "File name (relative to project root) for injected instructions.
External agents write here; the Emacs agent consumes them at the start of
each turn."
  :type 'string
  :group 'eai-code-monitor)

(defvar-local eai-code-monitor--last-instruction-mtime nil
  "Modification time of the last consumed instruction file.")

(defvar-local eai-code-monitor--current-status "idle"
  "Current status string for the monitor (e.g. `idle', `waiting', `error').")

(defvar-local eai-code-monitor--current-action nil
  "Description of what the agent is currently doing.")

(defvar-local eai-code-monitor--last-error nil
  "Last error message encountered.")

(defvar-local eai-code-monitor--turn-start nil
  "Timestamp when the current turn started.")

(defvar-local eai-code-monitor--recent-turns nil
  "List of recent turn descriptions, newest first.")

(defvar eai-code-monitor--max-recent-turns 10
  "Maximum number of recent turns to keep in the monitor.")

;;; Monitor buffer

(defun eai-code-monitor--buffer ()
  "Return or create the monitor buffer."
  (get-buffer-create "*eai-code monitor*"))

(defun eai-code-monitor--refresh ()
  "Refresh the monitor buffer with current state."
  (let ((buf (eai-code-monitor--buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "# eai-code Monitor\n\n")

      ;; Status
      (insert "## Status\n")
      (insert (format "State:    %s\n" eai-code-monitor--current-status))
      (insert (format "Action:   %s\n" (or eai-code-monitor--current-action "—")))
      (insert (format "Duration: %s\n"
                      (if eai-code-monitor--turn-start
                          (format "%.1fs" (- (float-time) eai-code-monitor--turn-start))
                        "—")))
      (when (and (boundp 'eai-code--tool-turn-count)
                 (> eai-code--tool-turn-count 0))
        (insert (format "Turns:    %d\n" eai-code--tool-turn-count)))
      (insert (format "Profile:  %s\n" (or (bound-and-true-p eai-code--current-profile) "—")))
      (when eai-code-monitor--last-error
        (insert (format "Last error: %s\n" eai-code-monitor--last-error)))
      (insert "\n")

      ;; Recent turns
      (insert "## Recent Turns\n")
      (if (null eai-code-monitor--recent-turns)
          (insert "No turns yet.\n")
        (cl-loop for turn in (cl-subseq eai-code-monitor--recent-turns 0
                                        (min (length eai-code-monitor--recent-turns)
                                             eai-code-monitor--max-recent-turns))
                 do (insert (format "- %s\n" turn))))
      (insert "\n")

      ;; Pending instructions
      (let ((inst-file (eai-code-monitor--instruction-file-path)))
        (insert "## External Help\n")
        (if (and inst-file (file-exists-p inst-file))
            (insert (format "Pending instruction file: %s\n" inst-file))
          (insert "No pending instructions.\n")))

      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun eai-code-monitor--instruction-file-path ()
  "Return the absolute path to the instruction file, or nil if no project."
  (when-let* ((proj (project-current))
              (root (project-root proj)))
    (expand-file-name eai-code-monitor-instruction-file root)))

;;; Hooks

(defun eai-code-monitor--pre-send (&rest _)
  "Hook before `gptel-send'.  Check for pending instructions and update state."
  (setq eai-code-monitor--current-status "sending")
  (setq eai-code-monitor--turn-start (float-time))
  (setq eai-code-monitor--last-error nil)
  (eai-code-monitor--refresh)
  (eai-code-monitor--inject-instructions))

(defun eai-code-monitor--post-response (beg end)
  "Hook on `gptel-post-response-functions'.  BEG and END are response bounds."
  (setq eai-code-monitor--current-status "idle")
  (push (format "Response received (%d chars)" (- end beg))
        eai-code-monitor--recent-turns)
  (eai-code-monitor--refresh))

(defun eai-code-monitor--on-error (fsm)
  "Hook after `gptel--handle-error'.  FSM is the finite state machine."
  (when-let* ((info (gptel-fsm-info fsm))
              (err (plist-get info :error)))
    (setq eai-code-monitor--current-status "error")
    (setq eai-code-monitor--last-error
          (cond ((stringp err) err)
                ((plistp err) (gptel--to-string (or (plist-get err :message)
                                                    (plist-get err :type)
                                                    err)))
                (t (gptel--to-string err))))
    (push (format "Error: %s" eai-code-monitor--last-error)
          eai-code-monitor--recent-turns)
    (eai-code-monitor--refresh)))

;;; Instruction injection

(defun eai-code-monitor--inject-instructions ()
  "If an instruction file exists and is un-consumed, insert it before point.
Marks the file as consumed by recording its mtime.  The file is NOT deleted
so the external helper can see what was sent; users should delete it manually."
  (when-let* ((path (eai-code-monitor--instruction-file-path))
              ((file-exists-p path))
              (attrs (file-attributes path))
              (mtime (nth 5 attrs)))
    (when (or (null eai-code-monitor--last-instruction-mtime)
              (time-less-p eai-code-monitor--last-instruction-mtime mtime))
      (let ((text (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
        (unless (string-blank-p text)
          (goto-char (point-max))
          (insert "\n\n* External instruction:\n" text "\n")
          (setq eai-code-monitor--last-instruction-mtime mtime)
          (message "eai-code: injected external instructions from %s" path))))))

;;; User commands

;;;###autoload
(defun eai-code-monitor ()
  "Open or refresh the eai-code monitor buffer."
  (interactive)
  (eai-code-monitor--refresh)
  (pop-to-buffer (eai-code-monitor--buffer)))

;;;###autoload
(defun eai-code-export-state ()
  "Dump current state to a temp file and open a helper buffer.

The helper buffer contains instructions for an external agent (e.g. Claude Code)
on how to assist the Emacs agent."
  (interactive)
  (let* ((chat-buf (current-buffer))
         (chat-name (buffer-name chat-buf))
         (project (project-current))
         (project-root (and project (project-root project)))
         (temp-file (make-temp-file "eai-code-state-"))
         (state-file (make-temp-file "eai-code-state-buffer-")))

    ;; Write the chat buffer content to a temp file
    (with-current-buffer chat-buf
      (write-region (point-min) (point-max) temp-file nil 'silent))

    ;; Write monitor state to a separate file
    (with-temp-file state-file
      (insert "# eai-code Agent State Export\n\n")
      (insert (format "Project: %s\n" (or project-root "—")))
      (insert (format "Chat buffer: %s\n" chat-name))
      (insert (format "Status: %s\n" eai-code-monitor--current-status))
      (insert (format "Action: %s\n" (or eai-code-monitor--current-action "—")))
      (insert (format "Last error: %s\n" (or eai-code-monitor--last-error "—")))
      (insert (format "Duration: %s\n"
                      (if eai-code-monitor--turn-start
                          (format "%.1fs" (- (float-time) eai-code-monitor--turn-start))
                        "—")))
      (insert "\n## Recent Turns\n")
      (dolist (turn (cl-subseq eai-code-monitor--recent-turns 0
                               (min (length eai-code-monitor--recent-turns) 20)))
        (insert (format "- %s\n" turn)))
      (insert "\n## Metrics Summary\n")
      (when (boundp 'eai-code-metrics--log)
        (insert (format "Total requests: %d\n" (length eai-code-metrics--log)))
        (let ((successes (cl-count-if (lambda (e) (plist-get e :success))
                                      eai-code-metrics--log)))
          (insert (format "Successes: %d, Failures: %d\n" successes (- (length eai-code-metrics--log) successes)))))
      (insert "\n## Chat Content\n")
      (insert "See: " temp-file "\n"))

    ;; Open the helper buffer
    (let ((help-buf (get-buffer-create "*eai-code external help*")))
      (with-current-buffer help-buf
        (erase-buffer)
        (insert "# eai-code External Help\n\n")
        (insert "The Emacs agent is stuck or could use assistance.\n\n")
        (insert "## How to help\n\n")
        (insert "1. Read the state file: " state-file "\n")
        (insert "2. Read the chat buffer: " temp-file "\n")
        (insert "3. Decide what's needed: fix, instruction, or tool improvement.\n")
        (insert "4. Write your instructions to:\n")
        (insert "   " (or (eai-code-monitor--instruction-file-path) "— no project —") "\n")
        (insert "\n## Instruction format\n\n")
        (insert "Write plain text or org-mode in the instruction file.\n")
        (insert "The agent will see it as the next user prompt.\n")
        (insert "Keep it focused — the agent reads it inline.\n")
        (insert "\n## Example\n\n")
        (insert "#+begin_example\n")
        (insert "The outline-replace-section tool requires an exact section name.\n")
        (insert "Use buffer-outline first to find the exact name, then replace.\n")
        (insert "#+end_example\n")
        (insert "\n## Important\n\n")
        (insert "- The instruction file is read ONCE per turn.\n")
        (insert "- The agent will NOT see your instruction until its next send.\n")
        (insert "- If the agent is mid-response, wait for it to finish or abort (C-g).\n"))
      (pop-to-buffer help-buf))))

;;;###autoload
(defun eai-code-monitor-enable ()
  "Enable the monitor hooks."
  (interactive)
  (advice-add 'gptel-send :before #'eai-code-monitor--pre-send)
  (add-hook 'gptel-post-response-functions #'eai-code-monitor--post-response)
  (advice-add 'gptel--handle-error :after #'eai-code-monitor--on-error)
  (message "eai-code monitor enabled"))

;;;###autoload
(defun eai-code-monitor-disable ()
  "Disable the monitor hooks."
  (interactive)
  (advice-remove 'gptel-send #'eai-code-monitor--pre-send)
  (remove-hook 'gptel-post-response-functions #'eai-code-monitor--post-response)
  (advice-remove 'gptel--handle-error #'eai-code-monitor--on-error)
  (message "eai-code monitor disabled"))

(provide 'eai-code-monitor)
;;; eai-code-monitor.el ends here
