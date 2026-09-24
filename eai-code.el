;;; eai-code.el ---  -*- lexical-binding: t; -*-
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
;; Your friendly coding agent
;;
;;; Code:

(require 's)
(require 'vc)
(require 'eai-code-metrics)
(require 'eai-code-monitor)
(require 'eai-code-error)
(require 'eai-code-compact)
(require 'eai-code-agent)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

;; gptel-org is loaded conditionally at runtime; declare its functions
;; to suppress byte-compiler warnings.
(declare-function gptel-org--annotate-links "gptel-org" (beg end))
(declare-function gptel-org--save-state "gptel-org")
(declare-function gptel-org--restore-state "gptel-org")
(declare-function gptel-org-set-properties "gptel-org" (pt &optional msg))
(declare-function gptel--parse-buffer "gptel-request" (backend &optional max-entries))

(defcustom eai-code-directive
  "You are a large language model living in Emacs, helping with development.

You're working on one project at a time. The current project is ${project-name} in ${project-root}. You have tools available to help you operate with projects. You have dedicated tools for file operations, project exploration, code search, buffer editing, and more. Always prefer these tools over shell commands. Only use `run-shell' when no specific tool exists for the task. You doo not echo large code segments to the user - we're working inside the codebase, all code is there. You may display small snippets if required for context, and otherwise provide references/links into code.

You rely on the user for solving your mistakes, like when inserting data causes corruption. When you detect such issues you directly STOP and inform the user about the problem. Do not try to fix it yourself.

You can store memories just for yourself in ${memory-file}. You should read that at the start of a new session, and keep it updated with relevant information. Make sure to keep it as compact as possible, though, so we can work with limited context.

The project planning is happening in ${planning-file}.

You are empowered to improve your own tools and configuration. If you notice patterns where you could work more effectively (e.g., a backend is consistently slow, a tool description is unclear, a new composite tool would save context), you may propose changes to:
- Agent profiles in eai-code-agent.el
- The system prompt or directives
- Tool descriptions or implementations
- Error handling behavior

For any code change, first verify the file compiles (`M-x emacs-lisp-byte-compile` or `M-x check-parens`), then test the change. Destructive changes require explicit user confirmation.

When multiple independent tools can be called at once, batch them in a single tool-call block rather than calling them sequentially. This reduces latency and improves user experience.

"
  "The template for the default directive to use in eai-code chat buffers."
  :type 'string
  :group 'eai-code)

(defcustom eai-code-file-map
  '((memory   . "memory.org")
    (chat     . "chat.org")
    (planning . "planning.org"))
  "The org files, relative to the project directory."
  :type '(alist :key-type symbol :value-type string)
  :group 'eai-code)

(defcustom eai-code-persist-chat nil
  "Configure chat session persistence.

When set to `t' eai-code will create the chat file specified in
`eai-code-file-map', othewise it will use a buffer without file
backing"
  :type 'boolean
  :group 'eai-code)

(defcustom eai-code-default-memory
  "* LLM memory file

This file contains your memory for this project. This file is fully under your control - you can do whatever you want with it."
  "The default content of new memory files"
  :type 'string
  :group 'eai-code)

(defcustom eai-code-default-planning
  "* Planning for ${project-name}

A blank canvas so far.
"
  "The default content of new memory files"
  :type 'string
  :group 'eai-code)

(defcustom eai-code-default-backend ""
  "The name of the default backend to use for code sessions"
  :type 'string
  :group 'eai-code)

(defcustom eai-code-default-model nil
  "The name of the default model for code sessions"
  :type 'symbol
  :group 'eai-code)

(defcustom eai-code-strip-reasoning t
  "When non-nil, strip #+begin_reasoning blocks from responses."
  :type 'boolean
  :group 'eai-code)

(defcustom eai-code-log-reasoning nil
  "When non-nil, log stripped reasoning to *eai-code reasoning* buffer."
  :type 'boolean
  :group 'eai-code)

(defcustom eai-code-debug nil
  "When non-nil, log debug info to *eai-code debug* buffer."
  :type 'boolean
  :group 'eai-code)

(defface eai-code-user-prompt
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the @developer: user prompt prefix."
  :group 'eai-code)

(defface eai-code-response-prompt
  '((t :inherit font-lock-type-face :weight bold))
  "Face for the @code monkey: response prefix."
  :group 'eai-code)

(defvar-local eai-code--request-active nil
  "Non-nil when a request is in flight in the current buffer.")

(defvar-local eai-code--activity-type nil
  "Symbol describing the current activity type for the header emoji.
Values: reasoning, tool-call, tool-result, text, error.")

(defvar-local eai-code--activity-emoji nil
  "Currently chosen emoji for the header line.
Randomly selected from a pool each time the activity type changes.")

(defvar-local eai-code--stream-accumulator nil
  "Accumulated response text during streaming.")

(defvar-local eai-code--stream-reasoning nil
  "Accumulated reasoning text during streaming.")

(defvar-local eai-code--tool-pending nil
  "Non-nil when the LLM has requested tool calls and we're waiting.")

(defvar-local eai-code--sub-agents nil
  "Alist of active sub-agents in the current buffer.
Each element is (ID . PLIST) with keys :description, :profile,
:backend, :model, :status, :start-time, :end-time, :result.")

(defvar-local eai-code--boundary-pos nil
  "Integer position where the current response should be inserted.
Set to `(point-max)' in `eai-code-send' after the user's prompt is
extracted.  Unlike a marker, this integer does NOT advance when the
user types during an in-flight request or when filler is inserted and
removed.  This keeps it stable as the correct insertion point across
multi-turn tool-call conversations.")

(defvar-local eai-code--tool-turn-count 0
  "Number of tool-call/tool-result cycles in the current request.")

(defvar-local eai-code--request-start-time nil
  "Timestamp when the current request started.")

(defvar-local eai-code--filler-marker nil
  "Marker for temporary filler text inserted during tool calls.
Removed when the next response chunk arrives.")

(defun eai-code--aget (key data)
  "Safe aget for s-format.

For missing keys return the key name in s-format syntax."
  (or (alist-get key data nil nil #'string=)
      (format "${%s}" key)))

(defun eai-code--directive ()
  "Dynamically build the system prompt for the code session.

This includes details about the project, but should also give us options about
dynamically adding in information to improve results later on."
  (let ((project (project-current)))
    (s-format eai-code-directive 'eai-code--aget
              `(("project-name" . ,(project-name project))
                ("project-root" . ,(project-root project))
                ("memory-file" . ,(eai-code--get-special-file 'memory))
                ("planning-file" . ,(eai-code--get-special-file 'planning))
                ))))

(defun eai-code--get-special-file (key)
  "Return the absolute path for KEY based on `eai-code-file-map'."
  (let* ((project (project-current t))
         (root (project-root project))
         (filename (alist-get key eai-code-file-map)))
    (if filename
        (expand-file-name filename root)
      (error "No filename mapped for key: %s" key))))

(defun eai-code--ensure-directory (dir)
  "Ensure DIR exists. Create it if missing, or abort with `user-error'."
  (interactive "DTarget directory: ")
  (unless (file-directory-p dir)
    (let ((choice (completing-read
                   (format "Directory %s does not exist. Choose: " dir)
                   '("create directory" "abort") nil t)))
      (cond
       ((equal choice "create directory")
        (make-directory dir t)
        (message "Created directory: %s" dir))
       ((equal choice "abort")
        (user-error "Directory creation aborted by user")))))
  dir)

;; this selection is a bit annoying - maybe use transient instead?
(defun eai-code--find-or-create-project (&optional dir)
  "Ensure `default-directory' is in a project. If `dir' is given, switch to it.

If not in a project, offer to cd elsewhere or init a Git repo. Aborts on
user cancel."
  (interactive)
  (let ((dir (or dir default-directory)))
    (let ((project (project-current nil dir)))
      (if project
          (let ((project-root (project-root project)))
            (message "✓ In project: %s" project-root)
            (setq default-directory project-root))
        (let* ((choice (completing-read
                        "Not in a project. Choose: "
                        '("change directory"
                          "initialize git repo here"
                          "abort") nil t))
               (_cmd (lambda (fn)
                       (funcall fn)            ; ensure function executes and exits
                       (eai-code--find-or-create-project)))) ; tail-call again
          (cond
           ((equal choice "change directory")
            (let ((new-dir (read-directory-name "Choose project directory: ")))
              (unless (file-directory-p new-dir)
                (eai-code--ensure-directory new-dir))
              (eai-code--find-or-create-project new-dir)))
           ((equal choice "initialize git repo here")
            (setq default-directory dir)
            (unless (file-directory-p dir)
              (eai-code--ensure-directory dir))
            (vc-create-repo 'Git)
            (message "Initialized Git repo in: %s" default-directory)
            (eai-code--find-or-create-project))
           ((equal choice "abort")
            (user-error "User aborted project setup"))))))))

(defun eai-code--setup-memory ()
  "Locate the memory file, and create it, if it is empty"
  (interactive)
  (let* ((project (project-current))
         (_project-root (project-root project))
         (memory-file (eai-code--get-special-file 'memory)))
    (with-current-buffer (find-file-noselect memory-file)
      (when (= (buffer-size) 0)
        (insert eai-code-default-memory)
        (save-buffer))
      (kill-buffer))))

(defun eai-code--setup-plan ()
  "Locate the planning file, and create it, if it is empty"
  (interactive)
  (let* ((project (project-current))
         (_project-root (project-root project))
         (planning-file (eai-code--get-special-file 'planning)))
    (with-current-buffer (find-file-noselect planning-file)
      (when (= (buffer-size) 0)
        (let ((insert
               (s-format eai-code-default-planning 'eai-code--aget
                         `(("project-name" . ,(project-name project))
                           ("project-root" . ,(project-root project))
                           ))))
          (insert insert)
          (save-buffer)))
      (kill-buffer))))

;;; Reasoning stripping

(defun eai-code--response-text (response)
  "Normalize RESPONSE to a plain string.
Handles gptel's (reasoning . text) cons cell by extracting text.
Returns the text string, or the empty string for nil."
  (cond
   ((null response) "")
   ((stringp response) response)
   ((and (consp response) (eq (car response) 'reasoning))
    (cdr response))
   ((listp response) (format "%S" response))
   (t (format "%s" response))))

(defun eai-code--strip-reasoning (text)
  "Strip #+begin_reasoning ... #+end_reasoning blocks from TEXT.
Also removes comma-prefixed lines that some models emit as
reasoning artifacts.  Returns the cleaned text or TEXT unchanged
if it is not a string."
  (if (stringp text)
      (let ((clean (replace-regexp-in-string
                    "#\\+begin_reasoning\n\\(?:.*\n\\)*?#\\+end_reasoning\n?"
                    ""
                    text)))
        ;; Some models prefix every reasoning line with a comma
        (setq clean (replace-regexp-in-string
                     "^,\\(?:[^\n]*\\)\n" "" clean))
        (string-trim clean))
    text))

(defun eai-code--maybe-log-reasoning (raw-text)
  "If `eai-code-log-reasoning' is non-nil, log RAW-TEXT to side buffer."
  (when eai-code-log-reasoning
    (let ((buf (get-buffer-create "*eai-code reasoning*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "\n--- " (format-time-string "%Y-%m-%d %H:%M:%S") " ---\n")
        (insert (if (stringp raw-text) raw-text (format "%S" raw-text)) "\n")))))

(defun eai-code--post-response-strip-reasoning (beg end)
  "Hook on `gptel-post-response-functions'.
Strip reasoning blocks from the response between BEG and END.
If `eai-code-log-reasoning' is non-nil, save the raw reasoning
first.  Must run before other hooks that may move point."
  (when eai-code-strip-reasoning
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "#\\+begin_reasoning\n\\(?:.*\n\\)*?#\\+end_reasoning\n?"
              end t)
        (let ((rb (match-beginning 0))
              (re (match-end 0)))
          (eai-code--maybe-log-reasoning
           (buffer-substring-no-properties rb re))
          (let ((inhibit-read-only t))
            (delete-region rb re))
          (setq end (- end (- re rb))))))))

;;; Header line

(defun eai-code--random-emoji (type)
  "Pick a random emoji string for activity TYPE."
  (let ((pool (pcase type
                ('reasoning '("🧠" "💭" "🤔" "🧬" "🔮"))
                ('tool-call '("🔨" "🔩" "🛠️" "🔧" "⚒️"))
                ('tool-result '("🟢" "⚙️" "💎" "✅" "✔️"))
                ('text       '("📃" "📝" "📜" "📋" "📄"))
                ('error      '("🚫" "🔴" "⛔" "📛" "🥀"))
                (_           '("🕐" "🕒" "🕔" "🕖" "🕘")))))
    (nth (random (length pool)) pool)))

(defun eai-code--status-string ()
  "Build the status string for the header line."
  (let* ((backend (when (boundp 'gptel-backend)
                    (ignore-errors (gptel-backend-name gptel-backend))))
         (model (when (boundp 'gptel-model) gptel-model))
         (profile (or (bound-and-true-p eai-code--current-profile) 'default))
         (elapsed (when (and eai-code--request-active eai-code--request-start-time)
                    (let ((sec (- (float-time) eai-code--request-start-time)))
                      (if (< sec 60)
                          (format " %.0fs" sec)
                        (format " %.0fm" (/ sec 60))))))
         (turns (when (and eai-code--request-active (> eai-code--tool-turn-count 0))
                  (format " T%d" eai-code--tool-turn-count)))
         (active (if eai-code--request-active
                     (concat (or eai-code--activity-emoji "⏳") " "
                             (pcase eai-code--activity-type
                               ('reasoning "thinking")
                               ('tool-call "tools")
                               ('tool-result "waiting")
                               ('error "error")
                               (_ "sending"))
                             (or elapsed "")
                             (or turns ""))
                   "✅ idle"))
         (sub-count (cl-count-if
                     (lambda (a) (eq (plist-get (cdr a) :status) 'running))
                     eai-code--sub-agents))
         (sub (when (> sub-count 0)
                (format " | Subs: %d" sub-count))))
    (format "Status: %s | Backend: %s | Model: %s | Profile: %s%s"
            active
            (or backend "—")
            (or (and model (symbol-name model)) "—")
            (symbol-name profile)
            (or sub ""))))

(defun eai-code--header-line ()
  "Return the `header-line-format' value for eai-code chat buffers."
  (let* ((sub-count (length eai-code--sub-agents))
         (sub-btn (when (> sub-count 0)
                    (concat " "
                            (propertize
                             (buttonize (format "[Agents: %d]" sub-count)
                                        (lambda (&rest _) (eai-code-agents)))
                             'mouse-face 'highlight
                             'help-echo "Click to view sub-agent activity"))))
         (rhs (concat
               (propertize (buttonize (concat "[" (eai-code--status-string) "]")
                                      (lambda (&rest _) (eai-code-status)))
                           'mouse-face 'highlight
                           'help-echo "Click to see status details")
               "  "
               (propertize
                (buttonize "[Send]"
                           (lambda (&rest _) (eai-code-send)))
                'mouse-face 'highlight
                'help-echo "C-c C-c")
               " "
               (propertize
                (buttonize "[Abort]"
                           (lambda (&rest _) (eai-code-abort)))
                'mouse-face 'highlight
                'help-echo "C-c C-k")
               (or sub-btn ""))))
    (concat
     (propertize
      " " 'display
      `(space :align-to (- right ,(+ 3 (string-width rhs)))))
     rhs)))

;;;###autoload
(defun eai-code-status ()
  "Show detailed status in the minibuffer."
  (interactive)
  (message "%s" (eai-code--status-string)))

(defun eai-code--update-header-line ()
  "Refresh the header line in the current buffer."
  (setq header-line-format '(:eval (eai-code--header-line))))

(defun eai-code--setup-header-line ()
  "Set up the header line for an eai-code chat buffer."
  (setq header-line-format '(:eval (eai-code--header-line))))

;;; Agent activity buffer

(defun eai-code-agent--buffer ()
  "Return or create the *eai-code agents* buffer."
  (get-buffer-create "*eai-code agents*"))

(defun eai-code-agent--refresh ()
  "Refresh the *eai-code agents* buffer with current state.
Shows running and completed sub-agents with their metadata."
  (let ((buf (eai-code-agent--buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "# eai-code Sub-Agents\n\n")

      ;; Running agents
      (let ((running (cl-remove-if-not
                      (lambda (a) (eq (plist-get (cdr a) :status) 'running))
                      eai-code--sub-agents)))
        (insert "## Running\n")
        (if (null running)
            (insert "No active sub-agents.\n")
          (dolist (a running)
            (let* ((plist (cdr a))
                   (duration (- (float-time) (plist-get plist :start-time))))
              (insert (format "- [%s] %s | %s | %s | %.1fs\n"
                              (car a)
                              (plist-get plist :profile)
                              (plist-get plist :backend)
                              (plist-get plist :description)
                              duration)))))
        (insert "\n"))

      ;; Completed agents
      (let ((done (cl-remove-if-not
                   (lambda (a) (eq (plist-get (cdr a) :status) 'done))
                   eai-code--sub-agents)))
        (insert "## Completed\n")
        (if (null done)
            (insert "No completed sub-agents in this session.\n")
          (dolist (a (cl-subseq done 0 (min 20 (length done))))
            (let* ((plist (cdr a))
                   (duration (when (plist-get plist :end-time)
                               (- (plist-get plist :end-time)
                                  (plist-get plist :start-time)))))
              (insert (format "- [%s] %s | %s | %s | %s"
                              (car a)
                              (plist-get plist :profile)
                              (plist-get plist :backend)
                              (plist-get plist :description)
                              (if duration (format "%.1fs" duration) "?")))
              (insert "\n"))))
        (insert "\n"))

      ;; Errors
      (let ((errors (cl-remove-if-not
                     (lambda (a) (eq (plist-get (cdr a) :status) 'error))
                     eai-code--sub-agents)))
        (when errors
          (insert "## Errors\n")
          (dolist (a errors)
            (let ((plist (cdr a)))
              (insert (format "- [%s] %s | %s | Error: %s\n"
                              (car a)
                              (plist-get plist :profile)
                              (plist-get plist :description)
                              (or (plist-get plist :result) "unknown")))))
          (insert "\n")))

      (setq buffer-read-only t)
      (goto-char (point-min)))))

;;;###autoload
(defun eai-code-agents ()
  "Open the *eai-code agents* buffer showing sub-agent activity."
  (interactive)
  (eai-code-agent--refresh)
  (pop-to-buffer (eai-code-agent--buffer)))

;;; Agent-based send

(defun eai-code--extract-prompt (prefix)
  "Extract prompt text after the last PREFIX in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((pos (search-backward prefix nil t)))
      (if pos
          (string-trim (buffer-substring-no-properties
                        (+ pos (length prefix)) (point-max)))
        ""))))

(defun eai-code--build-conversation ()
  "Build conversation history from the current chat buffer.

Uses `gptel--parse-buffer' to extract all prior user prompts and
assistant responses, respecting the `gptel' text properties and
prefix alists.  Returns a list of strings alternating
user/assistant/user/... suitable for passing as the PROMPT
argument to `gptel-request'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((messages (gptel--parse-buffer gptel-backend)))
        (mapcar (lambda (msg) (plist-get msg :content)) messages)))))

(defun eai-code--insert-response (response prefix)
  "Insert RESPONSE with PREFIX into the current buffer.
Strips reasoning blocks before insertion and logs them if configured.
Uses `eai-code--boundary-pos' as the insertion point.  Unlike a marker,
this integer stays stable when filler is inserted/removed or when the
user types during an in-flight request.

When the user already typed a follow-up prompt after the boundary,
the response is inserted BEFORE their text and no duplicate
`@developer:' prompt is added.

Does nothing if RESPONSE is nil or yields only blank text after stripping."
  (let* ((text (eai-code--response-text response))
         (clean (eai-code--strip-reasoning text)))
    (when (and clean (not (string-blank-p clean)))
      (eai-code--maybe-log-reasoning response)
      (save-excursion
        (let* ((dev-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
               (response-prefix prefix)
               (insert-pos (or eai-code--boundary-pos (point-max)))
               (text-start-marker nil)
               (user-text nil))

          ;; Remove all filler blocks so they don't interfere.
          (eai-code--remove-filler)

          ;; Clamp insertion point to valid range.
          (setq insert-pos (max 1 (min insert-pos (point-max))))

          ;; Capture any user-typed text after the boundary BEFORE we
          ;; destroy it.  This includes their follow-up prompt plus any
          ;; auto-inserted dev prompt they never used.
          (setq user-text (when (< insert-pos (point-max))
                            (string-trim
                             (buffer-substring-no-properties insert-pos (point-max)))))

          ;; Delete EVERYTHING from the boundary to the end of the buffer.
          ;; This removes empty auto-inserted prompts, leaked filler, and
          ;; any other garbage in one shot.
          (when (< insert-pos (point-max))
            (delete-region insert-pos (point-max)))

          (goto-char insert-pos)

          ;; Ensure exactly one newline before the response prefix.
          (eai-code--ensure-newline-before)

          ;; Insert response prefix with face
          (insert (propertize (or response-prefix "")
                              'eai-code 'response
                              'face 'eai-code-response-prompt))

          ;; Convert markdown to org before inserting
          (let ((org-text (if (fboundp 'gptel--convert-markdown->org)
                              (gptel--convert-markdown->org clean)
                            clean)))
            ;; Remember where text starts for gptel tracking
            (setq text-start-marker (point-marker))
            ;; Insert the actual response text
            (insert org-text)
            ;; Tag response text with gptel property for tracking.
            ;; NOTE: Do NOT add `front-sticky' here — it causes
            ;; `org-src-font-lock-fontify-block' to error when fontifying
            ;; source blocks inside the response text.
            (add-text-properties text-start-marker (point)
                                 '(gptel response))
            (set-marker text-start-marker nil))

          ;; Ensure exactly one newline after response text
          (eai-code--ensure-newline-after)

          ;; Handle prompt insertion: if the user already typed a prompt
          ;; starting with @developer:, re-insert it.  Otherwise insert a
          ;; fresh one for the next turn.
          (if (and user-text
                   (string-prefix-p (string-trim dev-prefix) user-text))
              (progn
                (insert (propertize user-text 'eai-code 'prompt))
                (setq eai-code--boundary-pos (point-max)))
            (insert (propertize (or dev-prefix "")
                                'eai-code 'prompt
                                'face 'eai-code-user-prompt))
            (setq eai-code--boundary-pos (point-max)))

          (message "Response received."))))))

(defun eai-code--ensure-newline-before ()
  "Ensure there is exactly one newline immediately before point.
If there is no newline, inserts one and leaves point after it.
If there is more than one, deletes the extras and leaves point
after the single remaining one.  Does nothing at start of buffer."
  (let ((start (point)))
    (skip-chars-backward "\n")
    (cond
     ;; At start of buffer: just clean up whitespace
     ((= (point) (point-min))
      (goto-char start)
      (delete-horizontal-space))
     ;; No newline before point: insert one, leave point after it
     ((= start (point))
      (goto-char start)
      (delete-horizontal-space)
      (insert "\n"))
     ;; Multiple newlines: keep one, move point past it
     ((> (- start (point)) 1)
      (delete-region (+ (point) 1) start)
      (forward-char 1))
     ;; Exactly one newline: move point past it
     (t (forward-char 1)))))

(defun eai-code--ensure-newline-after ()
  "Ensure there is exactly one newline immediately after point.
If there is no newline, inserts one and leaves point after it.
If there is more than one, deletes the extras and leaves point
after the single remaining one."
  (let ((start (point)))
    (skip-chars-forward "\n")
    (cond
     ;; No newline after point: insert one, leave point after it
     ((= (point) start)
      (goto-char start)
      (insert "\n"))
     ;; Multiple newlines: keep one, move point past it
     ((> (- (point) start) 1)
      (delete-region (+ start 1) (point))
      (goto-char (+ start 1)))
     ;; Exactly one newline: move point past it
     (t (goto-char (+ start 1))))))

(defun eai-code--insert-filler (text)
  "Insert temporary filler TEXT at `(point-max)'.
Filler text provides status feedback during tool-call rounds.  It is
inserted at the end of the buffer so it is always visible.  Unlike the
old marker-based approach, this does not interfere with
`eai-code--boundary-pos'.

The start position is stored in `eai-code--filler-marker' for later
removal.  If you want to keep filler as a permanent record, don't call
`eai-code--remove-filler'."
  (eai-code--remove-filler)
  ;; Insert filler at point-max.  The user's in-flight text (if any) is
  ;; already in the buffer after boundary-pos, so filler goes after it.
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    ;; Mark the start of the block BEFORE inserting text.  Use `nil'
    ;; insertion type so the marker stays here for reliable removal.
    (setq eai-code--filler-marker (point-marker))
    (insert "#+begin_quote\n" text "\n#+end_quote\n")))

(defun eai-code--remove-filler ()
  "Remove all leaked filler text blocks from the chat buffer.
Filler text (`#+begin_quote ... #+end_quote') is temporarily
inserted during tool-call rounds.  Removes the known filler at
`eai-code--filler-marker' first, then scans for any leaked blocks
that contain status text (e.g. \"Turn\", emoji indicators) and
removes them too.  Normal user content with `#+begin_quote' is
preserved."
  ;; Fast path: remove filler at the known marker position
  (when (and eai-code--filler-marker
             (marker-position eai-code--filler-marker))
    (save-excursion
      (goto-char eai-code--filler-marker)
      (let ((end (save-excursion
                   (when (re-search-forward "#\\+end_quote" nil t)
                     (line-end-position)))))
        (when end
          (delete-region eai-code--filler-marker end)
          ;; Also delete trailing blank lines
          (while (and (> (point-max) (point-min))
                      (eq (char-before (point-max)) ?\n))
            (delete-region (1- (point-max)) (point-max))))))
    (set-marker eai-code--filler-marker nil))
  ;; Fallback: scan entire buffer for leaked filler blocks.
  ;; Only remove blocks that contain our status text ("Turn", emoji,
  ;; "processing", "executing", "awaiting") so user content is safe.
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+begin_quote" nil t)
      (let ((block-start (line-beginning-position))
            (block-end nil))
        (when (re-search-forward "#\\+end_quote" nil t)
          (setq block-end (line-end-position))
          ;; Check if this block looks like our filler text.
          ;; Case-insensitive so "Processing" matches "processing".
          (when (save-excursion
                  (goto-char block-start)
                  (let ((case-fold-search t))
                    (re-search-forward "Turn\\|⚙️\\|📥\\|processing\\|executing\\|awaiting\\|running\\|reading\\|writing\\|searching\\|listing\\|calling"
                                       block-end t)))
            (delete-region block-start block-end)
            ;; Delete any extra blank lines left behind
            (while (and (not (eobp))
                        (string-blank-p
                         (buffer-substring (line-beginning-position)
                                           (line-end-position))))
              (delete-region (line-beginning-position)
                             (min (1+ (line-end-position)) (point-max))))))))))

(defun eai-code--tool-name (tool-obj)
  "Extract the display name from a gptel TOOL-OBJ.
TOOL-OBJ may be:
- A `gptel-tool' CL struct record or vector
- A list `(TOOL ARGS RESULT)' or `(TOOL ARGS CB)' from callbacks
- A plist/alist with :name key

Falls back to `gptel-tool-name' if available.  Returns nil if nothing works."
  (when tool-obj
    (let ((obj tool-obj))
      ;; Unwrap callback list forms: (TOOL ARGS RESULT) or (TOOL . ARGS)
      (when (and (listp obj) (not (null obj)))
        (setq obj (car-safe obj)))
      (cond
       ;; CL struct record or vector: [cl-struct-TAG function name description ...]
       ((or (vectorp obj) (recordp obj))
        (let* ((len (length obj))
               (tag (aref obj 0))
               (name nil))
          (when (and (> len 2)
                     (symbolp tag)
                     (string-prefix-p "cl-struct-" (symbol-name tag)))
            ;; Slot 2 is the human-readable name string
            (let ((candidate (aref obj 2)))
              (when (stringp candidate)
                (setq name candidate)))
            ;; Fallback: slot 1 is the function symbol; use its name
            (unless name
              (let ((candidate (aref obj 1)))
                (when (symbolp candidate)
                  (setq name (symbol-name candidate))))))
          (or name
              (and (fboundp 'gptel-tool-name)
                   (condition-case nil
                       (let ((n (gptel-tool-name obj)))
                         (when (stringp n) n))
                     (error nil))))))
       ;; Plist/alist fallback
       ((and (listp obj) (not (null obj)))
        (or (plist-get obj :name)
            (plist-get obj 'name)
            (alist-get 'name obj)
            (alist-get "name" obj)
            (car-safe obj)))
       (t nil)))))

(defun eai-code--tool-call-description (call)
  "Return a short human-readable description of a tool CALL.
CALL is a cons cell (TOOL . ARGS) as passed by gptel.
If anything goes wrong, returns a safe fallback string."
  (condition-case err
      (let* ((tool (car-safe call))
             (args (cdr-safe call))
             (name (downcase (or (eai-code--tool-name tool) "?"))))
        (cond
         ;; Shell / execute — show the command
         ((member name '("bash" "shell" "exec" "run_command" "sh"))
          (let ((cmd (or (plist-get args :command)
                         (alist-get 'command args)
                         (car-safe args)
                         "?")))
            (format "running `%s`"
                    (eai-code--truncate-string (format "%s" cmd) 50))))
         ;; File reading
         ((member name '("read_file" "file_read" "read" "cat"))
          (let ((file (or (plist-get args :file)
                          (alist-get 'file args)
                          (plist-get args :path)
                          (alist-get 'path args)
                          (car-safe args)
                          "?")))
            (format "reading `%s`"
                    (eai-code--truncate-string (format "%s" file) 50))))
         ;; File writing
         ((member name '("write_file" "file_write" "write"))
          (let ((file (or (plist-get args :file)
                          (alist-get 'file args)
                          (plist-get args :path)
                          (alist-get 'path args)
                          (car-safe args)
                          "?")))
            (format "writing `%s`"
                    (eai-code--truncate-string (format "%s" file) 50))))
         ;; Search / grep
         ((member name '("search" "grep" "find" "rg" "ag"))
          (let ((query (or (plist-get args :query)
                           (alist-get 'query args)
                           (plist-get args :pattern)
                           (alist-get 'pattern args)
                           (plist-get args :regex)
                           (car-safe args)
                           "?")))
            (format "searching for `%s`"
                    (eai-code--truncate-string (format "%s" query) 50))))
         ;; Directory listing
         ((member name '("list_directory" "ls" "dir"))
          (let ((dir (or (plist-get args :directory)
                         (alist-get 'directory args)
                         (plist-get args :path)
                         (alist-get 'path args)
                         ".")))
            (format "listing `%s`"
                    (eai-code--truncate-string (format "%s" dir) 50))))
         ;; Generic fallback
         (t (format "calling %s" name))))
    (error
     (eai-code--debug-log "tool-call-description ERROR: %S" err)
     "executing tool")))

(defun eai-code--truncate-string (str len)
  "Truncate STR to at most LEN characters, appending ... if truncated."
  (if (> (length str) len)
      (concat (substring str 0 (max 0 (- len 3))) "...")
    str))

(defun eai-code--debug-log (fmt &rest args)
  "Log a debug message to *eai-code debug*.
Only logs when `eai-code-debug' is non-nil."
  (when (bound-and-true-p eai-code-debug)
    (let ((buf (get-buffer-create "*eai-code debug*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "%H:%M:%S ")
                (apply #'format fmt args)
                "\n")))))

(defun eai-code--handle-callback (response response-prefix &optional stream info)
  "Handle a single gptel callback chunk RESPONSE.
INFO is the gptel FSM info plist, used to extract error details.
Accumulates streaming text and reasoning when STREAM is non-nil,
and inserts the final response when the stream ends (RESPONSE is t).
When STREAM is nil, strings are full responses and inserted
immediately.  Tracks tool-call/tool-result state so that
`eai-code--request-active' stays t across multi-turn tool
conversations.
RESPONSE-PREFIX is the prefix to insert before the response text."
  (condition-case err
      (prog1
          (cond
           ;; Reasoning chunk: accumulate but don't display
           ((and (consp response) (eq (car response) 'reasoning))
            (setq eai-code--activity-type 'reasoning)
            (setq eai-code--activity-emoji (eai-code--random-emoji 'reasoning))
            (unless eai-code--request-active
              (setq eai-code--request-active t))
            (when (stringp (cdr response))
              (setq eai-code--stream-reasoning
                    (concat eai-code--stream-reasoning (cdr response)))
              (eai-code--debug-log "callback: reasoning chunk (%d chars, total: %d)"
                                   (length (cdr response))
                                   (length eai-code--stream-reasoning))))
           ;; Tool call: show feedback so user knows tools are running
           ((and (consp response) (eq (car response) 'tool-call))
            (setq eai-code--activity-type 'tool-call)
            (setq eai-code--activity-emoji (eai-code--random-emoji 'tool-call))
            (setq eai-code--tool-pending t)
            (cl-incf eai-code--tool-turn-count)
            (unless eai-code--request-active
              (setq eai-code--request-active t))
            (let ((calls (cdr response)))
              (eai-code--debug-log "callback: tool-call (%d): %s"
                                   (length calls)
                                   (mapconcat
                                    (lambda (c)
                                      (let* ((tool (car c))
                                             (args (cdr c))
                                             (tool-name (or (eai-code--tool-name tool) "?"))
                                             (args-str (eai-code--truncate-string
                                                        (format "%S" args) 120)))
                                        (format "%s(%s)" tool-name args-str)))
                                    calls " | "))
              (message "Turn %d: %s"
                       eai-code--tool-turn-count
                       (mapconcat #'eai-code--tool-call-description
                                  calls "; "))
              ;; gptel only reports calls needing confirmation here; they
              ;; don't run until accepted, so hand them to gptel's prompt.
              (run-at-time 0 nil #'gptel--display-tool-calls calls info t)
              ;; Replace old filler with current tool-call status
              (eai-code--remove-filler)
              (eai-code--insert-filler
               (format "⚙️  Turn %d: %s"
                       eai-code--tool-turn-count
                       (mapconcat #'eai-code--tool-call-description
                                  calls "; ")))))
           ;; Tool result: show feedback with tool names
           ;; Note: gptel may not send a separate `tool-call' callback to
           ;; our handler, so we do the descriptive work here.
           ((and (consp response) (eq (car response) 'tool-result))
            (setq eai-code--activity-type 'tool-result)
            (setq eai-code--activity-emoji (eai-code--random-emoji 'tool-result))
            (setq eai-code--tool-pending nil)
            (cl-incf eai-code--tool-turn-count)
            (unless eai-code--request-active
              (setq eai-code--request-active t))
            (let* ((results (cdr response))
                   (tool-names (delq nil (mapcar #'eai-code--tool-name results))))
              (eai-code--debug-log "callback: tool-result (%d): %s"
                                   (length results)
                                   (mapconcat
                                    (lambda (r)
                                      (eai-code--truncate-string
                                       (format "%S" r) 200))
                                    results " | "))
              (message "Turn %d: %s executed, awaiting response..."
                       eai-code--tool-turn-count
                       (or (mapconcat #'identity tool-names ", ") "tools"))
              ;; Replace old filler with current tool-result status
              (eai-code--remove-filler)
              (eai-code--insert-filler
               (format "📥 Turn %d: %s finished, awaiting model response..."
                       eai-code--tool-turn-count
                       (or (mapconcat #'identity tool-names ", ") "tools")))))
           ;; Text response
           ((stringp response)
            (if stream
                ;; Streaming: accumulate chunks, wait for t
                (progn
                  (eai-code--debug-log "callback: text chunk (%d chars)" (length response))
                  (setq eai-code--activity-type 'text)
                  (setq eai-code--activity-emoji (eai-code--random-emoji 'text))
                  (unless eai-code--request-active
                    (setq eai-code--request-active t))
                  (setq eai-code--stream-accumulator
                        (concat eai-code--stream-accumulator response)))
              ;; Non-streaming: insert immediately if non-empty
              (eai-code--debug-log "callback: text response (%d chars)" (length response))
              (setq eai-code--request-active nil
                    eai-code--activity-type nil)
              (eai-code--update-header-line)
              (if (string-blank-p response)
                  (eai-code--debug-log "callback: ignoring blank non-streaming response (tool-pending: %s)"
                                       eai-code--tool-pending)
                (eai-code--insert-response response response-prefix))
              ;; Only clear request-active if no tools are pending
              (unless eai-code--tool-pending
                (setq eai-code--request-active nil))))
           ;; End of stream marker (only in streaming mode)
           ((eq response t)
            (eai-code--debug-log "callback: end-of-stream")
            ;; Log reasoning if configured
            (when (and eai-code-log-reasoning eai-code--stream-reasoning)
              (eai-code--maybe-log-reasoning
               (concat "#+begin_reasoning\n"
                       eai-code--stream-reasoning
                       "#+end_reasoning\n")))
            ;; Insert the final response only if non-empty
            (if (or (null eai-code--stream-accumulator)
                    (string-blank-p eai-code--stream-accumulator))
                (progn
                  (eai-code--debug-log "callback: ignoring blank end-of-stream (reasoning: %d chars, tool-pending: %s)"
                                       (length (or eai-code--stream-reasoning ""))
                                       eai-code--tool-pending)
                  ;; Still clear accumulators
                  (setq eai-code--stream-accumulator nil
                        eai-code--stream-reasoning nil))
              (eai-code--insert-response eai-code--stream-accumulator response-prefix)
              ;; Clear accumulators
              (setq eai-code--stream-accumulator nil
                    eai-code--stream-reasoning nil))
            ;; Only clear request-active if no tools are pending.
            ;; If tools are pending, a follow-up request is coming.
            (if eai-code--tool-pending
                (eai-code--debug-log "end-of-stream: keeping active (tool pending)")
              (setq eai-code--request-active nil
                    eai-code--activity-type nil
                    eai-code--tool-turn-count 0
                    eai-code--request-start-time nil)))
           ;; Error (nil)
           ((null response)
            (eai-code--debug-log "callback: error (nil)")
            (setq eai-code--request-active nil
                  eai-code--tool-pending nil
                  eai-code--activity-type 'error
                  eai-code--activity-emoji (eai-code--random-emoji 'error)
                  eai-code--tool-turn-count 0
                  eai-code--request-start-time nil)
            (setq eai-code--stream-accumulator nil
                  eai-code--stream-reasoning nil)
            ;; Extract error details and insert a recoverable marker if needed
            (let* ((err (or (and info (plist-get info :error))
                            "Request failed: API unreachable"))
                   (pos (or (and eai-code--state
                                 (plist-get eai-code--state :position))
                            (point-max))))
              (when (and (boundp 'eai-code--state) eai-code--state)
                (plist-put eai-code--state :error err))
              (unless (and pos (get-char-property pos 'eai-code-error))
                (when (fboundp 'eai-code-error--insert-marker)
                  (eai-code-error--insert-marker pos err)))
              (message "Request failed%s"
                       (if (stringp err) (concat ": " err) ""))))
           ;; Unknown response type
           (t
            (eai-code--debug-log "callback: unknown response type: %S" response)))
        ;; Always refresh the header line after any callback so the
        ;; emoji indicator updates in real time.
        (eai-code--update-header-line))
    (error
     (eai-code--debug-log "callback ERROR: %S" err)
     (message "eai-code callback error: %S" err)
     (setq eai-code--request-active nil
           eai-code--tool-pending nil
           eai-code--activity-type 'error
           eai-code--activity-emoji (eai-code--random-emoji 'error)
           eai-code--tool-turn-count 0
           eai-code--request-start-time nil)
     (eai-code--update-header-line))))

;;;###autoload
(defun eai-code-send ()
  "Send the conversation history via `gptel-request'.
Builds the full conversation from the chat buffer and sends it,
inserting the response (with reasoning stripped) when it arrives.
Use `C-c C-c' in a chat buffer bound by `eai-code'."
  (interactive)
  (when eai-code--request-active
    (user-error "Request already in progress; wait or press C-g to abort"))
  (let* ((response-prefix (alist-get 'org-mode gptel-response-prefix-alist))
         (conversation (eai-code--build-conversation))
         (buf (current-buffer)))
    (when (or (null conversation)
              (string-empty-p (car (last conversation))))
      (user-error "Empty prompt"))
    ;; Auto-compact if the buffer has grown too large
    (when (fboundp 'eai-code-compact--maybe-auto)
      (eai-code-compact--maybe-auto))
    ;; Reset stream accumulators and tool state
    (setq eai-code--stream-accumulator nil
          eai-code--stream-reasoning nil
          eai-code--tool-pending nil
          eai-code--tool-turn-count 0
          eai-code--request-start-time (float-time))
    ;; Set boundary position to current point-max.  Unlike a marker,
    ;; this integer stays stable when the user types during the
    ;; request or when filler is inserted and removed.
    (setq eai-code--boundary-pos (point-max))
    (setq eai-code--request-active t)
    (eai-code--update-header-line)
    ;; Capture state for error recovery (normally done by gptel-send advice)
    (when (fboundp 'eai-code-error--capture-state)
      (eai-code-error--capture-state))
    (message "Sending...")
    (condition-case err
        (let ((stream (and (boundp 'gptel-stream) gptel-stream)))
          (setq-local gptel--fsm-last
                      (gptel-request conversation
                        :system (eai-code--directive)
                        :stream stream
                        :callback (lambda (response info)
                                    (with-current-buffer buf
                                      (eai-code--handle-callback response response-prefix stream info))))))
      (error
       (eai-code--debug-log "send ERROR: %S" err)
       (setq eai-code--request-active nil
             eai-code--tool-pending nil
             eai-code--activity-type 'error
             eai-code--activity-emoji (eai-code--random-emoji 'error)
             eai-code--tool-turn-count 0
             eai-code--request-start-time nil)
       (setq eai-code--boundary-pos nil)
       (eai-code--update-header-line)
       (message "Send failed: %S" err)))))

;;;###autoload
(defun eai-code-abort ()
  "Abort the current in-flight request in the chat buffer.
Bound to C-c C-k in eai-code chat buffers."
  (interactive)
  (if eai-code--request-active
      (progn
        (setq eai-code--request-active nil
              eai-code--tool-pending nil
              eai-code--activity-type nil
              eai-code--activity-emoji nil
              eai-code--tool-turn-count 0
              eai-code--request-start-time nil)
        (setq eai-code--stream-accumulator nil
              eai-code--stream-reasoning nil)
        (setq eai-code--boundary-pos nil)
        (eai-code--remove-filler)
        (gptel-abort (current-buffer))
        (eai-code--update-header-line)
        (message "Request aborted"))
    (message "No active request")))

;;; Chat buffer setup

(defun eai-code--save-state ()
  "Save gptel state to the chat buffer's org properties.
Intended as a `before-save-hook' in eai-code chat buffers."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (when (fboundp 'gptel-org--save-state)
          (gptel-org--save-state))
      (error nil))))

(defun eai-code--find-chat ()
  "Locate the chat file for this project, and set it up as gptel session."
  (interactive)
  (let* ((project (project-current))
         (_project-root (project-root project))
         (project-name (project-name project))
         (chat-file (eai-code--get-special-file 'chat))
         (chat-buf (if eai-code-persist-chat
                       (find-file-noselect chat-file)
                     (get-buffer-create (format "*%s-chat*" project-name)))))
    (with-current-buffer chat-buf
      ;; this should happen automatically by gptel later on anyway
      ;; this will remove our buffer local variables, so do that
      ;; first
      (unless (derived-mode-p 'org-mode)
        (org-mode))

      (unless eai-code-persist-chat
        (setq buffer-offer-save nil)
        (setq backup-inhibited t))

      (setq-local gptel-prompt-prefix-alist (copy-alist gptel-prompt-prefix-alist))
      (setq-local gptel-response-prefix-alist (copy-alist gptel-response-prefix-alist))

      (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@developer:\n")
      (setf (alist-get 'org-mode gptel-response-prefix-alist) "@code monkey:\n")
      (setq-local gptel-directives '((default . eai-code--directive)))
      (setq-local gptel--system-message (alist-get 'default gptel-directives))
      (setq-local buffer-auto-save-file-name nil)

      ;; We handle tool results and reasoning ourselves; don't let gptel
      ;; insert them into the chat buffer.
      (setq-local gptel-include-tool-results nil)
      (setq-local gptel-include-reasoning nil)

      ;; Load gptel-org features conditionally at runtime.
      ;; This is done at runtime (not compile time) so the library
      ;; only needs to be present when the user is using gptel.
      (condition-case nil
          (progn
            (require 'gptel-org)
            ;; Enable markdown-to-org conversion for cleaner responses
            (setq-local gptel-org-convert-response t)
            ;; Highlight org links that will be sent as context
            (jit-lock-register #'gptel-org--annotate-links)
            ;; Save/restore gptel state in org properties on save/open
            (add-hook 'before-save-hook #'eai-code--save-state nil t)
            ;; Restore state from existing chat files
            (when (and eai-code-persist-chat
                       (file-exists-p chat-file)
                       (> (buffer-size) 0))
              (gptel-org--restore-state)))
        (error nil))

      ;; Key bindings live in a minor mode map (no gptel-mode; we control the
      ;; full send flow)
      (eai-code-chat-mode 1)

      ;; Enable auto-compaction for this buffer
      (eai-code-compact-enable)

      ;; insert prefix for new buffers
      (when (= (buffer-size) 0)
        (insert (alist-get 'org-mode gptel-prompt-prefix-alist)))

      ;; Restore state may have set backend/model; only override with
      ;; defaults if they weren't restored from the file.
      (unless (local-variable-p 'gptel-backend)
        (when (and eai-code-default-backend
                   (not (string-empty-p eai-code-default-backend)))
          (setq-local gptel-backend (gptel-get-backend eai-code-default-backend))))
      (unless (local-variable-p 'gptel-model)
        (when eai-code-default-model
          (setq-local gptel-model eai-code-default-model)))

      ;; Initial status header
      (eai-code--setup-header-line))
    (switch-to-buffer chat-buf)))

(defvar-keymap eai-code-chat-mode-map
  :doc "Keymap for `eai-code-chat-mode'."
  "C-c C-c" #'eai-code-send
  "C-c C-k" #'eai-code-abort)

(define-minor-mode eai-code-chat-mode
  "Minor mode for eai-code chat buffers.
Its keymap is buffer-local, so the bindings don't leak into `org-mode-map' the
way `local-set-key' in an org buffer does."
  :lighter " eai"
  :keymap eai-code-chat-mode-map)

(defun eai-code (&optional arg)
  "Start a new code agent session.

Can be called with a prefix argument to provide a directory, otherwise it'll use
the current directory, if it is a project, or prompt."
  (interactive "P")
  (let ((project-dir (when arg
                       (read-directory-name "Select project directory: "))))
    (eai-code--find-or-create-project project-dir)
    (eai-code--setup-memory)
    (eai-code--setup-plan)
    (eai-code--find-chat)
    (eai-code-metrics-enable)
    (eai-code-monitor-enable)
    (eai-code-error-enable)
    (eai-code-agent-enable)))

(provide 'eai-code)

;;; eai-code.el ends here
