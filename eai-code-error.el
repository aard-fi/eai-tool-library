;;; eai-code-error.el --- Error recovery for eai-code -*- lexical-binding: t; -*-
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
;; Error tracking and recovery for eai-code requests.
;;
;; - Buffer-local `eai-code--state' tracks the last request's context
;; - Error markers in chat buffer with clickable recovery actions
;; - `eai-code-retry', `eai-code-retry-with-profile', `eai-code-abort'
;; - Tool call failure recovery with per-tool retry overlays
;; - Integrates with `eai-code-metrics' to log recovery attempts
;;
;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

(defvar eai-code--current-profile)

(defvar eai-code--state nil
  "Buffer-local plist tracking the last request state.

Keys:
  :prompt       — prompt text sent (or marker position)
  :callback     — original callback function
  :profile      — agent profile used (if any)
  :attempt-count — number of retry attempts so far
  :error        — last error info plist
  :fsm          — the fsm from the last request
  :position     — marker position where response would be inserted
  :backend      — backend name used
  :model        — model symbol used
  :stream       — whether streaming was enabled
  :transforms   — prompt transform functions
  :system       — system message used")

(make-variable-buffer-local 'eai-code--state)

(defvar eai-code-error--marker-face 'error
  "Face used for error marker blocks.")

(defvar eai-code-error--max-retries 3
  "Maximum number of automatic retry attempts.")

;;; State capture

(defun eai-code-error--capture-state (&rest _)
  "Capture pre-send state into `eai-code--state'.

Intended as :before advice on `gptel-send'."
  (setq eai-code--state
        (list :prompt (point)
              :profile (or (bound-and-true-p eai-code--current-profile) 'default)
              :attempt-count 0
              :error nil
              :position (point-marker)
              :backend (gptel-backend-name gptel-backend)
              :model gptel-model
              :stream gptel-stream
              :transforms gptel-prompt-transform-functions
              :system gptel--system-message)))

;;; Error handling

(defun eai-code-error--handle-error (fsm)
  "Advice after `gptel--handle-error'.  FSM is the finite state machine.

Captures error info and inserts a recoverable error marker."
  (when-let* ((info (gptel-fsm-info fsm))
              (err (plist-get info :error)))
    ;; Update state with error info
    (plist-put eai-code--state :error err)
    (plist-put eai-code--state :fsm fsm)
    ;; Insert error marker in the buffer
    (let ((buf (plist-get info :buffer))
          (pos (or (plist-get info :tracking-marker)
                   (plist-get info :position))))
      (when (and buf (buffer-live-p buf) pos)
        (with-current-buffer buf
          (save-excursion
            (goto-char pos)
            (eai-code-error--insert-marker pos err)))))
    ;; Log recovery opportunity via metrics
    (when (boundp 'eai-code-metrics--log)
      (eai-code-metrics--record-recovery
       (plist-get eai-code--state :backend)
       (plist-get eai-code--state :model)
       err))))

(defun eai-code-error--insert-marker (pos error-info)
  "Insert a recoverable error marker at POS.

ERROR-INFO is the error plist from the fsm.  The marker contains
actual clickable buttons for [Retry], [Retry with profile], and [Abort]."
  (let ((inhibit-read-only t)
        (msg (eai-code-error--format-message error-info)))
    (save-excursion
      (goto-char pos)
      ;; Insert plain text wrapper
      (insert "\n#+begin_error\n" "Request failed: " msg "\n")
      ;; Insert [Retry] button
      (eai-code-error--insert-button "[Retry]" 'eai-code-error--retry-keymap error-info)
      (insert " ")
      ;; Insert [Retry with profile] button
      (eai-code-error--insert-button "[Retry with profile]" 'eai-code-error--retry-profile-keymap error-info)
      (insert " ")
      ;; Insert [Abort] button
      (eai-code-error--insert-button "[Abort]" 'eai-code-error--abort-keymap error-info)
      (insert "\n#+end_error\n\n"))))

(defun eai-code-error--insert-button (label keymap-sym error-info)
  "Insert LABEL as a clickable button.
KEYMAP-SYM is a quoted symbol whose value is the keymap to use.
ERROR-INFO is stored as a text property."
  (let ((start (point))
        end)
    (insert label)
    (setq end (point))
    (add-text-properties
     start end
     `(keymap ,(symbol-value keymap-sym)
              face link
              mouse-face highlight
              help-echo "Click or press RET to activate"
              eai-code-error t
              eai-code-error-info ,error-info))))

(defun eai-code-error--format-message (error-info)
  "Extract a human-readable message string from ERROR-INFO."
  (let ((msg (cond
              ((stringp error-info) error-info)
              ((plistp error-info)
               (or (plist-get error-info :message)
                   (plist-get error-info :type)
                   (gptel--to-string error-info)))
              (t (gptel--to-string error-info)))))
    (string-trim msg)))

(defun eai-code-error--button-keymap (action)
  "Return a keymap that invokes ACTION (a function symbol) on RET or mouse-1."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                `(lambda ()
                   (interactive)
                   (call-interactively #',action)))
    (define-key map [mouse-1]
                `(lambda (event)
                   (interactive "e")
                   (mouse-set-point event)
                   (call-interactively #',action)))
    map))

(defvar eai-code-error--retry-keymap
  (eai-code-error--button-keymap 'eai-code-retry)
  "Keymap for the [Retry] button.")

(defvar eai-code-error--retry-profile-keymap
  (eai-code-error--button-keymap 'eai-code-retry-with-profile)
  "Keymap for the [Retry with profile] button.")

(defvar eai-code-error--abort-keymap
  (eai-code-error--button-keymap 'eai-code-abort)
  "Keymap for the [Abort] button.")

;;; Recovery commands

;;;###autoload
(defun eai-code-retry ()
  "Retry the last failed request with the same parameters.

Increments the attempt count.  If max retries exceeded, prompt for
confirmation."
  (interactive)
  (if (null eai-code--state)
      (message "No pending request to retry")
    (let ((attempts (or (plist-get eai-code--state :attempt-count) 0)))
      (when (>= attempts eai-code-error--max-retries)
        (unless (y-or-n-p (format "Already tried %d times. Retry again? " attempts))
          (user-error "Retry cancelled")))
      (plist-put eai-code--state :attempt-count (1+ attempts))
      ;; Remove the error marker if present
      (eai-code-error--clear-marker)
      ;; Re-send using gptel-send
      (message "Retrying request (attempt %d)..." (1+ attempts))
      (gptel-send))))

;;;###autoload
(defun eai-code-retry-with-profile (profile)
  "Retry the last failed request with a different PROFILE.

PROFILE is a symbol from `eai-code-agent-profiles'.  The profile
will be applied before resending."
  (interactive
   (list (intern (completing-read "Profile: "
                                  (mapcar (lambda (p) (symbol-name (car p)))
                                          (when (boundp 'eai-code-agent-profiles)
                                            eai-code-agent-profiles))
                                  nil t))))
  (if (null eai-code--state)
      (message "No pending request to retry")
    (setq eai-code--current-profile profile)
    (plist-put eai-code--state :profile profile)
    (plist-put eai-code--state :attempt-count 0)
    (eai-code-error--clear-marker)
    (message "Retrying with profile '%s'..." profile)
    (gptel-send)))

;;;###autoload
(defun eai-code-abort ()
  "Clear the error state and allow a new prompt.

Does NOT send anything.  Just resets the local state so the user
can type a new prompt without retrying."
  (interactive)
  (eai-code-error--clear-marker)
  (setq eai-code--state nil)
  (message "Error state cleared. You can send a new prompt."))

;;; Marker cleanup

(defun eai-code-error--clear-marker ()
  "Remove any error markers from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (let ((pos (next-single-property-change (point) 'eai-code-error)))
               (when pos
                 (goto-char pos)
                 (when (get-char-property pos 'eai-code-error)
                   (let ((start pos)
                         (end (or (next-single-property-change pos 'eai-code-error)
                                  (point-max))))
                     (delete-region start end))))
               pos)))))

;;; Tool call failure recovery

(defun eai-code-error--tool-retry-overlay (tool-call-info)
  "Create an overlay for retrying a failed tool call.

TOOL-CALL-INFO is a plist with :name, :args, and :id.  The overlay
provides a clickable [Retry tool] action."
  (let* ((ov (make-overlay (point) (point) nil t t))
         (name (plist-get tool-call-info :name))
         (_args (plist-get tool-call-info :args)))
    (overlay-put ov 'eai-code-tool-retry t)
    (overlay-put ov 'eai-code-tool-info tool-call-info)
    (overlay-put ov 'after-string
                 (concat "\n"
                         (propertize (format "[Retry %s]" name)
                                     'face 'link
                                     'keymap (let ((map (make-sparse-keymap)))
                                               (define-key map (kbd "RET")
                                                           (lambda ()
                                                             (interactive)
                                                             (eai-code-error--retry-tool tool-call-info)))
                                               map))))
    ov))

(defun eai-code-error--retry-tool (tool-call-info)
  "Retry a single tool call.

TOOL-CALL-INFO contains the tool name and arguments.  This sends
the tool result back to the LLM to continue the conversation.

Note: This is a best-effort retry.  Non-idempotent tools may
have side effects on retry."
  (let ((name (plist-get tool-call-info :name))
        (args (plist-get tool-call-info :args))
        (id (plist-get tool-call-info :id)))
    (message "Retrying tool call: %s" name)
    ;; Find the tool and call it
    (when-let* ((tool (gptel-get-tool name))
                (fn (gptel-tool-function tool)))
      (condition-case err
          (let ((result (apply fn args)))
            ;; Send result back to LLM
            (when (fboundp 'gptel--send-tool-result)
              (gptel--send-tool-result id result)))
        (error
         (message "Tool retry failed: %S" err))))))

;;; Metrics integration

(declare-function eai-code-metrics--record-recovery "eai-code-metrics"
                  (backend model error))

;;; Enable/disable

;;;###autoload
(defun eai-code-error-enable ()
  "Enable error tracking and recovery hooks."
  (interactive)
  (advice-add 'gptel-send :before #'eai-code-error--capture-state)
  (advice-add 'gptel--handle-error :after #'eai-code-error--handle-error)
  (message "eai-code error recovery enabled"))

;;;###autoload
(defun eai-code-error-disable ()
  "Disable error tracking and recovery hooks."
  (interactive)
  (advice-remove 'gptel-send #'eai-code-error--capture-state)
  (advice-remove 'gptel--handle-error #'eai-code-error--handle-error)
  (message "eai-code error recovery disabled"))

(provide 'eai-code-error)
;;; eai-code-error.el ends here
