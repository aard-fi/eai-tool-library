;;; eai-code-metrics.el --- Performance metrics for eai-code -*- lexical-binding: t; -*-
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
;; Lightweight metrics collection for eai-code requests.  Tracks per-request
;; performance data (backend, model, success/failure, duration, tokens) to
;; enable future self-improvement analysis.
;;
;; Hooked into gptel's response pipeline via advice on `gptel--insert-response'
;; and `gptel--handle-error', which both receive the full `info' plist.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

(defvar eai-code--current-profile nil
  "Stub: current agent profile for the active request.

Will be moved to eai-code-agent.el when that module is created.")

(make-variable-buffer-local 'eai-code--current-profile)

(defgroup eai-code-metrics nil
  "Performance metrics for eai-code."
  :group 'eai-code)

(defcustom eai-code-metrics-max-entries 1000
  "Maximum number of metrics entries to retain.

Older entries are dropped when the log exceeds this size."
  :type 'integer
  :group 'eai-code-metrics)

(defcustom eai-code-metrics-log-file nil
  "If non-nil, path to a file where metrics are persisted across sessions.

Set to a file path to enable persistence."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Log file"))
  :group 'eai-code-metrics)

(defvar eai-code-metrics--log nil
  "Global performance log.

Each entry is a plist with keys:
  :timestamp    — float, seconds since epoch
  :buffer       — string, buffer name
  :backend      — string, backend name (e.g. \"anthropic\")
  :model        — symbol, model name (e.g. \='claude-sonnet-4-6)
  :profile      — symbol or nil, agent profile used (e.g. \='executor)
  :success      — boolean, t if response was successful
  :error-type   — symbol or nil, error classification on failure
  :error-msg    — string or nil, short error message
  :duration     — float or nil, elapsed time in seconds
  :input-tokens — integer or nil
  :output-tokens— integer or nil
  :cached-tokens— integer or nil
  :request-start— marker or nil, start position of this turn in buffer")

(defvar eai-code-metrics--request-start nil
  "Buffer-local variable storing the start time of the current request.
Set by `eai-code-metrics--pre-request' and read by the post-request advice.")

(make-variable-buffer-local 'eai-code-metrics--request-start)

(defun eai-code-metrics--record (info success &optional _response-text)
  "Record a metrics entry from INFO plist.
SUCCESS is non-nil for a successful response.  RESPONSE-TEXT is unused
but reserved for future extraction of response size."
  (let* ((buf (plist-get info :buffer))
         (backend (when-let* ((b (buffer-local-value 'gptel-backend buf)))
                    (ignore-errors (gptel-backend-name b))))
         (model (buffer-local-value 'gptel-model buf))
         (profile (or (ignore-errors (buffer-local-value 'eai-code--current-profile buf))
                      (plist-get info :profile)))
         (error-data (plist-get info :error))
         (error-type (cond
                      ((null error-data) nil)
                      ((stringp error-data) 'message)
                      ((plistp error-data) (or (plist-get error-data :type)
                                               'unknown))
                      (t 'unknown)))
         (error-msg (cond
                     ((null error-data) nil)
                     ((stringp error-data) (string-trim error-data))
                     ((plistp error-data)
                      (string-trim (gptel--to-string
                                    (or (plist-get error-data :message)
                                        (plist-get error-data :type)
                                        error-data))))
                     (t (gptel--to-string error-data))))
         (tokens (plist-get info :tokens))
         (start-time (with-current-buffer buf
                       (when (boundp 'eai-code-metrics--request-start)
                         eai-code-metrics--request-start)))
         (duration (when start-time
                     (- (float-time) start-time)))
         (entry `(:timestamp ,(float-time)
                             :buffer ,(buffer-name buf)
                             :backend ,backend
                             :model ,model
                             :profile ,profile
                             :success ,(and success t)
                             :error-type ,error-type
                             :error-msg ,error-msg
                             :duration ,duration
                             :input-tokens ,(plist-get tokens :input)
                             :output-tokens ,(plist-get tokens :output)
                             :cached-tokens ,(plist-get tokens :cached)
                             :request-start ,(plist-get info :position))))
    ;; Push to front and trim if needed
    (push entry eai-code-metrics--log)
    (when (> (length eai-code-metrics--log) eai-code-metrics-max-entries)
      (setq eai-code-metrics--log
            (cl-subseq eai-code-metrics--log 0 eai-code-metrics-max-entries)))
    ;; Persist if configured
    (when eai-code-metrics-log-file
      (eai-code-metrics--persist))))

(defun eai-code-metrics--persist ()
  "Write the current metrics log to `eai-code-metrics-log-file'."
  (ignore-errors
    (with-temp-file eai-code-metrics-log-file
      (prin1 eai-code-metrics--log (current-buffer)))))

(defun eai-code-metrics--load ()
  "Load metrics log from `eai-code-metrics-log-file' if it exists."
  (when (and eai-code-metrics-log-file
             (file-readable-p eai-code-metrics-log-file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents eai-code-metrics-log-file)
        (goto-char (point-min))
        (setq eai-code-metrics--log (read (current-buffer)))))))

;; Advice hooks to capture the full info plist from gptel internals.

(defun eai-code-metrics--after-insert-response (response info &optional _raw)
  "Advice after `gptel--insert-response'.
Records a successful response.  RESPONSE is the text.  INFO is the plist."
  (when (stringp response)
    (eai-code-metrics--record info t response)))

(defun eai-code-metrics--after-handle-error (fsm)
  "Advice after `gptel--handle-error'.
Records a failed response.  FSM is the finite state machine object."
  (when-let* ((info (gptel-fsm-info fsm))
              (error-data (plist-get info :error)))
    (eai-code-metrics--record info nil)))

(defun eai-code-metrics--pre-request (&rest _)
  "Advice before `gptel-send' or `gptel-request'.
Records the start time in the current buffer if it is a gptel buffer."
  (when (and (buffer-live-p (current-buffer))
             (derived-mode-p 'gptel-mode))
    (setq eai-code-metrics--request-start (float-time))))

;;; User-visible commands

;;;###autoload
(defun eai-code-metrics-enable ()
  "Enable metrics collection for eai-code."
  (interactive)
  (eai-code-metrics--load)
  (advice-add 'gptel--insert-response :after #'eai-code-metrics--after-insert-response)
  (advice-add 'gptel--handle-error :after #'eai-code-metrics--after-handle-error)
  (advice-add 'gptel-send :before #'eai-code-metrics--pre-request)
  (message "eai-code metrics enabled"))

;;;###autoload
(defun eai-code-metrics-disable ()
  "Disable metrics collection for eai-code."
  (interactive)
  (advice-remove 'gptel--insert-response #'eai-code-metrics--after-insert-response)
  (advice-remove 'gptel--handle-error #'eai-code-metrics--after-handle-error)
  (advice-remove 'gptel-send #'eai-code-metrics--pre-request)
  (message "eai-code metrics disabled"))

;;;###autoload
(defun eai-code-metrics-status ()
  "Show a brief summary of collected metrics."
  (interactive)
  (let* ((total (length eai-code-metrics--log))
         (successes (cl-count-if (lambda (e) (plist-get e :success))
                                 eai-code-metrics--log))
         (failures (- total successes))
         (by-backend (cl-loop for e in eai-code-metrics--log
                              for b = (or (plist-get e :backend) "unknown")
                              collect b into backends
                              finally return (cl-sort
                                              (cl-loop for pair in (seq-group-by #'identity backends)
                                                       collect (cons (car pair) (length (cdr pair))))
                                              #'> :key #'cdr))))
    (message "Metrics: %d entries (%d success, %d fail). Backends: %s"
             total successes failures
             (mapconcat (lambda (p) (format "%s:%d" (car p) (cdr p)))
                        by-backend ", "))))

;;;###autoload
(defun eai-code-metrics-failure-rate (backend &optional model)
  "Return failure rate for BACKEND (and optional MODEL) as a float 0.0–1.0.
Returns nil if no matching entries exist."
  (let* ((entries (cl-remove-if-not
                   (lambda (e)
                     (and (equal (plist-get e :backend) backend)
                          (or (null model)
                              (equal (plist-get e :model) model))))
                   eai-code-metrics--log))
         (total (length entries)))
    (when (> total 0)
      (/ (cl-count-if (lambda (e) (null (plist-get e :success))) entries)
         (float total)))))

;;;###autoload
(defun eai-code-metrics-backend-stats ()
  "Return an alist of backend statistics.
Each element is (BACKEND MODEL TOTAL FAILURES AVG-DURATION)."
  (let ((groups (seq-group-by (lambda (e)
                                (cons (plist-get e :backend)
                                      (plist-get e :model)))
                              eai-code-metrics--log)))
    (cl-loop for ((backend . model) . entries) in groups
             collect (list backend
                           model
                           (length entries)
                           (cl-count-if (lambda (e) (null (plist-get e :success)))
                                        entries)
                           (let ((durations (cl-remove-if-not #'numberp
                                                              (mapcar (lambda (e)
                                                                        (plist-get e :duration))
                                                                      entries))))
                             (if durations
                                 (/ (cl-reduce #'+ durations) (length durations))
                               nil))))))

;;;###autoload
(defun eai-code-metrics-export ()
  "Open a buffer with a human-readable metrics summary."
  (interactive)
  (let ((buf (get-buffer-create "*eai-code metrics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# eai-code Metrics Summary\n\n")
      (let ((stats (eai-code-metrics-backend-stats)))
        (if (null stats)
            (insert "No metrics recorded yet.\n")
          (insert "| Backend | Model | Total | Failures | Avg Duration | Failure Rate |\n")
          (insert "|---------|-------|-------|----------|--------------|-------------|\n")
          (dolist (s stats)
            (cl-destructuring-bind (backend model total failures avg) s
              (insert (format "| %s | %s | %d | %d | %s | %.1f%% |\n"
                              (or backend "—")
                              (or (and model (symbol-name model)) "—")
                              total failures
                              (if avg (format "%.2fs" avg) "—")
                              (* 100 (/ failures (float total))))))))))
    (pop-to-buffer buf)))

(provide 'eai-code-metrics)
;;; eai-code-metrics.el ends here
