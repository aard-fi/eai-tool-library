;;; eai-code-compact.el --- Context compaction for eai-code -*- lexical-binding: t; -*-
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
;; Inline buffer summarization when the chat context grows too long.
;;
;; The chat buffer alternates @developer (user) and @code monkey (assistant)
;; turns.  When the buffer approaches a configurable token threshold, old turns
;; are summarized into compact blocks so recent context remains intact.
;;
;; Key commands:
;; - `eai-code-compact'      — manually compact older turns
;; - `eai-code-compact-auto' — toggle automatic compaction before each send
;;
;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

(defgroup eai-code-compact nil
  "Context compaction for eai-code."
  :group 'eai-code)

(defcustom eai-code-compact-token-threshold 6000
  "Approximate token count at which to trigger compaction.
This is a rough heuristic (4 chars ≈ 1 token)."
  :type 'integer
  :group 'eai-code-compact)

(defcustom eai-code-compact-keep-turns 4
  "Number of most recent turns to preserve intact."
  :type 'integer
  :group 'eai-code-compact)

(defcustom eai-code-compact-summary-prompt
  "Summarize the following conversation exchanges concisely. Preserve key decisions, TODO items, and architectural choices. Discard filler, pleasantries, and repeated confirmations.

%s

---
Provide a compact summary (3-5 sentences max)."
  "Prompt template for summarizing old turns.
Must contain a single %s for the turn text."
  :type 'string
  :group 'eai-code-compact)

(defvar-local eai-code-compact--auto-enabled nil
  "Non-nil when auto-compaction is active in the current buffer.")

;;; Turn parsing

(defun eai-code-compact--turns (&optional buf)
  "Parse BUF into a list of turns.
Each turn is a plist with :prompt-start, :prompt-end, :response-start,
:response-end, and :text (the full raw text of both sides).
Returns turns oldest-first.
BUF defaults to the current buffer."
  (with-current-buffer (or buf (current-buffer))
    (let ((prompt-prefix (or (alist-get 'org-mode gptel-prompt-prefix-alist)
                             "@developer:\n"))
          (response-prefix (or (alist-get 'org-mode gptel-response-prefix-alist)
                               "@code monkey:\n"))
          (turns nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward prompt-prefix nil t)
          (let* ((prompt-start (match-beginning 0))
                 (prompt-end (match-end 0))
                 (response-start (save-excursion
                                   (if (search-forward response-prefix nil t)
                                       (match-beginning 0)
                                     (point-max))))
                 (response-end (save-excursion
                                 (goto-char response-start)
                                 (if (search-forward prompt-prefix nil t)
                                     (match-beginning 0)
                                   (point-max))))
                 (text (buffer-substring-no-properties prompt-start response-end)))
            (push (list :prompt-start prompt-start
                        :prompt-end prompt-end
                        :response-start response-start
                        :response-end response-end
                        :text text)
                  turns))))
      (nreverse turns))))
;;; Summarization

(defun eai-code-compact--summarize (text callback)
  "Summarize TEXT and call CALLBACK with the summary string.
Uses `gptel-request' with a summarization prompt."
  (let ((prompt (format eai-code-compact-summary-prompt text)))
    (gptel-request prompt
      :callback (lambda (response _info)
                  (funcall callback (or response "[summary unavailable]"))))))

;;; Replacement

(defun eai-code-compact--replace (turns summary)
  "Replace TURNS (a list of turn plists) with SUMMARY.
Inserts a compact summary block at the position of the first turn
and deletes all turn text.  Must be called in the chat buffer."
  (when turns
    (let ((first-start (plist-get (car turns) :prompt-start))
          (last-end (plist-get (car (last turns)) :response-end)))
      (save-excursion
        (let ((inhibit-read-only t))
          (delete-region first-start last-end)
          (goto-char first-start)
          (insert "#+begin_summary\n"
                  "Context summary:\n"
                  summary "\n"
                  "#+end_summary\n\n"))))))
;;; User commands

;;;###autoload
(defun eai-code-compact (&optional keep-turns)
  "Compact older chat turns into a summary block.
Preserves the last KEEP-TURNS turns (defaults to
`eai-code-compact-keep-turns').  Interactively, prompts for
KEEP-TURNS with the current default."
  (interactive
   (list (read-number "Turns to keep: " eai-code-compact-keep-turns)))
  (let* ((keep (or keep-turns eai-code-compact-keep-turns))
         (turns (eai-code-compact--turns))
         (to-compact (butlast turns keep))
         (buf (current-buffer)))
    (cond
     ((null to-compact)
      (message "Nothing to compact (only %d turn(s))." (length turns)))
     ((= (length to-compact) 1)
      (message "Only 1 old turn — not worth summarizing yet."))
     (t
      (message "Compacting %d turns..." (length to-compact))
      (let ((text (mapconcat (lambda (turn) (plist-get turn :text))
                             to-compact "\n---\n")))
        (eai-code-compact--summarize
         text
         (lambda (summary)
           (with-current-buffer buf
             (save-excursion
               (eai-code-compact--replace to-compact summary))
             (message "Compacted %d turns into summary block."
                      (length to-compact))))))))))

;;;###autoload
(defun eai-code-compact-enable ()
  "Enable automatic compaction in the current buffer."
  (interactive)
  (setq-local eai-code-compact--auto-enabled t)
  (message "Auto-compaction enabled (threshold: ~%d tokens)"
           eai-code-compact-token-threshold))

;;;###autoload
(defun eai-code-compact-disable ()
  "Disable automatic compaction in the current buffer."
  (interactive)
  (setq-local eai-code-compact--auto-enabled nil)
  (message "Auto-compaction disabled"))

;;;###autoload
(defun eai-code-compact-auto-toggle ()
  "Toggle automatic compaction in the current buffer."
  (interactive)
  (if eai-code-compact--auto-enabled
      (eai-code-compact-disable)
    (eai-code-compact-enable)))

(defun eai-code-compact--maybe-auto ()
  "If the buffer is large and auto-compaction is enabled, compact old turns.
Returns t if compaction was triggered, nil otherwise."
  (when (and eai-code-compact--auto-enabled
             (> (/ (buffer-size) 4) eai-code-compact-token-threshold))
    (eai-code-compact eai-code-compact-keep-turns)
    t))
(provide 'eai-code-compact)
;;; eai-code-compact.el ends here
