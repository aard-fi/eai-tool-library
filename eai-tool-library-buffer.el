;;; eai-tool-library-buffer.el --- Tool functions for buffer access -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2025 Bernd Wachter
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
;; Provides LLM friendly interactions with emacs buffers.
;;
;;; Code:

(require 'eai-tool-library)

(defvar eai-code-edit-style nil
  "Forward declaration; defined in eai-code.el.")

(defvar eai-tool-library-buffer-tools '()
  "The list of buffer related tools")

(defvar eai-tool-library-buffer-tools-maybe-safe '()
  "The list of buffer related tools which may be destructive, but typically
the LLM behaves.")

(defvar eai-tool-library-buffer-tools-unsafe '()
  "The list of buffer related tools which are not safe.")

(defvar eai-tool-library-buffer-category-name "emacs-buffer"
  "The buffer category used for tool registration")

(defvar-local eai-tool-library-buffer--last-read-pos nil
  "Buffer local variable to track LLM read position.")

(defun eai-tool-library-buffer--filename (&optional buffer)
  "Returrn the full path of the file visited by `buffer' or current buffer"
  (buffer-file-name
   (get-buffer (if buffer
                   (format "%s" buffer)
                 (current-buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--filename
 :name  "buffer-filename"
 :description "Return the filename of a buffer, or nil if no filename is associated."
 :args (list '(:name "buffer"
                     :type string
                     :optional t
                     :description "The buffer to get the filename from. Uses the active buffer when omitted."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--read-buffer-contents (buffer)
  "Return contents of BUFFER."
  (eai-tool-library--debug-log (format "read-buffer-contents %s" buffer))
  (eai-tool-library--limit-result
   (let ((buffer (eai-tool-library--get-buffer buffer)))
     (with-current-buffer buffer
       (concat (buffer-substring-no-properties (point-min) (point-max)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-contents
 :name  "read-buffer-contents"
 :description "Read a buffers contents. The buffer may be a buffer name or a file path; files are opened if needed. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to retrieve contents from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--read-buffer-region (buffer from to)
  "Return contents of BUFFER region from FROM to TO."
  (eai-tool-library--debug-log (format "read-buffer-region %s %s->%s" buffer from to))
  (eai-tool-library--limit-result
   (let ((buffer (eai-tool-library--get-buffer buffer)))
     (with-current-buffer buffer
       (concat (buffer-substring-no-properties from to))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-region
 :name  "read-buffer-region"
 :description "Read a region of a buffer. The buffer may be a buffer name or a file path; files are opened if needed. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to retrieve contents from.")
             '(:name "from"
                     :type integer
                     :description "Start of the region to read.")
             '(:name "to"
                     :type integer
                     :description "End of the region to read."))
 :category "emacs-buffer")

;; TODO - we should also limit result here, but instead of throwing an error it'd
;;        probably bet better to do only partial reads if we'd exceed the result limit
(defun eai-tool-library-buffer--read-buffer-contents-since-last-read (buffer)
  "Return contents of BUFFER since last read, or all buffer on first read."
  (eai-tool-library--debug-log (format "read-buffer-contents-since-last-read %s" buffer))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos)
      (setq eai-tool-library-buffer--last-read-pos (point-min)))
    (let ((_buffer (eai-tool-library--get-buffer buffer))
          (last-pos eai-tool-library-buffer--last-read-pos))
      (setq eai-tool-library-buffer--last-read-pos (point-max))
      (message (format "Last read %s->%s" last-pos eai-tool-library-buffer--last-read-pos))
      (concat (buffer-substring-no-properties last-pos (point-max))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-contents-since-last-read
 :name  "read-buffer-contents-since-last-read"
 :description "Read content added to a buffer since last reading it. On first read, return complete buffer contents. The buffer may be a buffer name or a file path; files are opened if needed. This assumes buffers which only get appended to - don't try to edit a buffer read with this tool. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to retrieve contents from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--set-buffer-pos (buffer pos)
  "Set the last read position for buffer."
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos))
    (setq eai-tool-library-buffer--last-read-pos pos)))

(defun eai-tool-library-buffer--get-buffer-pos (buffer)
  "Get the last read position for buffer."
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos)
      (setq eai-tool-library-buffer--last-read-pos (point-min)))
    eai-tool-library-buffer--last-read-pos))

(defun eai-tool-library-buffer--list-buffers (&optional arg)
  "Return list of buffers."
  (eai-tool-library--debug-log (format "list-buffers %s" (or arg "")))
  (list-buffers-noselect)
  (with-current-buffer "*Buffer List*"
    (let ((content (buffer-string)))
      content)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--list-buffers
 :name  "list-buffers"
 :category "emacs-buffer"
 :description "List buffers open in Emacs, including file names and full paths. After using this, stop. Then evaluate which files are most likely to be relevant to the user's request."
 :category "emacs-buffer")

(defun eai-tool-library-buffer--get-in-direction (direction)
  "Return the name of the buffer in the window in DIRECTION from current window.

DIRECTION should be one of \='left, \='right, \='above, \='below, \='left-above,
\='left-below, \='right-above, or \='right-below (as symbols or strings).

If there is no window in that direction, return nil."
  (eai-tool-library--debug-log (format "buffer-in-direction %s" direction))
  (let* ((dir-sym (if (symbolp direction)
                      direction
                    (intern direction)))
         (win (window-in-direction dir-sym)))
    (when win
      (buffer-name (window-buffer win)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--get-in-direction
 :name "get-in-direction"
 :category "emacs-buffer"
 :description
 "Given a direction (e.g. left, right, above), return the name of the buffer displayed in that window relative to the currently selected window.
 Returns nil if no such window exists."
 :args (list '(:name "direction"
                     :type string
                     :description "The direction to search, one of left, right, above, below, right-above, right-below, left-above, left-below ."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--erase-buffer (buffer)
  "Erase contents of BUFFER."
  (eai-tool-library--debug-log (format "erase-buffer %s" buffer))
  (let ((buffer (eai-tool-library--get-buffer buffer)))
    (with-current-buffer buffer
      (erase-buffer))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--erase-buffer
 :name  "erase-buffer"
 :description "Erase buffers contents. The buffer may be a buffer name or a file path; files are opened if needed. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to erase contents in."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--buffer-size (buffer)
  "Return the size of BUFFER."
  (eai-tool-library--debug-log (format "buffer-size %s" buffer))
  (let ((buffer (eai-tool-library--get-buffer buffer)))
    (with-current-buffer buffer
      (buffer-size))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--buffer-size
 :name  "buffer-size"
 :description "Return the size of a buffer in characters. The buffer may be a buffer name or a file path; files are opened if needed. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to get the size from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--replace-region-confirm (buffer _from _to _text)
  "Return non-nil if replace-region needs confirmation for BUFFER."
  (eai-tool-library--buffer-modify-confirm-p buffer))

(defun eai-tool-library-buffer--replace-region (buffer from to text)
  "Replace text in BUFFER from FROM to TO with TEXT.

When `eai-code-edit-style' is `review', inserts smerge-style
conflict markers instead of replacing directly."
  (eai-tool-library--debug-log (format "replace-region %s->%s in %s with %s" from to buffer text))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (eai-tool-library-write-deny-check (buffer-file-name))
    (if (eq eai-code-edit-style 'review)
        (let ((old-text (buffer-substring-no-properties from to)))
          (goto-char from)
          (delete-region from to)
          (insert (concat "<<<<<<< REGION BEFORE REPLACE\n"
                          old-text
                          "=======\n"
                          text
                          "\n>>>>>>> REGION AFTER REPLACE\n"))
          (smerge-mode 1))
      (delete-region from to)
      (goto-char from)
      (insert text))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--replace-region
 :name  "replace-region"
 :description "Replace a region in a buffer with new text. The buffer may be a buffer name or a file path; files are opened if needed. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to replace contents in.")
             '(:name "from"
                     :type integer
                     :description "The begin of the region.")
             '(:name "to"
                     :type integer
                     :description "The end of the region.")
             '(:name "text"
                     :type string
                     :description "The text to replace the region with."))
 :category "emacs-buffer"
 :confirm #'eai-tool-library-buffer--replace-region-confirm)

(defun eai-tool-library-buffer--remove-region-confirm (buffer _from _to)
  "Return non-nil if remove-region needs confirmation for BUFFER."
  (eai-tool-library--buffer-modify-confirm-p buffer))

(defun eai-tool-library-buffer--remove-region (buffer from to)
  "Remove region from FROM to TO in buffer BUFFER.

When `eai-code-edit-style' is `review', wraps the removed text in
smerge-style markers so the deletion can be reviewed."
  (eai-tool-library--debug-log (format "remove-region %s->%s from %s" from to buffer))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (eai-tool-library-write-deny-check (buffer-file-name))
    (if (eq eai-code-edit-style 'review)
        (let ((old-text (buffer-substring-no-properties from to)))
          (goto-char from)
          (delete-region from to)
          (insert (concat "<<<<<<< REMOVED REGION\n"
                          old-text
                          "=======\n"
                          "\n>>>>>>> END REMOVED REGION\n"))
          (smerge-mode 1))
      (delete-region from to))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--remove-region
 :name  "remove-region"
 :description "Remove a region in a buffer. The buffer may be a buffer name or a file path; files are opened if needed. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to remove contents in.")
             '(:name "from"
                     :type integer
                     :description "The begin of the region.")
             '(:name "to"
                     :type integer
                     :description "The end of the region."))
 :category "emacs-buffer"
 :confirm #'eai-tool-library-buffer--remove-region-confirm)

(defun eai-tool-library-buffer--insert-at-confirm (buffer _at _text)
  "Return non-nil if insert-at needs confirmation for BUFFER."
  (eai-tool-library--buffer-modify-confirm-p buffer))

(defun eai-tool-library-buffer--insert-at (buffer at text)
  "Move point in buffer BUFFER to AT, and then insert TEXT.

When `eai-code-edit-style' is `review', wraps the inserted text
in smerge-style markers instead of inserting directly."
  (eai-tool-library--debug-log (format "insert-at %s at %s in %s" text at buffer))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (eai-tool-library-write-deny-check (buffer-file-name))
    (goto-char (+ 1 at))
    (if (eq eai-code-edit-style 'review)
        (progn
          (insert (concat "<<<<<<< INSERTED AT POSITION "
                          (number-to-string at) "\n"
                          text
                          "\n=======\n"
                          ">>>>>>> END INSERT\n"))
          (smerge-mode 1))
      (insert text))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--insert-at
 :name  "insert-at"
 :description "Insert text in a buffer at a specific location. The buffer may be a buffer name or a file path; files are opened if needed. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to add contents to.")
             '(:name "at"
                     :type integer
                     :description "The point in the buffer where text should be inserted.")
             '(:name "text"
                     :type string
                     :description "The text to insert with."))
 :category "emacs-buffer"
 :confirm #'eai-tool-library-buffer--insert-at-confirm)

(defun eai-tool-library-buffer--number (value)
  "Return VALUE as a number; LLMs sometimes send numbers as strings."
  (if (stringp value) (string-to-number value) value))

(defun eai-tool-library-buffer--true-p (value)
  "Return non-nil if VALUE is true; JSON false arrives as `:json-false'."
  (and value (not (eq value :json-false))))

(defun eai-tool-library-buffer--refresh ()
  "Revert the current buffer if its file changed and it has no unsaved edits."
  (when (and buffer-file-name
             (not (buffer-modified-p))
             (not (verify-visited-file-modtime)))
    (revert-buffer t t t)))

(defun eai-tool-library-buffer--numbered-lines (start end)
  "Return lines START to END of the current buffer, numbered like cat -n.
Output is cut at a line boundary before it exceeds
`eai-tool-library-max-result-size', with a note on where to continue."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((total (count-lines (point-min) (point-max)))
             (end (min total end))
             (line start)
             (size 0)
             lines)
        (goto-char (point-min))
        (forward-line (1- start))
        (catch 'full
          (while (<= line end)
            (let ((text (format "%6d\t%s\n" line
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
              (when (> (+ size (length text)) eai-tool-library-max-result-size)
                (push (format "[truncated; continue from line %d of %d]\n"
                              line total)
                      lines)
                (throw 'full nil))
              (push text lines)
              (setq size (+ size (length text))
                    line (1+ line))
              (forward-line 1))))
        (apply #'concat (nreverse lines))))))

(defun eai-tool-library-buffer--read-lines (buffer &optional start end)
  "Return lines START to END of BUFFER, numbered like cat -n.
BUFFER may be a buffer name or a file path.  START defaults to 1, END
to the last line."
  (eai-tool-library--debug-log (format "read-lines %s %s->%s" buffer start end))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (eai-tool-library-buffer--refresh)
    (eai-tool-library-buffer--numbered-lines
     (max 1 (or (eai-tool-library-buffer--number start) 1))
     (or (eai-tool-library-buffer--number end) most-positive-fixnum))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-lines
 :name "read-lines"
 :description "Read lines of a file or buffer, numbered like `cat -n'. Reads the buffer when the file is open, so unsaved edits are included. Long output is cut at a line boundary with a note where to continue. The buffer may be a buffer name or a file path; files are opened if needed."
 :args (list '(:name "buffer"
                     :type string
                     :description "Buffer name or file path.")
             '(:name "start"
                     :type integer
                     :optional t
                     :description "First line to read, 1-based. Defaults to 1.")
             '(:name "end"
                     :type integer
                     :optional t
                     :description "Last line to read. Defaults to the last line."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--replace-text-confirm (buffer _old _new &optional _all)
  "Return non-nil if replace-text needs confirmation for BUFFER."
  (eai-tool-library--buffer-modify-confirm-p buffer))

(defun eai-tool-library-buffer--replace-text (buffer old new &optional all)
  "Replace the exact text OLD with NEW in BUFFER.
OLD must occur exactly once, unless ALL is true, in which case every
occurrence is replaced.  The edit is one change group: in Lisp buffers
the new text is re-indented, and if the buffer had balanced parens
before but not after, the edit is rolled back.  A file buffer without
unsaved changes before the edit is saved.  Returns the edited lines
with some context."
  (eai-tool-library--debug-log (format "replace-text %s" buffer))
  (when (string-empty-p old)
    (error "OLD must not be empty"))
  (with-current-buffer (eai-tool-library--get-buffer buffer)
    (eai-tool-library-write-deny-check (buffer-file-name))
    (eai-tool-library-buffer--refresh)
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search nil)
              (all (eai-tool-library-buffer--true-p all))
              (lisp (derived-mode-p 'lisp-data-mode))
              (was-modified (buffer-modified-p))
              (count 0)
              regions)
          (goto-char (point-min))
          (while (search-forward old nil t)
            (setq count (1+ count)))
          (cond
           ((= count 0)
            (error "Text not found in %s" (buffer-name)))
           ((and (> count 1) (not all))
            (error "Text occurs %d times in %s; include more context to make it unique, or set all"
                   count (buffer-name))))
          (let ((balanced (or (not lisp) (ignore-errors (check-parens) t)))
                (group (prepare-change-group)))
            (activate-change-group group)
            (condition-case err
                (progn
                  (goto-char (point-min))
                  (while (search-forward old nil t)
                    (replace-match new t t)
                    (push (cons (copy-marker (- (point) (length new)))
                                (point-marker))
                          regions))
                  (when lisp
                    (dolist (region regions)
                      (indent-region (car region) (cdr region)))
                    (when balanced
                      (check-parens)))
                  (accept-change-group group))
              (error
               (cancel-change-group group)
               (error "Edit rolled back: %s" (error-message-string err)))))
          (let* ((regions (nreverse regions))
                 (first (line-number-at-pos (car (car regions))))
                 (last (line-number-at-pos (cdr (car (last regions)))))
                 (saved (and buffer-file-name (not was-modified)
                             (progn (save-buffer) t))))
            (format "Replaced %d occurrence%s in %s%s\n%s"
                    count (if (= count 1) "" "s")
                    (or buffer-file-name (buffer-name))
                    (if saved ", saved"
                      " (not saved: buffer had unsaved changes)")
                    (eai-tool-library-buffer--numbered-lines
                     (max 1 (- first 2)) (+ last 2)))))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--replace-text
 :name "replace-text"
 :description "Replace an exact piece of text in a file or buffer. Prefer this over shell tools like sed. OLD must match exactly, including whitespace and indentation, and occur exactly once unless all is true; include enough surrounding lines to make it unique. In Lisp buffers the new text is re-indented and an edit that unbalances parens is rolled back. A file without unsaved changes is saved afterwards. Returns the edited lines with context. The buffer may be a buffer name or a file path; files are opened if needed."
 :args (list '(:name "buffer"
                     :type string
                     :description "Buffer name or file path.")
             '(:name "old"
                     :type string
                     :description "The exact text to replace.")
             '(:name "new"
                     :type string
                     :description "The replacement text.")
             '(:name "all"
                     :type boolean
                     :optional t
                     :description "Replace every occurrence instead of requiring exactly one."))
 :category "emacs-buffer"
 :confirm #'eai-tool-library-buffer--replace-text-confirm)

;; the following tools directly make existing functions available
(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'get-buffer-create
 :name  "get-buffer-create"
 :description "Use get-buffer-create to create or get a buffer. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to create or retrieve."))
 :category "emacs-buffer")

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'switch-to-buffer
 :name  "switch-to-buffer"
 :description "Use switch-to-buffer to switch to a buffer. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to switch to."))
 :category "emacs-buffer")

(provide 'eai-tool-library-buffer)

;;; eai-tool-library-buffer.el ends here
