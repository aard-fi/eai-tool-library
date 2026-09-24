;;; eai-tool-library-org.el --- Org-mode tools for LLM agents -*- lexical-binding: t; -*-
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
;; Org-mode specific tools for LLM agents.
;;
;; Most tools accept a heading name as the primary identifier and navigate
;; internally, so the LLM does not need to do a separate navigate step.
;; Subtree boundaries use org's own functions rather than outline :to
;; positions, which may not correctly represent nested subtree boundaries.
;;
;; Duplicate headings are handled via an optional INDEX argument (0-based).
;; When multiple headings match and INDEX is not provided, a clear error
;; message listing all matches is returned so the LLM can retry.
;;
;;; Code:

(require 'eai-tool-library)
(require 'eai-tool-library-outline)

(require 'org)

(defvar eai-tool-library-org-tools '()
  "The list of org-mode related tools")

(defvar eai-tool-library-org-tools-maybe-safe '()
  "The list of org-mode related tools which may be destructive.")

(defvar eai-tool-library-org-category-name "emacs-org"
  "The org category used for tool registration")

;;; Navigation helpers

(defun eai-tool-library-org--find-headings (buffer heading-name)
  "Return all outline entries matching HEADING-NAME in BUFFER.
Each entry is a plist from `eai-tool-library-outline--get'."
  (with-current-buffer buffer
    (seq-filter (lambda (entry)
                  (string= (plist-get entry :name) heading-name))
                (eai-tool-library-outline--get buffer))))

(defun eai-tool-library-org--duplicate-error (heading-name matches)
  "Format an error string listing duplicate MATCHES for HEADING-NAME.
MATCHES is a list of outline entry plists.  Indexes are 0-based."
  (concat (format "Multiple headings match '%s'. Use the index argument (0-based) to disambiguate:\n" heading-name)
          (cl-loop for i from 0
                   for entry in matches
                   concat (format "  [%d] %s (position %d)\n"
                                  i (plist-get entry :name) (plist-get entry :from)))))

(defun eai-tool-library-org--goto-heading (buffer heading-name &optional index)
  "Navigate to the INDEX-th heading matching HEADING-NAME in BUFFER.

If INDEX is nil and only one match exists, navigate to it.
If INDEX is nil and multiple matches exist, return a duplicate
error string.  Returns the position on success, nil if no match
is found, or an error string when duplicates are detected."
  (with-current-buffer buffer
    (let* ((matches (eai-tool-library-org--find-headings buffer heading-name))
           (count (length matches)))
      (cond
       ((zerop count) nil)
       ((and (> count 1) (null index))
        (eai-tool-library-org--duplicate-error heading-name matches))
       (t (let ((match (nth (or index 0) matches)))
            (when match
              (goto-char (plist-get match :from))
              (point))))))))

(defun eai-tool-library-org--resolve-heading (buffer heading-name &optional index)
  "Internal helper: resolve HEADING-NAME with optional INDEX.
Returns (position . nil) on success, or (nil . message) on failure.
The caller should check (car result) for position or (cdr result) for error."
  (let ((result (eai-tool-library-org--goto-heading buffer heading-name index)))
    (cond
     ((null result)
      (cons nil (format "Heading '%s' not found in buffer %s" heading-name buffer)))
     ((stringp result)
      (cons nil result))
     (t (cons result nil)))))

;;; Org tools

(defun eai-tool-library-org--archive-subtree (buffer heading-name &optional index)
  "Archive the subtree at HEADING-NAME in BUFFER.

Optional INDEX (0-based) disambiguates when multiple headings match.
Navigates to the heading and calls `org-archive-subtree'.
Returns a success or error message."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (org-archive-subtree)
        (format "Archived subtree '%s' in %s" heading-name buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools-maybe-safe
 :function #'eai-tool-library-org--archive-subtree
 :name "org-archive-subtree"
 :description "Archive an org-mode subtree by heading name. The subtree is moved to the archive file configured for the buffer. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name to archive.")
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(defun eai-tool-library-org--todo (buffer heading-name state &optional index)
  "Set the TODO state of HEADING-NAME in BUFFER to STATE.

Optional INDEX (0-based) disambiguates when multiple headings match.
Navigates to the heading and calls `org-todo'.
Returns a success or error message."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (org-todo state)
        (format "Set TODO state of '%s' to %s in %s"
                heading-name (or state "nil") buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools-maybe-safe
 :function #'eai-tool-library-org--todo
 :name "org-todo"
 :description "Change the TODO state of an org-mode heading by name. Navigates to the heading and sets its TODO keyword. Pass nil or an empty string to remove the TODO state. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name to modify.")
             '(:name "state"
                     :type string
                     :description "The new TODO state (e.g. TODO, DONE, nil)."
                     :optional t)
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(defun eai-tool-library-org--set-property (buffer heading-name property value &optional index)
  "Set PROPERTY to VALUE for HEADING-NAME in BUFFER.

Optional INDEX (0-based) disambiguates when multiple headings match.
Navigates to the heading and sets the property via `org-set-property'.
Returns a success or error message."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (org-set-property property value)
        (format "Set property '%s' to '%s' on heading '%s' in %s"
                property value heading-name buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools-maybe-safe
 :function #'eai-tool-library-org--set-property
 :name "org-set-property"
 :description "Set a property on an org-mode heading by name. Navigates to the heading and sets the property. If the property already exists it is updated. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name to modify.")
             '(:name "property"
                     :type string
                     :description "The property name to set.")
             '(:name "value"
                     :type string
                     :description "The value to set the property to.")
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(defun eai-tool-library-org--get-property (buffer heading-name property &optional index)
  "Return the value of PROPERTY for HEADING-NAME in BUFFER.

Optional INDEX (0-based) disambiguates when multiple headings match.
Navigates to the heading and reads the property via `org-entry-get'.
Returns the property value or a message if not found."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (let ((val (org-entry-get nil property)))
          (or val
              (format "Property '%s' not found on heading '%s'" property heading-name)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools
 :function #'eai-tool-library-org--get-property
 :name "org-get-property"
 :description "Get a property value from an org-mode heading by name. Returns the property value, or a message if the heading or property is not found. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name to read from.")
             '(:name "property"
                     :type string
                     :description "The property name to read.")
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(defun eai-tool-library-org--read-subtree (buffer heading-name &optional index)
  "Read the subtree at HEADING-NAME in BUFFER.

Optional INDEX (0-based) disambiguates when multiple headings match.
Uses org's own subtree boundary detection (org-back-to-heading +
org-end-of-subtree) rather than outline :to positions, which may
not correctly represent subtree boundaries for nested headings.
Returns the subtree contents as a string."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (save-excursion
          (org-back-to-heading)
          (let ((from (point))
                (to (org-end-of-subtree t t)))
            (buffer-substring-no-properties from to)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools
 :function #'eai-tool-library-org--read-subtree
 :name "org-read-subtree"
 :description "Read the contents of an org-mode subtree by heading name. Uses org's own subtree boundary detection for accuracy with nested headings. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name of the subtree to read.")
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(defun eai-tool-library-org--replace-subtree (buffer heading-name new-string &optional index)
  "Replace the subtree at HEADING-NAME in BUFFER with NEW-STRING.

Optional INDEX (0-based) disambiguates when multiple headings match.
Uses org's own subtree boundary detection for accurate replacement
of nested subtrees.  The heading line itself is preserved; only
the subtree body is replaced.  If NEW-STRING starts with a
heading (e.g. '* Heading text'), the entire subtree including
the heading is replaced."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s is not in org-mode" buffer))
    (let ((resolved (eai-tool-library-org--resolve-heading buffer heading-name index)))
      (if (cdr resolved)
          (cdr resolved)
        (save-excursion
          (org-back-to-heading)
          (let ((from (point))
                (to (org-end-of-subtree t t)))
            ;; If new-string starts with a heading, replace the whole subtree
            ;; including the heading line.  Otherwise preserve the heading.
            (if (string-match-p "^\\*+ " new-string)
                (progn
                  (delete-region from to)
                  (goto-char from)
                  (insert new-string))
              (forward-line 1)
              (let ((body-from (point)))
                (delete-region body-from to)
                (goto-char body-from)
                (insert new-string)))
            (format "Replaced subtree '%s' in %s" heading-name buffer)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-org-tools-maybe-safe
 :function #'eai-tool-library-org--replace-subtree
 :name "org-replace-subtree"
 :description "Replace the contents of an org-mode subtree by heading name. Uses org's own subtree boundary detection for accuracy with nested headings. If the replacement text starts with a heading line (e.g. '* New heading'), the entire subtree including the heading is replaced. Otherwise only the subtree body is replaced and the heading is preserved. If multiple headings match, an error lists all matches with 0-based indexes so you can retry with the index argument. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer containing the org-mode heading.")
             '(:name "heading-name"
                     :type string
                     :description "The exact heading name of the subtree to replace.")
             '(:name "new-string"
                     :type string
                     :description "The new text to replace the subtree with.")
             '(:name "index"
                     :type integer
                     :description "Optional 0-based index to disambiguate when multiple headings match."
                     :optional t))
 :category "emacs-org")

(provide 'eai-tool-library-org)

;;; eai-tool-library-org.el ends here
