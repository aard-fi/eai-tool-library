;;; eai-tool-library-outline.el --- Outline access -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;; Keywords: tools
;;
;;; Commentary:
;;
;; This module contains tools for retrieving and editing data based on
;; outlines. If available this will work with treesitter, with a fallback
;; to imenu offering the same functionality. If that's not available there's
;; some very limited regexp fallback.
;;
;;; Code:

(require 'eai-tool-library)

(require 'imenu)
(require 'treesit)

(defvar eai-tool-library-outline-tools '()
  "The list of buffer related tools")

(defvar eai-tool-library-outline-tools-maybe-safe '()
  "The list of buffer related tools which may be destructive, but typically
the LLM behaves.")

(defvar eai-tool-library-outline-category-name "emacs-outline"
  "The buffer category used for tool registration")

;;; Outline / structure navigation

(defvar eai-tool-library-outline--ts-node-types
  '((emacs-lisp-mode . ("function_definition" "list"))
    (python-ts-mode . ("function_definition" "class_definition" "decorated_definition"))
    (python-mode . ("function_definition" "class_definition" "decorated_definition"))
    (c-ts-mode . ("function_definition" "struct_specifier" "enum_specator" "class_specifier"))
    (c-mode . ("function_definition" "struct_specifier" "enum_specator" "class_specifier"))
    (rust-ts-mode . ("function_item" "impl_item" "struct_item" "enum_item" "trait_item"))
    (java-ts-mode . ("class_declaration" "method_declaration" "interface_declaration" "enum_declaration"))
    (json-ts-mode . ("object" "array"))
    (yaml-ts-mode . ("block_mapping" "block_sequence"))
    (html-ts-mode . ("element" "script_element" "style_element"))
    (nix-ts-mode . ("attrset_expression" "function_expression"))
    (toml-ts-mode . ("table" "array_table"))
    (css-ts-mode . ("rule_set" "media_statement"))
    (typescript-ts-mode . ("function_declaration" "class_declaration" "method_definition" "interface_declaration"))
    (js-ts-mode . ("function_declaration" "class_declaration" "method_definition")))
  "Mapping from major mode to tree-sitter node types considered outline items.
Types listed here will be walked recursively to produce an outline.")

(defun eai-tool-library-outline--section-separator (buffer)
  "Return mode-appropriate separator for inserting between sections in BUFFER.

This is just a quick and dirty attempt at preventing code to merge, and needs to
be refined later on."
  (with-current-buffer (get-buffer-create buffer)
    (let ((mode major-mode))
      (cond
       ((memq mode '(emacs-lisp-mode lisp-interaction-mode lisp-mode)) "\n\n")
       ((memq mode '(python-ts-mode python-mode)) "\n\n\n")
       ((memq mode '(org-mode)) "\n")
       (t "\n\n\n")))))

(defun eai-tool-library-outline--ts (buffer)
  "Walk tree-sitter nodes in BUFFER to produce outline entries.

Uses `eai-tool-library-outline--ts-node-types' to determine
which node types to descend into and report.  Returns a list of
plists with :name, :type, :from, and :to keys."
  (with-current-buffer buffer
    (let* ((modes (list major-mode))
           (mode (car modes))
           (types (or (alist-get mode eai-tool-library-outline--ts-node-types)
                      (when (string-suffix-p "-ts-mode" (symbol-name mode))
                        (alist-get (intern (string-remove-suffix "-ts-mode" (symbol-name mode)))
                                   eai-tool-library-outline--ts-node-types))))
           (root (treesit-buffer-root-node))
           (result nil))
      (when (and root types)
        (let ((stack (list root)))
          (while stack
            (let* ((node (pop stack))
                   (node-type (treesit-node-type node)))
              (cond
               ((member node-type types)
                ;; This is an outline node — record it and descend
                ;; into its children for nested items
                (let* ((name-node (or (treesit-node-child-by-field-name node "name")
                                      (treesit-node-child node 0 t)))
                       (name (when name-node
                               (treesit-node-text name-node t))))
                  (push (list :name (or name node-type)
                              :type node-type
                              :from (treesit-node-start node)
                              :to (treesit-node-end node))
                        result)
                  (dolist (child (treesit-node-children node t))
                    (push child stack))))
               ;; Not an outline type but could contain outline items
               ;; at deeper nesting levels
               (t
                (dolist (child (treesit-node-children node t))
                  (push child stack))))))))
      (nreverse result))))

(defun eai-tool-library-outline--imenu-walk (items prefix result)
  "Recursively walk imenu ITEMS, prepending PREFIX to names, appending to RESULT.
Return the final RESULT list (in reverse order, to be nreversed by caller)."
  (dolist (item items)
    (cond
     ((imenu--subalist-p item)
      (setq result
            (eai-tool-library-outline--imenu-walk
             (cdr item)
             (if prefix
                 (concat prefix "/" (car item))
               (car item))
             result)))
     (t
      (let ((name (car item))
            (pos (if (overlayp (cdr item))
                     (overlay-start (cdr item))
                   (marker-position (cdr item)))))
        (push (list :name (if prefix (concat prefix "/" name) name)
                    :type "imenu"
                    :from pos
                    :to pos)
              result)))))
  result)

(defun eai-tool-library-outline--imenu (buffer)
  "Use imenu to produce outline entries for BUFFER.

Returns a flat list of plists with :name, :type, :from, and :to keys.
Handles nested imenu entries by flattening them with hierarchical names.
The :to value for each entry is derived by using the :from of the next
entry, or the buffer end for the last entry.  Trailing whitespace is
stripped from :to boundaries for precision.

Respects the major-mode-specific `imenu-create-index-function' when
available (e.g., `org-imenu-get-tree' for org-mode), falling back to
`imenu-default-create-index-function' otherwise."
  (with-current-buffer buffer
    (let* ((index-fn (or imenu-create-index-function
                         #'imenu-default-create-index-function))
           (index (funcall index-fn))
           (result nil))
      (setq result (eai-tool-library-outline--imenu-walk index nil result))
      ;; result is in reverse push order.  Sort ascending by :from.
      (setq result (sort result
                         (lambda (a b) (< (plist-get a :from)
                                          (plist-get b :from)))))
      ;; Fill in :to values by building a new list (avoid mutation issues).
      ;; Walk backwards: each entry's :to is the trimmed position before
      ;; the next entry, or point-max for the last entry.
      (let ((next-from (point-max))
            (new-result nil))
        (dolist (entry (nreverse result))
          (let ((to-pos (save-excursion
                          (goto-char next-from)
                          (skip-chars-backward " \t\n\r")
                          (point))))
            (push (list :name (plist-get entry :name)
                        :type (plist-get entry :type)
                        :from (plist-get entry :from)
                        :to to-pos)
                  new-result)
            (setq next-from (plist-get entry :from))))
        new-result))))

(defun eai-tool-library-outline--regex (buffer)
  "Use outline-regexp and regex matching to produce outline entries for BUFFER.

This is the lowest-quality fallback, used when neither tree-sitter
nor imenu has useful data.  Returns a flat list of plists."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((result nil)
            (re (or outline-regexp
                    ;; Generic fallback: common definition patterns
                    (concat "^\\("
                            (mapconcat #'identity
                                       '("defun" "defmacro" "defvar" "defconst"
                                         "class " "def " "function " "async function"
                                         "pub fn" "fn " "impl " "struct " "enum "
                                         "interface " "type " "const "
                                         "#+\\*+" "#+\\." "^@@")
                                       "\\|")
                            "\\)"))))
        (while (re-search-forward re nil t)
          (let ((beg (line-beginning-position))
                (end (line-end-position)))
            (push (list :name (string-trim (buffer-substring-no-properties beg end))
                        :type "outline"
                        :from beg
                        :to end)
                  result)))
        (nreverse result)))))

(defun eai-tool-library-outline--get (&optional buffer)
  "Return a structural outline of BUFFER as a list of plists.

Each plist has :name, :type, :from, and :to keys.  Uses tree-sitter
when available, falls back to imenu, then to outline-regexp matching."
  (let* ((buffer (or buffer (current-buffer)))
         (buffer (if (stringp buffer)
                     (eai-tool-library--get-buffer buffer)
                   buffer))
         (result nil))
    ;; Try tree-sitter first
    (with-current-buffer buffer
      (when (and (fboundp #'treesit-available-p)
                 (treesit-available-p)
                 (alist-get major-mode eai-tool-library-outline--ts-node-types))
        (condition-case nil
            (when (treesit-buffer-root-node)
              (setq result (eai-tool-library-outline--ts buffer)))
          (treesit-no-parser nil))))
    ;; Fallback to imenu
    (unless result
      (with-current-buffer buffer
        (condition-case nil
            (when (or imenu-create-index-function
                      imenu-generic-expression
                      (fboundp #'imenu-default-create-index-function))
              (setq result (eai-tool-library-outline--imenu buffer)))
          (imenu-unavailable nil))))
    ;; Last fallback: regex
    (unless result
      (setq result (eai-tool-library-outline--regex buffer)))
    result))

(defun eai-tool-library-outline--replace-section (buffer section-name new-string)
  "Replace the outline section named SECTION-NAME in BUFFER with NEW-STRING.

Uses `eai-tool-library-outline-' to locate the section by exact name
match, then replaces the region (:from to :to) with NEW-STRING.
Returns a message indicating success or failure."
  (let* ((buf (get-buffer-create (or buffer (current-buffer))))
         (outline (eai-tool-library-outline--get buf))
         (match (seq-find (lambda (entry)
                            (string= (plist-get entry :name) section-name))
                          outline)))
    (if (not match)
        (format "Section '%s' not found in buffer %s" section-name buffer)
      (let ((from (plist-get match :from))
            (to (plist-get match :to)))
        (with-current-buffer buf
          (delete-region from to)
          (goto-char from)
          (insert new-string))
        (format "Replaced section '%s' (%d-%d) in %s" section-name from to buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-outline-tools-maybe-safe
 :function #'eai-tool-library-outline--replace-section
 :name "outline-replace-section"
 :description "Replace a named outline section in a buffer with new text. Uses the buffer outline to locate the section by exact name match, then replaces the entire section region. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to perform the replacement in.")
             '(:name "section-name"
                     :type string
                     :description "The exact name of the section to find and replace.")
             '(:name "new-string"
                     :type string
                     :description "The new text to replace the section with."))
 :category "emacs-outline")

(eai-tool-library-make-tools-and-register
 'eai-tool-library-outline-tools
 :function #'eai-tool-library-outline--get
 :name "outline-get"
 :description "Return a structural outline of a buffer as a list of named sections with their byte positions. Uses tree-sitter when available, falls back to imenu, then to outline-regexp matching. Each entry has :name, :type, :from, and :to keys. Useful for navigating large buffers before reading or editing specific regions."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to get the outline of. Uses the current buffer when omitted."
                     :optional t))
 :category "emacs-outline")

(defun eai-tool-library-outline--read-section (buffer section-name)
  "Read the contents of outline section named SECTION-NAME in BUFFER.

Uses `eai-tool-library-outline-' to locate the section by exact name
match, then returns the region contents.  Returns an error message if
the section is not found."
  (let* ((buf (get-buffer-create (or buffer (current-buffer))))
         (outline (eai-tool-library-outline--get buf))
         (match (seq-find (lambda (entry)
                            (string= (plist-get entry :name) section-name))
                          outline)))
    (if (not match)
        (format "Section '%s' not found in buffer %s" section-name buffer)
      (let ((from (plist-get match :from))
            (to (plist-get match :to)))
        (with-current-buffer buf
          (concat (buffer-substring-no-properties from to)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-outline-tools
 :function #'eai-tool-library-outline--read-section
 :name "outline-read-section"
 :description "Read a single named outline section from a buffer. Uses the buffer outline to locate the section by exact name match, then returns its contents. Useful for large buffers where reading the whole thing is wasteful. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to read the section from.")
             '(:name "section-name"
                     :type string
                     :description "The exact name of the section to read."))
 :category "emacs-outline")

;;; inserting

(defun eai-tool-library-outline--insert-before (buffer section-name new-string)
  "Insert NEW-STRING before the outline section named SECTION-NAME in BUFFER.

Uses `eai-tool-library-outline-' to locate the section by exact name match,
then inserts the new text before the section's :from position.  Mode-appropriate
whitespace is added automatically around the inserted text."
  (let* ((buf (get-buffer-create (or buffer (current-buffer))))
         (outline (eai-tool-library-outline--get buf))
         (match (seq-find (lambda (entry)
                            (string= (plist-get entry :name) section-name))
                          outline)))
    (if (not match)
        (format "Section '%s' not found in buffer %s" section-name buffer)
      (let ((from (plist-get match :from))
            (sep (eai-tool-library-outline--section-separator buf)))
        (with-current-buffer buf
          (goto-char from)
          (insert (concat sep new-string sep)))
        (format "Inserted new section before '%s' at position %d in %s" section-name from buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-outline-tools-maybe-safe
 :function #'eai-tool-library-outline--insert-before
 :name "outline-insert-before"
 :description "Insert new text before a named outline section in a buffer. Uses the buffer outline to locate the section by exact name match, then inserts the new text before that section's start position. Mode-appropriate whitespace is automatically added around the inserted text. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to insert text into.")
             '(:name "section-name"
                     :type string
                     :description "The exact name of the existing section to insert before.")
             '(:name "new-string"
                     :type string
                     :description "The new text to insert."))
 :category "emacs-outline")

(defun eai-tool-library-outline--insert-after (buffer section-name new-string)
  "Insert NEW-STRING after the outline section named SECTION-NAME in BUFFER.

Uses `eai-tool-library-outline-' to locate the section by exact name match,
then inserts the new text after the section's :to position.  Mode-appropriate
whitespace is added automatically around the inserted text."
  (let* ((buf (get-buffer-create (or buffer (current-buffer))))
         (outline (eai-tool-library-outline--get buf))
         (match (seq-find (lambda (entry)
                            (string= (plist-get entry :name) section-name))
                          outline)))
    (if (not match)
        (format "Section '%s' not found in buffer %s" section-name buffer)
      (let ((to (plist-get match :to))
            (sep (eai-tool-library-outline--section-separator buf)))
        (with-current-buffer buf
          (goto-char to)
          (insert (concat sep new-string sep)))
        (format "Inserted new section after '%s' at position %d in %s" section-name to buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-outline-tools-maybe-safe
 :function #'eai-tool-library-outline--insert-after
 :name "outline-insert-after"
 :description "Insert new text after a named outline section in a buffer. Uses the buffer outline to locate the section by exact name match, then inserts the new text after that section's end position. Mode-appropriate whitespace is automatically added around the inserted text. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to insert text into.")
             '(:name "section-name"
                     :type string
                     :description "The exact name of the existing section to insert after.")
             '(:name "new-string"
                     :type string
                     :description "The new text to insert."))
 :category "emacs-outline")

(provide 'eai-tool-library-outline)

;;; eai-tool-library-outline.el ends here
