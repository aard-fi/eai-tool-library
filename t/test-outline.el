;;; test-outline.el --- Tests for eai-tool-library-outline -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-outline module.
;;
;;; Code:

(require 'test-init)
(require 'outline)
(require 'eai-tool-library-outline)

(defconst etl-outline--two-defuns
  (concat "(defun etl-outline-fn-alpha ()\n"
          "  \"Alpha function.\"\n"
          "  1)\n\n"
          "(defun etl-outline-fn-beta (x)\n"
          "  \"Beta function.\"\n"
          "  (* x 2))\n")
  "Buffer content with two named defuns for outline tests.")

(defmacro etl-outline--with-elisp-buffer (name content &rest body)
  "Evaluate BODY with an emacs-lisp-mode buffer NAME containing CONTENT."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (erase-buffer)
             (emacs-lisp-mode)
             (insert ,content))
           ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

;;; eai-tool-library-outline--section-separator

(ert-deftest etl-outline/section-separator/emacs-lisp-mode ()
  "Returns double newline for emacs-lisp-mode."
  (let ((buf (get-buffer-create "*etl-test-sep-el*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (emacs-lisp-mode))
          (should (string= "\n\n"
                           (eai-tool-library-outline--section-separator
                            (buffer-name buf)))))
      (kill-buffer buf))))

(ert-deftest etl-outline/section-separator/python-mode ()
  "Returns triple newline for python-mode (if available)."
  (skip-unless (fboundp 'python-mode))
  (let ((buf (get-buffer-create "*etl-test-sep-py*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (python-mode))
          (should (string= "\n\n\n"
                           (eai-tool-library-outline--section-separator
                            (buffer-name buf)))))
      (kill-buffer buf))))

(ert-deftest etl-outline/section-separator/org-mode ()
  "Returns single newline for org-mode."
  (skip-unless (fboundp 'org-mode))
  (let ((buf (get-buffer-create "*etl-test-sep-org*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (org-mode))
          (should (string= "\n"
                           (eai-tool-library-outline--section-separator
                            (buffer-name buf)))))
      (kill-buffer buf))))

(ert-deftest etl-outline/section-separator/fundamental-mode ()
  "Returns triple newline for fundamental-mode (default)."
  (let ((buf (get-buffer-create "*etl-test-sep-fund*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (fundamental-mode))
          (should (string= "\n\n\n"
                           (eai-tool-library-outline--section-separator
                            (buffer-name buf)))))
      (kill-buffer buf))))

;;; eai-tool-library-outline--imenu-walk

(ert-deftest etl-outline/imenu-walk/flat-items ()
  "Processes a flat list of imenu items into outline entries."
  (let ((buf (get-buffer-create "*etl-test-walk*")))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello world")
          (let* ((m1 (set-marker (make-marker) 1 buf))
                 (m2 (set-marker (make-marker) 7 buf))
                 (items (list (cons "item-one" m1)
                              (cons "item-two" m2)))
                 (result (eai-tool-library-outline--imenu-walk items nil '())))
            (should (= 2 (length result)))
            (let ((names (mapcar (lambda (e) (plist-get e :name)) result)))
              (should (member "item-one" names))
              (should (member "item-two" names)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest etl-outline/imenu-walk/items-have-required-keys ()
  "Each outline entry has :name, :type, :from, and :to keys."
  (let ((buf (get-buffer-create "*etl-test-walk-keys*")))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "content")
          (let* ((m (set-marker (make-marker) 1 buf))
                 (items (list (cons "myfn" m)))
                 (result (eai-tool-library-outline--imenu-walk items nil '()))
                 (entry (car result)))
            (should entry)
            (should (plist-member entry :name))
            (should (plist-member entry :type))
            (should (plist-member entry :from))
            (should (plist-member entry :to))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest etl-outline/imenu-walk/with-prefix ()
  "Prepends prefix to item names."
  (let ((buf (get-buffer-create "*etl-test-walk-prefix*")))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "content")
          (let* ((m (set-marker (make-marker) 1 buf))
                 (items (list (cons "myfn" m)))
                 (result (eai-tool-library-outline--imenu-walk items "Category" '()))
                 (entry (car result)))
            (should (string= "Category/myfn" (plist-get entry :name)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest etl-outline/imenu-walk/empty-items ()
  "Returns the original result for an empty items list."
  (let ((result (eai-tool-library-outline--imenu-walk '() nil '(a b c))))
    (should (equal '(a b c) result))))

;;; eai-tool-library-outline--imenu

(ert-deftest etl-outline/imenu/emacs-lisp-returns-list ()
  "Returns a list for an emacs-lisp-mode buffer."
  (etl-outline--with-elisp-buffer "*etl-test-imenu*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--imenu buf)))
      (should (listp result)))))

(ert-deftest etl-outline/imenu/finds-defuns ()
  "The imenu index for an elisp buffer contains the defined functions."
  (etl-outline--with-elisp-buffer "*etl-test-imenu-defuns*" etl-outline--two-defuns
    (let* ((result (eai-tool-library-outline--imenu buf))
           (names (mapcar (lambda (e) (plist-get e :name)) result))
           (all-names (mapconcat #'identity names " ")))
      (should (string-match-p "alpha" all-names))
      (should (string-match-p "beta" all-names)))))

(ert-deftest etl-outline/imenu/entries-have-positions ()
  "Each imenu entry has numeric :from and :to positions."
  (etl-outline--with-elisp-buffer "*etl-test-imenu-pos*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--imenu buf)))
      (dolist (entry result)
        (should (integerp (plist-get entry :from)))
        (should (integerp (plist-get entry :to)))))))

;;; eai-tool-library-outline--regex

(ert-deftest etl-outline/regex/finds-defun-lines ()
  "Regex fallback finds lines starting with known keywords when outline-regexp is nil."
  (with-temp-buffer
    (fundamental-mode)
    ;; Set outline-regexp to nil to force the generic fallback pattern
    (setq-local outline-regexp nil)
    (insert "defun foo-function\n  body\n\ndefvar my-variable 42\n")
    (let ((result (eai-tool-library-outline--regex (current-buffer))))
      (should (listp result))
      (should (> (length result) 0))
      (let ((names (mapcar (lambda (e) (plist-get e :name)) result)))
        (should (cl-some (lambda (n) (string-match-p "defun" n)) names))))))

(ert-deftest etl-outline/regex/entries-have-required-keys ()
  "Each regex-derived entry has :name, :type, :from, and :to keys."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local outline-regexp nil)
    (insert "defun test-fn\n")
    (let* ((result (eai-tool-library-outline--regex (current-buffer)))
           (entry (car result)))
      (when entry
        (should (plist-member entry :name))
        (should (plist-member entry :type))
        (should (plist-member entry :from))
        (should (plist-member entry :to))))))

(ert-deftest etl-outline/regex/no-match-returns-empty ()
  "Returns empty list for a buffer with no matching lines."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local outline-regexp nil)
    (insert "this line has no known keywords at the start\n")
    (let ((result (eai-tool-library-outline--regex (current-buffer))))
      (should (listp result))
      (should (= 0 (length result))))))

;;; eai-tool-library-outline--get

(ert-deftest etl-outline/get/returns-list ()
  "Returns a list (possibly empty) for any buffer."
  (etl-outline--with-elisp-buffer "*etl-test-get*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--get buf)))
      (should (listp result)))))

(ert-deftest etl-outline/get/finds-sections ()
  "Returns outline entries for a buffer with known structure."
  (etl-outline--with-elisp-buffer "*etl-test-get-sections*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--get buf)))
      (should (> (length result) 0)))))

(ert-deftest etl-outline/get/accepts-buffer-object ()
  "Accepts a buffer object as argument."
  (etl-outline--with-elisp-buffer "*etl-test-get-obj*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--get buf)))
      (should (listp result)))))

(ert-deftest etl-outline/get/current-buffer-when-nil ()
  "Uses the current buffer when argument is nil."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert etl-outline--two-defuns)
    (let ((result (eai-tool-library-outline--get)))
      (should (listp result)))))

;;; eai-tool-library-outline--read-section

(ert-deftest etl-outline/read-section/found ()
  "Returns the content of a found section."
  (etl-outline--with-elisp-buffer "*etl-test-read-sec*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let* ((section-name (plist-get first :name))
               (result (eai-tool-library-outline--read-section
                        (buffer-name buf) section-name)))
          (should (stringp result))
          (should (> (length result) 0)))))))

(ert-deftest etl-outline/read-section/not-found ()
  "Returns an error message when the section is not found."
  (etl-outline--with-elisp-buffer "*etl-test-read-sec-nf*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--read-section
                   (buffer-name buf) "nonexistent-section-xyz-999")))
      (should (stringp result))
      (should (string-match-p "not found" result)))))

(ert-deftest etl-outline/read-section/content-matches-buffer ()
  "The section content matches what is in the buffer."
  (etl-outline--with-elisp-buffer "*etl-test-read-content*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let* ((section-name (plist-get first :name))
               (result (eai-tool-library-outline--read-section
                        (buffer-name buf) section-name)))
          (with-current-buffer buf
            (let ((buf-text (buffer-substring-no-properties
                             (plist-get first :from)
                             (plist-get first :to))))
              (should (string= buf-text result)))))))))

;;; eai-tool-library-outline--replace-section

(ert-deftest etl-outline/replace-section/found ()
  "Replaces the content of a found section."
  (etl-outline--with-elisp-buffer "*etl-test-replace-sec*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let ((section-name (plist-get first :name)))
          (eai-tool-library-outline--replace-section
           (buffer-name buf) section-name "(defun etl-outline-fn-alpha () 999)\n")
          (with-current-buffer buf
            (should (string-match-p "999" (buffer-string)))))))))

(ert-deftest etl-outline/replace-section/not-found ()
  "Returns an error message when the section is not found."
  (etl-outline--with-elisp-buffer "*etl-test-replace-sec-nf*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--replace-section
                   (buffer-name buf) "nonexistent-xyz-999" "new text")))
      (should (stringp result))
      (should (string-match-p "not found" result)))))

(ert-deftest etl-outline/replace-section/returns-success-message ()
  "Returns a success message string when replacement succeeds."
  (etl-outline--with-elisp-buffer "*etl-test-replace-msg*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let* ((section-name (plist-get first :name))
               (result (eai-tool-library-outline--replace-section
                        (buffer-name buf) section-name "replacement")))
          (should (stringp result))
          (should (string-match-p "Replaced" result)))))))

;;; eai-tool-library-outline--insert-before

(ert-deftest etl-outline/insert-before/found ()
  "Inserts text before the named section."
  (etl-outline--with-elisp-buffer "*etl-test-insert-before*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let ((section-name (plist-get first :name))
              (fn-name (plist-get first :name)))
          (eai-tool-library-outline--insert-before
           (buffer-name buf) section-name ";; inserted-before-marker\n")
          (with-current-buffer buf
            (should (string-match-p "inserted-before-marker" (buffer-string)))
            ;; The inserted marker should appear before the function definition in text
            (let* ((content (buffer-string))
                   (marker-pos (string-search "inserted-before-marker" content))
                   (fn-pos (string-search "etl-outline-fn-alpha" content)))
              (when (and marker-pos fn-pos)
                (should (< marker-pos fn-pos))))))))))

(ert-deftest etl-outline/insert-before/not-found ()
  "Returns an error message when the section is not found."
  (etl-outline--with-elisp-buffer "*etl-test-insert-before-nf*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--insert-before
                   (buffer-name buf) "nonexistent-xyz-999" "text")))
      (should (stringp result))
      (should (string-match-p "not found" result)))))

;;; eai-tool-library-outline--insert-after

(ert-deftest etl-outline/insert-after/found ()
  "Inserts text after the named section."
  (etl-outline--with-elisp-buffer "*etl-test-insert-after*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let ((section-name (plist-get first :name)))
          (eai-tool-library-outline--insert-after
           (buffer-name buf) section-name ";; inserted-after-marker\n")
          (with-current-buffer buf
            (should (string-match-p "inserted-after-marker" (buffer-string)))))))))

(ert-deftest etl-outline/insert-after/not-found ()
  "Returns an error message when the section is not found."
  (etl-outline--with-elisp-buffer "*etl-test-insert-after-nf*" etl-outline--two-defuns
    (let ((result (eai-tool-library-outline--insert-after
                   (buffer-name buf) "nonexistent-xyz-999" "text")))
      (should (stringp result))
      (should (string-match-p "not found" result)))))

(ert-deftest etl-outline/insert-after/returns-success-message ()
  "Returns a success message string when insertion succeeds."
  (etl-outline--with-elisp-buffer "*etl-test-insert-after-msg*" etl-outline--two-defuns
    (let* ((outline (eai-tool-library-outline--get buf))
           (first (car outline)))
      (when first
        (let* ((section-name (plist-get first :name))
               (result (eai-tool-library-outline--insert-after
                        (buffer-name buf) section-name "new text")))
          (should (stringp result))
          (should (string-match-p "Inserted" result)))))))

(provide 'test-outline)
;;; test-outline.el ends here
