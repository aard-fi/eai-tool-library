;;; test-elisp.el --- Tests for eai-tool-library-elisp -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-elisp module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-elisp)

(defconst etl-elisp--sample-defun
  "(defun etl-elisp--test-fn (x y)\n  \"Sample test function.\"\n  (+ x y))\n"
  "A sample defun used across tests.")

(defconst etl-elisp--two-defuns
  (concat "(defun etl-elisp--fn-one ()\n  \"First function.\"\n  1)\n\n"
          "(defun etl-elisp--fn-two (z)\n  \"Second function.\"\n  (* z 2))\n")
  "Buffer content with two defuns.")

(defmacro etl-elisp--with-elisp-buffer (content &rest body)
  "Evaluate BODY with a temp buffer in emacs-lisp-mode containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,content)
     ,@body))

;;; eai-tool-library-elisp--run-eval

(ert-deftest etl-elisp/run-eval/simple-expression ()
  "Evaluates a simple elisp expression from a string."
  (should (= 42 (eai-tool-library-elisp--run-eval "(+ 20 22)"))))

(ert-deftest etl-elisp/run-eval/string-result ()
  "Returns string results correctly."
  (should (string= "hello" (eai-tool-library-elisp--run-eval "(concat \"hel\" \"lo\")"))))

(ert-deftest etl-elisp/run-eval/multiple-forms ()
  "Evaluates multiple forms, returning the last result."
  (should (= 10 (eai-tool-library-elisp--run-eval "(setq etl--eval-tmp 5) (* etl--eval-tmp 2)"))))

(ert-deftest etl-elisp/run-eval/list-result ()
  "Returns list results."
  (should (equal '(1 2 3) (eai-tool-library-elisp--run-eval "(list 1 2 3)"))))

;;; eai-tool-library-elisp--defun-region

(ert-deftest etl-elisp/defun-region/found ()
  "Returns a cons cell for a found function."
  (etl-elisp--with-elisp-buffer etl-elisp--sample-defun
    (let ((result (eai-tool-library-elisp--defun-region "etl-elisp--test-fn")))
      (should (consp result))
      (should (integerp (car result)))
      (should (integerp (cdr result)))
      (should (< (car result) (cdr result))))))

(ert-deftest etl-elisp/defun-region/starts-at-defun ()
  "The start of the region points to the beginning of the defun."
  (etl-elisp--with-elisp-buffer etl-elisp--sample-defun
    (let ((result (eai-tool-library-elisp--defun-region "etl-elisp--test-fn")))
      (should (= 1 (car result))))))

(ert-deftest etl-elisp/defun-region/region-contains-function ()
  "The region contains the complete defun text."
  (etl-elisp--with-elisp-buffer etl-elisp--sample-defun
    (let ((result (eai-tool-library-elisp--defun-region "etl-elisp--test-fn")))
      (let ((text (buffer-substring-no-properties (car result) (cdr result))))
        (should (string-match-p "defun" text))
        (should (string-match-p "etl-elisp--test-fn" text))))))

(ert-deftest etl-elisp/defun-region/not-found-returns-nil ()
  "Returns nil when the function is not found."
  (etl-elisp--with-elisp-buffer "(defun other-fn () 42)\n"
    (should (null (eai-tool-library-elisp--defun-region "nonexistent-fn-xyz")))))

(ert-deftest etl-elisp/defun-region/finds-second-defun ()
  "Can find the second of two defuns in a buffer."
  (etl-elisp--with-elisp-buffer etl-elisp--two-defuns
    (let ((result (eai-tool-library-elisp--defun-region "etl-elisp--fn-two")))
      (should (consp result))
      (let ((text (buffer-substring-no-properties (car result) (cdr result))))
        (should (string-match-p "fn-two" text))))))

(ert-deftest etl-elisp/defun-region/in-named-buffer ()
  "Works when specifying a buffer by name."
  (let ((buf (get-buffer-create "*etl-test-defun-region*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (emacs-lisp-mode)
            (insert etl-elisp--sample-defun))
          (let ((result (eai-tool-library-elisp--defun-region
                         "etl-elisp--test-fn" buf)))
            (should (consp result))))
      (kill-buffer buf))))

;;; eai-tool-library-elisp--replace-defun-region

(ert-deftest etl-elisp/replace-defun-region/basic ()
  "Replaces the target defun with new text."
  (etl-elisp--with-elisp-buffer etl-elisp--sample-defun
    (eai-tool-library-elisp--replace-defun-region
     "etl-elisp--test-fn"
     "(defun etl-elisp--test-fn (x y)\n  \"Replaced.\"\n  (- x y))\n")
    (should (string-match-p "Replaced" (buffer-string)))
    (should (string-match-p "(- x y)" (buffer-string)))))

(ert-deftest etl-elisp/replace-defun-region/not-found-is-silent ()
  "Does not error when target function is not found."
  (etl-elisp--with-elisp-buffer etl-elisp--sample-defun
    (eai-tool-library-elisp--replace-defun-region
     "nonexistent-fn-xyz"
     "(defun nonexistent-fn-xyz () nil)")
    ;; original content unchanged
    (should (string-match-p "etl-elisp--test-fn" (buffer-string)))))

;;; eai-tool-library-elisp-variable-doc

(ert-deftest etl-elisp/variable-doc/known-variable ()
  "Returns documentation string for a known variable."
  (let ((result (eai-tool-library-elisp-variable-doc "eai-tool-library-debug")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest etl-elisp/variable-doc/unknown-variable ()
  "Returns a no-documentation message for an unknown variable."
  (let ((result (eai-tool-library-elisp-variable-doc "etl-no-such-var-xyz-999")))
    (should (stringp result))
    (should (string-match-p "No documentation" result))))

;;; eai-tool-library-elisp-function-doc

(ert-deftest etl-elisp/function-doc/known-function ()
  "Returns documentation for a known built-in function."
  (let ((result (eai-tool-library-elisp-function-doc "car")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest etl-elisp/function-doc/unknown-function ()
  "Returns a no-documentation message for an unknown function."
  (let ((result (eai-tool-library-elisp-function-doc "etl-no-such-fn-xyz-999")))
    (should (stringp result))
    (should (string-match-p "No documentation" result))))

(ert-deftest etl-elisp/function-doc/eai-function-with-doc ()
  "Returns documentation for an eai function that has a docstring."
  (let ((result (eai-tool-library-elisp-function-doc "eai-tool-library--debug-log")))
    (should (stringp result))
    (should (string-match-p "debug\\|log" (downcase result)))))

(ert-deftest etl-elisp/function-doc/function-without-docstring ()
  "Returns nil for a bound function that has no docstring."
  (let ((result (eai-tool-library-elisp-function-doc "eai-tool-library--limit-result")))
    (should (null result))))

;;; eai-tool-library-elisp-describe-symbol

(ert-deftest etl-elisp/describe-symbol/function ()
  "Returns a non-empty string for a known function symbol."
  (let ((result (eai-tool-library-elisp-describe-symbol "car")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest etl-elisp/describe-symbol/variable ()
  "Returns a non-empty string for a known variable symbol."
  (let ((result (eai-tool-library-elisp-describe-symbol "load-path")))
    (should (stringp result))
    (should (> (length result) 0))))

;;; eai-tool-library-elisp-describe-symbol-fuzzy

(ert-deftest etl-elisp/describe-symbol-fuzzy/returns-callable ()
  "Returns a callable lambda (the actual search is in the returned closure)."
  (let ((result (eai-tool-library-elisp-describe-symbol-fuzzy "anything")))
    (should (functionp result))))

(ert-deftest etl-elisp/describe-symbol-fuzzy/lambda-returns-matches ()
  "The returned lambda finds matching symbols."
  (let* ((searcher (eai-tool-library-elisp-describe-symbol-fuzzy "ignored"))
         (result (funcall searcher "eai-tool-library")))
    (should (stringp result))
    (should (string-match-p "eai-tool-library" result))))

(ert-deftest etl-elisp/describe-symbol-fuzzy/lambda-no-match ()
  "The returned lambda returns a no-match message for unknown patterns."
  (let* ((searcher (eai-tool-library-elisp-describe-symbol-fuzzy "ignored"))
         (result (funcall searcher "etl-no-match-symbol-zxqv9999")))
    (should (stringp result))
    (should (string-match-p "No matches" result))))

(ert-deftest etl-elisp/describe-symbol-fuzzy/lambda-respects-limit ()
  "The returned lambda limits results when a limit is given."
  (let* ((searcher (eai-tool-library-elisp-describe-symbol-fuzzy "ignored"))
         (unlimited (funcall searcher "car"))
         (limited (funcall searcher "car" 2))
         (count-lines (lambda (s) (length (split-string s "\n" t)))))
    (should (<= (funcall count-lines limited) (funcall count-lines unlimited)))))

(provide 'test-elisp)
;;; test-elisp.el ends here
