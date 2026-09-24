;;; test-emacs.el --- Tests for eai-tool-library-emacs -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-emacs module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-emacs)

;;; eai-tool-library-emacs--describe-variable

(ert-deftest etl-emacs/describe-variable/bound-variable ()
  "Returns a non-empty string for a bound variable."
  (let ((result (eai-tool-library-emacs--describe-variable "load-path")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest etl-emacs/describe-variable/emacs-version ()
  "Returns a string representation for emacs-version."
  (let ((result (eai-tool-library-emacs--describe-variable "emacs-version")))
    (should (stringp result))
    ;; The result is prin1-to-string of emacs-version string
    (should (string-match-p "[0-9]" result))))

(ert-deftest etl-emacs/describe-variable/eai-variable ()
  "Returns the value for an eai-tool-library variable."
  (let ((result (eai-tool-library-emacs--describe-variable
                 "eai-tool-library-max-result-size")))
    (should (stringp result))
    (should (string-match-p "[0-9]" result))))

(ert-deftest etl-emacs/describe-variable/unbound-variable ()
  "Returns an error message for an unbound variable."
  (let ((result (eai-tool-library-emacs--describe-variable
                 "etl-no-such-variable-xyz-9999")))
    (should (stringp result))
    (should (string-match-p "not bound" result))))

(ert-deftest etl-emacs/describe-variable/unbound-contains-variable-name ()
  "The error message for an unbound variable includes the variable name."
  (let* ((varname "etl-no-such-variable-abc-1234")
         (result (eai-tool-library-emacs--describe-variable varname)))
    (should (string-match-p varname result))))

;;; eai-tool-library-emacs--describe-function

(ert-deftest etl-emacs/describe-function/known-builtin ()
  "Returns documentation for a built-in function."
  (let ((result (eai-tool-library-emacs--describe-function "car")))
    (should (stringp result))
    (should (> (length result) 0))
    ;; prin1-to-string of a doc string includes the quotes
    (should (string-match-p "car\\|list\\|cons\\|nil" result))))

(ert-deftest etl-emacs/describe-function/known-eai-function ()
  "Returns documentation for a function defined in this library."
  (let ((result (eai-tool-library-emacs--describe-function
                 "eai-tool-library--limit-result")))
    (should (stringp result))))

(ert-deftest etl-emacs/describe-function/undefined-function ()
  "Returns an error message for an undefined function."
  (let ((result (eai-tool-library-emacs--describe-function
                 "etl-no-such-function-xyz-9999")))
    (should (stringp result))
    (should (string-match-p "not defined" result))))

(ert-deftest etl-emacs/describe-function/undefined-contains-function-name ()
  "The error message for an undefined function includes the function name."
  (let* ((fname "etl-no-such-function-abc-1234")
         (result (eai-tool-library-emacs--describe-function fname)))
    (should (string-match-p fname result))))

(provide 'test-emacs)
;;; test-emacs.el ends here
