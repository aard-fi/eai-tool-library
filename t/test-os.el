;;; test-os.el --- Tests for eai-tool-library-os -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-os module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-os)

;;; eai-tool-library-os--run-shell

(ert-deftest etl-os/run-shell/echo ()
  "Runs a simple echo command and captures output."
  (let ((result (eai-tool-library-os--run-shell "echo hello")))
    (should (stringp result))
    (should (string-match-p "hello" result))))

(ert-deftest etl-os/run-shell/returns-string ()
  "Always returns a string."
  (let ((result (eai-tool-library-os--run-shell "true")))
    (should (stringp result))))

(ert-deftest etl-os/run-shell/pwd-output ()
  "Running pwd returns a directory path."
  (let ((result (eai-tool-library-os--run-shell "pwd")))
    (should (stringp result))
    (should (> (length (string-trim result)) 0))))

(ert-deftest etl-os/run-shell/multiline-output ()
  "Captures multi-line output."
  (let ((result (eai-tool-library-os--run-shell "printf 'a\\nb\\nc\\n'")))
    (should (string-match-p "a" result))
    (should (string-match-p "b" result))
    (should (string-match-p "c" result))))

;;; eai-tool-library-os--read-file-contents

(ert-deftest etl-os/read-file-contents/basic ()
  "Reads and returns the contents of a file."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (result (eai-tool-library-os--read-file-contents file)))
    (should (stringp result))
    (should (string-match-p "Lorem ipsum" result))))

(ert-deftest etl-os/read-file-contents/returns-full-content ()
  "Returns the complete file contents."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (result (eai-tool-library-os--read-file-contents file))
         (expected (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
    (should (string= expected result))))

;;; eai-tool-library-os--get-default-directory

(ert-deftest etl-os/get-default-directory/current-buffer ()
  "Returns default-directory of the current buffer when no argument given."
  (let ((result (eai-tool-library-os--get-default-directory)))
    (should (stringp result))
    (should (file-directory-p result))))

(ert-deftest etl-os/get-default-directory/named-buffer ()
  "Returns default-directory of the named buffer."
  (let ((buf (get-buffer-create "*etl-test-default-dir*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory "/tmp/"))
          (let ((result (eai-tool-library-os--get-default-directory
                         "*etl-test-default-dir*")))
            (should (string= "/tmp/" result))))
      (kill-buffer buf))))

(ert-deftest etl-os/get-default-directory/nil-uses-current ()
  "Passing nil uses the current buffer."
  (let ((result (eai-tool-library-os--get-default-directory nil)))
    (should (stringp result))
    (should (string= default-directory result))))

;;; eai-tool-library-os--set-default-directory

(ert-deftest etl-os/set-default-directory/current-buffer ()
  "Sets default-directory of the current buffer."
  (let ((buf (get-buffer-create "*etl-test-setdir*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory "/tmp/"))
          (eai-tool-library-os--set-default-directory "/var/tmp" "*etl-test-setdir*")
          (with-current-buffer buf
            (should (string= (expand-file-name "/var/tmp")
                             default-directory))))
      (kill-buffer buf))))

(ert-deftest etl-os/set-default-directory/expands-path ()
  "Expands relative or ~ paths when setting default-directory."
  (let ((buf (get-buffer-create "*etl-test-setdir-expand*")))
    (unwind-protect
        (progn
          (eai-tool-library-os--set-default-directory "/tmp" "*etl-test-setdir-expand*")
          (with-current-buffer buf
            (should (file-directory-p default-directory))))
      (kill-buffer buf))))

(ert-deftest etl-os/set-default-directory/roundtrip-with-get ()
  "set and get default-directory roundtrip correctly."
  (let ((buf (get-buffer-create "*etl-test-dir-roundtrip*")))
    (unwind-protect
        (progn
          (eai-tool-library-os--set-default-directory
           "/tmp" "*etl-test-dir-roundtrip*")
          (let ((result (eai-tool-library-os--get-default-directory
                         "*etl-test-dir-roundtrip*")))
            (should (string= (expand-file-name "/tmp") result))))
      (kill-buffer buf))))

(provide 'test-os)
;;; test-os.el ends here
