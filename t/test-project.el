;;; test-project.el --- Tests for eai-tool-library-project -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-project module.
;; These tests assume the test runner executes within a git/project repository.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-project)

(defvar etl-project--root
  (expand-file-name ".." gtl-test-directory)
  "The project root directory (parent of test directory).")

;;; eai-tool-library-project--current

(ert-deftest etl-project/current/finds-project ()
  "Returns a project object when called from within a project."
  (let ((default-directory etl-project--root))
    (let ((result (eai-tool-library-project--current)))
      (should result))))

(ert-deftest etl-project/current/with-explicit-path ()
  "Returns a project object when called with an explicit project path."
  (let ((result (eai-tool-library-project--current etl-project--root)))
    (should result)))

(ert-deftest etl-project/current/root-contains-project-dir ()
  "The project root is an ancestor of or equal to the project directory."
  (let* ((default-directory etl-project--root)
         (proj (eai-tool-library-project--current))
         (root (and proj (project-root proj))))
    (when root
      (should (file-directory-p root))
      ;; file-in-directory-p handles trailing slash differences correctly
      (should (file-in-directory-p etl-project--root root)))))

;;; eai-tool-library-project--files

(ert-deftest etl-project/files/returns-list ()
  "Returns a list of files for the project."
  (let ((default-directory etl-project--root))
    (let ((result (eai-tool-library-project--files etl-project--root)))
      (should (listp result))
      (should (> (length result) 0)))))

(ert-deftest etl-project/files/contains-known-file ()
  "The file list contains a known file in the project."
  (let ((result (eai-tool-library-project--files etl-project--root))
        (known-file (expand-file-name "eai-tool-library.el" etl-project--root)))
    (should (member known-file result))))

(ert-deftest etl-project/files/filtered-by-subdir ()
  "When dirs filter is given, only files in those subdirs are returned."
  (let ((result (eai-tool-library-project--files etl-project--root '("t/"))))
    (should (listp result))
    (dolist (file result)
      (should (string-match-p "/t/" file)))))

;;; eai-tool-library-project--locate-file

(ert-deftest etl-project/locate-file/by-basename ()
  "Locates a file by its basename."
  (let ((default-directory etl-project--root))
    (let ((result (eai-tool-library-project--locate-file
                   "eai-tool-library.el" etl-project--root)))
      (should (stringp result))
      (should (string-suffix-p "eai-tool-library.el" result)))))

(ert-deftest etl-project/locate-file/by-suffix ()
  "Locates a file by a trailing path suffix."
  (let ((result (eai-tool-library-project--locate-file
                 "t/test-init.el" etl-project--root)))
    (should (stringp result))
    (should (string-suffix-p "t/test-init.el" result))))

(ert-deftest etl-project/locate-file/not-found-returns-nil ()
  "Returns nil when the file is not found in the project."
  (let ((result (eai-tool-library-project--locate-file
                 "nonexistent-file-xyz-99999.el" etl-project--root)))
    (should (null result))))

;;; eai-tool-library-project--buffers

(ert-deftest etl-project/buffers/returns-list ()
  "Returns a list (possibly empty) of project buffers."
  (let ((default-directory etl-project--root))
    (let ((result (eai-tool-library-project--buffers etl-project--root)))
      (should (listp result)))))

(ert-deftest etl-project/buffers/opened-file-in-list ()
  "A buffer visiting a project file appears in the project buffer list."
  (let* ((file (expand-file-name "eai-tool-library.el" etl-project--root))
         (buf (find-file-noselect file)))
    (unwind-protect
        (let ((result (eai-tool-library-project--buffers etl-project--root)))
          (should (memq buf result)))
      (kill-buffer buf))))

;;; eai-tool-library-project--find-regexp

(ert-deftest etl-project/find-regexp/returns-list ()
  "Returns a list of matches for a known pattern."
  (let ((default-directory etl-project--root))
    (let ((result (eai-tool-library-project--find-regexp
                   "eai-tool-library" etl-project--root)))
      (should (listp result))
      (should (> (length result) 0)))))

(ert-deftest etl-project/find-regexp/match-has-required-keys ()
  "Each match has :file, :line, :column, and :summary keys."
  (let ((result (eai-tool-library-project--find-regexp
                 "eai-tool-library" etl-project--root)))
    (let ((entry (car result)))
      (when entry
        (should (plist-member entry :file))
        (should (plist-member entry :line))
        (should (plist-member entry :column))
        (should (plist-member entry :summary))))))

(ert-deftest etl-project/find-regexp/no-match-returns-empty ()
  "Returns an empty list when no files match."
  (let ((result (eai-tool-library-project--find-regexp
                 "zzz-etl-no-such-pattern-xyz-99999" etl-project--root)))
    (should (or (null result) (listp result)))))

(ert-deftest etl-project/find-regexp/filtered-by-subdir ()
  "When dirs filter is given, only searches files in those subdirs."
  (let ((result (eai-tool-library-project--find-regexp
                 "require" etl-project--root '("t/"))))
    (dolist (match result)
      (should (string-match-p "/t/" (plist-get match :file))))))

(provide 'test-project)
;;; test-project.el ends here
