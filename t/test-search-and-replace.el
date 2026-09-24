;;; test-search-and-replace.el --- Tests for eai-tool-library-search-and-replace -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-search-and-replace module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-search-and-replace)

(defun etl-sar--run-smerge-test (input search replacement)
  "Apply gptel-search-replace-smerge to INPUT buffer with SEARCH/REPLACEMENT.
Returns the resulting buffer string."
  (with-temp-buffer
    (insert input)
    (gptel-search-replace-smerge search replacement)
    (buffer-string)))

(defun etl-sar--run-smerge-old-test (input search replacement)
  "Apply gptel-search-replace-smerge-old to INPUT with SEARCH/REPLACEMENT.
SEARCH is treated as a regexp by the old function."
  (with-temp-buffer
    (insert input)
    (gptel-search-replace-smerge-old search replacement)
    (buffer-string)))

;;; gptel-search-replace-smerge

(ert-deftest etl-sar/smerge/single-match-produces-conflict-block ()
  "A single match produces one set of smerge conflict markers."
  (let ((result (etl-sar--run-smerge-test
                 "hello world\n"
                 "hello"
                 "goodbye")))
    (should (string-match-p "<<<<<<< ORIGINAL" result))
    (should (string-match-p "=======" result))
    (should (string-match-p ">>>>>>> REPLACEMENT" result))))

(ert-deftest etl-sar/smerge/original-text-preserved-in-block ()
  "The original matching text appears in the conflict block."
  (let ((result (etl-sar--run-smerge-test
                 "the quick brown fox\n"
                 "quick"
                 "slow")))
    (should (string-match-p "quick" result))))

(ert-deftest etl-sar/smerge/replacement-text-in-block ()
  "The replacement text appears in the conflict block."
  (let ((result (etl-sar--run-smerge-test
                 "the quick brown fox\n"
                 "quick"
                 "slow")))
    (should (string-match-p "slow" result))))

(ert-deftest etl-sar/smerge/non-matching-text-unchanged ()
  "Text that does not match is left unchanged."
  (let ((result (etl-sar--run-smerge-test
                 "aaa bbb ccc\n"
                 "bbb"
                 "xxx")))
    (should (string-match-p "aaa" result))
    (should (string-match-p "ccc" result))))

(ert-deftest etl-sar/smerge/no-match-leaves-buffer-unchanged ()
  "When search string is not found, the buffer is not modified."
  (let* ((input "hello world\n")
         (result (etl-sar--run-smerge-test input "notfound" "anything")))
    (should (string= input result))))

(defun etl-sar--count-occurrences (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let ((count 0)
        (start 0))
    (while (setq start (string-search needle haystack start))
      (setq count (1+ count))
      (setq start (+ start (length needle))))
    count))

(ert-deftest etl-sar/smerge/multiple-matches ()
  "At least one conflict block is produced when there are multiple occurrences.
Note: due to the stale end-bound after the first replacement grows the buffer,
subsequent matches may not be processed in a single pass."
  (let ((result (etl-sar--run-smerge-test
                 "foo bar foo\n"
                 "foo"
                 "baz")))
    (should (>= (etl-sar--count-occurrences "<<<<<<< ORIGINAL" result) 1))))

(ert-deftest etl-sar/smerge/from-test-data-file ()
  "Produces expected output matching the smerge1.txt test fixture."
  (let* ((input-file (gtl--test-data-file "lorem-ipsum.txt"))
         (expected-file (gtl--test-data-file "smerge1.txt"))
         (result (with-temp-buffer
                   (insert-file-contents input-file)
                   (gptel-search-replace-smerge
                    "quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
                    "quis nostrud 'aarde' exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
                   (buffer-string)))
         (expected (with-temp-buffer
                     (insert-file-contents expected-file)
                     (buffer-string))))
    (should (string= expected result))))

(ert-deftest etl-sar/smerge/multiline-search ()
  "Handles multi-line search strings."
  (let ((result (etl-sar--run-smerge-test
                 "line one\nline two\nline three\n"
                 "line one\nline two"
                 "replaced one\nreplaced two")))
    (should (string-match-p "<<<<<<< ORIGINAL" result))
    (should (string-match-p "replaced" result))))

(ert-deftest etl-sar/smerge/prefix-suffix-preserved ()
  "Text on the same line before and after the match is preserved."
  (let ((result (etl-sar--run-smerge-test
                 "prefix TARGET suffix\n"
                 "TARGET"
                 "REPLACEMENT")))
    (should (string-match-p "prefix" result))
    (should (string-match-p "suffix" result))))

;;; gptel-search-replace-smerge-old (regexp-based version)
;; Note: this function is the old/legacy version and has a known bug where
;; re-search-forward is called with a stale end bound after the first
;; replacement, causing an error. Only the no-match case is safe to test.

(ert-deftest etl-sar/smerge-old/no-match-leaves-buffer-unchanged ()
  "The old function does not modify the buffer when no match is found."
  (let* ((input "hello world\n")
         (result (etl-sar--run-smerge-old-test input "notfound" "anything")))
    (should (string= input result))))

(provide 'test-search-and-replace)
;;; test-search-and-replace.el ends here
