;;; test-date-time.el --- Tests for eai-tool-library-date-time -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-date-time module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-date-time)

;;; eai-tool-library-date-time--get-date

(ert-deftest etl-date-time/get-date/returns-string ()
  "get-date returns a string."
  (should (stringp (eai-tool-library-date-time--get-date))))

(ert-deftest etl-date-time/get-date/format ()
  "get-date returns a string matching YYYY-MM-DD format."
  (let ((date (eai-tool-library-date-time--get-date)))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date))))

(ert-deftest etl-date-time/get-date/valid-components ()
  "get-date returns plausible year, month, and day values."
  (let* ((date (eai-tool-library-date-time--get-date))
         (parts (split-string date "-"))
         (year (string-to-number (nth 0 parts)))
         (month (string-to-number (nth 1 parts)))
         (day (string-to-number (nth 2 parts))))
    (should (>= year 2020))
    (should (and (>= month 1) (<= month 12)))
    (should (and (>= day 1) (<= day 31)))))

(ert-deftest etl-date-time/get-date/consistent-with-current-time ()
  "get-date returns the same date as format-time-string."
  (should (string= (format-time-string "%Y-%m-%d")
                   (eai-tool-library-date-time--get-date))))

;;; eai-tool-library-date-time--get-time

(ert-deftest etl-date-time/get-time/returns-string ()
  "get-time returns a string."
  (should (stringp (eai-tool-library-date-time--get-time))))

(ert-deftest etl-date-time/get-time/format ()
  "get-time returns a string matching HH:MM format."
  (let ((time (eai-tool-library-date-time--get-time)))
    (should (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}$" time))))

(ert-deftest etl-date-time/get-time/valid-components ()
  "get-time returns plausible hour and minute values."
  (let* ((time (eai-tool-library-date-time--get-time))
         (parts (split-string time ":"))
         (hour (string-to-number (nth 0 parts)))
         (minute (string-to-number (nth 1 parts))))
    (should (and (>= hour 0) (<= hour 23)))
    (should (and (>= minute 0) (<= minute 59)))))

(ert-deftest etl-date-time/get-time/consistent-with-current-time ()
  "get-time returns the same time as format-time-string."
  (should (string= (format-time-string "%H:%M")
                   (eai-tool-library-date-time--get-time))))

(provide 'test-date-time)
;;; test-date-time.el ends here
