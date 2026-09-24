;;; eai-tool-library-project.el --- Tools related to project handling -*- lexical-binding: t; -*-
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
;; This module contains some safe modules useful for gathering various data from
;; emacs. For potentially destructive tools as well as for elisp development
;; support see the `elisp' module.
;;
;;; Code:

(require 'eai-tool-library)
(require 'project)
(require 'xref)

(defvar eai-tool-library-project-tools '()
  "The list of emacs related tools")
;; we skip the other two variables as this module should only contain safe tools

(defun eai-tool-library-project--current (&optional project)
  "Return the project for `project', or current directory"
  (or (and project (project-current nil project))
      (project-current)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-project-tools
 :function #'eai-tool-library-project--current
 :name  "project-current"
 :description "Return the root directory of a project, or nil, if not in a project."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t))
 :category "project")

(defun eai-tool-library-project--files (&optional project dirs)
  "Return list of files in the current project.

Optional DIRS is a list of directories within the project to filter by."
  (let* ((project (or (and project (project-current nil project))
                      (project-current)))
         (project-dir (if project (project-root project) (error "Not in a project"))))
    (if (not project)
        (message "Not currently in a project")
      (let* ((files (project-files project))
             (filtered-files files))
        (when dirs
          (setq filtered-files
                (cl-remove-if-not
                 (lambda (file)
                   (let ((relpath (file-relative-name file project-dir)))
                     (cl-some (lambda (dir) (string-prefix-p dir relpath)) dirs)))
                 files)))
        filtered-files))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-project-tools
 :function #'eai-tool-library-project--files
 :name  "project-files"
 :description "Returns the files contained in a project, optionally limited to subdirectories."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t)
             '(:name "dirs"
                     :type array
                     :description "A list of subdirectories of the project for limiting file results. They're expected relative to the project directory, so there's no need trying to expand them."
                     :optional t))
 :category "project")

(defun eai-tool-library-project--buffers (&optional project)
  "Return list of buffers visiting files in the current project.
Optional DIRS is a list of directories within the project to filter by."
  (let* ((project (or (and project (project-current nil project))
                      (project-current))))
    (if (not project)
        (message "Not currently in a project")
      (project-buffers project))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-project-tools
 :function #'eai-tool-library-project--buffers
 :name  "project-buffers"
 :description "Returns the open buffers related to a project."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t))
 :category "project")

(defun eai-tool-library-project--find-regexp (regexp &optional project dirs)
  "Find all matches for REGEXP in the current project.

Optional DIRS is a list of directories within the project to filter by."
  (let* ((project (or (and project (project-current nil project))
                      (project-current)))
         (project-dir (if project (project-root project) (error "Not in a project"))))
    (if (not project)
        (message "Not currently in a project")
      (let* ((files (project-files project))
             (filtered-files files))
        (when dirs
          (setq filtered-files
                (cl-remove-if-not
                 (lambda (file)
                   (let ((relpath (file-relative-name file project-dir)))
                     (cl-some (lambda (dir) (string-prefix-p dir relpath)) dirs)))
                 files)))
        (let ((xrefs (xref-matches-in-files regexp filtered-files)))
          (mapcar
           (lambda (xref)
             (let ((loc (xref-item-location xref)))
               (list :file (xref-file-location-file loc)
                     :line (xref-file-location-line loc)
                     :column (xref-file-location-column loc)
                     :summary (xref-item-summary xref))))
           xrefs))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-project-tools
 :function #'eai-tool-library-project--find-regexp
 :name  "project-find-regexp"
 :description "Search for a regexp pattern in files within the current project, optionally limited to subdirectories. Returns a list of matches with file, line, column, and summary."
 :args (list '(:name "regexp"
                     :type string
                     :description "The regular expression pattern to search for.")
             '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t)
             '(:name "dirs"
                     :type array
                     :description "A list of subdirectories of the project for limiting search results. They're expected relative to the project directory."
                     :optional t))
 :category "project")

(defun eai-tool-library-project--locate-file (file &optional project include-all)
  "Locate a file in the current project by name, returning its full path.

FILE is a file name or pattern to search for. When INCLUDE-ALL is
non-nil, include all files under the project root, except for VCS
directories."
  (let* ((project (or (and project (project-current nil project))
                      (project-current))))
    (if (not project)
        (message "Not currently in a project")
      (let ((files (project-files project)))
        (cl-find-if
         (lambda (f)
           (or (string-suffix-p file f)
               (string= file (file-name-nondirectory f))))
         files)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-project-tools
 :function #'eai-tool-library-project--locate-file
 :name  "project-locate-file"
 :description "Locate a file in the current project by name or suffix, returning its full path."
 :args (list '(:name "file"
                     :type string
                     :description "The file name or pattern to search for in the project.")
             '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t)
             '(:name "include-all"
                     :type boolean
                     :description "When non-nil, include all files under the project root, except for VCS directories."
                     :optional t))
 :category "project")

(provide 'eai-tool-library-project)
;;; eai-tool-library-project.el ends here
