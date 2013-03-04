;;; helm-find-files-in-project.el --- Find files in project from helm

;; Copyright (C) 2012  Toshiyuki Takahashi

;; Author: Toshiyuki Takahashi (@tototoshi)
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;;
;;; (require 'helm-find-files-in-project)
;;;
;;; Code:

;;; find files in current project

(defvar helm-find-files-in-project-filter-pattern
  "\\~\\|\\.git\\|target/\\|\\.class\\|\\.svn")

(defun helm-find-files-in-project-dirname (file)
  (chomp (shell-command-to-string (format "dirname %s" file))))

(defun helm-find-files-in-project-find-project-file (directory)
  (find "^\\.git$\\|^pom\\.xml$\\|.+\\.sbt$\\|^build.xml$"
        (directory-files directory)
        :test #'(lambda (x y) (string-match (concat x "$") y))))

(defun helm-find-files-in-project-find-project-file-recursively (directory)
  (cond ((and (string= "/" directory) (string= "/" (helm-find-files-in-project-dirname "/"))) nil)
        ((helm-find-files-in-project-find-project-file directory)
         (concat (file-name-as-directory directory)
                 (helm-find-files-in-project-find-project-file directory)))
        (t (helm-find-files-in-project-find-project-file-recursively
            (helm-find-files-in-project-dirname directory)))))

(defun helm-find-files-in-project-find-project-root ()
  (let* ((current-directory (with-current-buffer helm-current-buffer
                (helm-c-current-directory)))
         (project-file (helm-find-files-in-project-find-project-file-recursively current-directory)))
    (when project-file
      (helm-find-files-in-project-dirname project-file))))

(defun helm-c-source-files-under-tree-candidates-function ()
  (let ((project-root (helm-find-files-in-project-find-project-root)))
    (when (helm-find-files-in-project-find-project-root)
      (split-string
       (shell-command-to-string
        (format "find %s -type f | grep -v '%s'"
                project-root
                helm-find-files-in-project-filter-pattern))
       "\n"))))

(defvar helm-c-source-files-in-project
  '((name . "Files in project")
    (candidates . helm-c-source-files-under-tree-candidates-function)
    (action . find-file)))

(defun helm-find-files-in-project ()
  (interactive)
  (helm 'helm-c-source-files-in-project))

(provide 'helm-find-files-in-project)

;;; helm-find-files-in-project.el ends here

