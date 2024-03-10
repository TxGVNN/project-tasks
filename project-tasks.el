;;; project-tasks.el --- Efficient task management for your project  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giap Tran

;; Author: Giap Tran <txgvnn@gmail.com>
;; Keywords: project, workflow, tools
;; Homepage: https://github.com/TxGVNN/project-tasks
;; Package-Requires: ((emacs "26.1") (project "0.6.0"))
;; Version: 0.5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `project-tasks' provides efficient task management for your project.
;; It allows you to organize and manage your project's tasks in an
;; Org file.  You can quickly jump to the task file, capture new tasks
;; and easily run tasks by name.
;;
;; The main idea is to use Org source blocks as tasks.  Each source
;; block is a task and its name is the task name.  You can use
;; `project-tasks' to quickly run a task by name.
;;
;; Example:
;; If we want to have a fast way to run `top` command in our project,
;; we can add a source block named `Run top` to `tasks.org` file:
;; #+name: Run top
;; #+begin_src elisp :results none
;; (comint-run "top")
;; #+end_src
;;
;; Then we just need to run M-x project-tasks to select and run the task.

;;; Code:
(require 'org)

;;; Custom vars
(defgroup project-tasks nil
  "Project tasks."
  :group 'project)

(defcustom project-tasks-files '("tasks.org")
  "List of task files, the file can be a regexp pattern."
  :type '(repeat string)
  :group 'project-tasks)

(defcustom project-tasks-ignore-files nil
  "List of ignored task files."
  :type '(repeat string)
  :group 'project-tasks)

(defcustom project-tasks-separator " -> "
  "Separator to separate file name and task name."
  :type 'string
  :group 'project-tasks)

(defcustom project-tasks-root-func #'project-tasks-project-root
  "Function to get project root directory."
  :type 'function
  :group 'project-tasks)

(defun project-tasks-project-root ()
  "Get project root by builtin `project' package."
  (if (fboundp 'project-root)
      (project-root (project-current t))
    (error "Please install `project' package or set `project-tasks-root-func'")))

(defun project-tasks--eval(task)
  "Execute a source block with name TASK."
  (org-babel-goto-named-src-block task)
  (org-babel-execute-src-block))

(defun project-tasks-current-buffer ()
  "Evaluate a selected source block from current Org buffer."
  (interactive)
  (let ((src-block-names (org-babel-src-block-names)))
    (unless src-block-names
      (error "No source blocks found in current buffer"))
    (let ((task (completing-read "Select task: " src-block-names nil t)))
      (project-tasks--eval task))))

(defun project-tasks--get-task-files ()
  "Get list of task files and exclude ignored files."
  (let* ((project-root-dir (funcall project-tasks-root-func))
         (project-files (mapcar (lambda (file)
                                  (file-relative-name file project-root-dir))
                                (project-files (project-current t))))
         (files (cl-remove-if (lambda (file)
                                (or (cl-some (lambda (pattern)
                                               (string-match-p pattern file))
                                             project-tasks-ignore-files)
                                    (not (cl-some (lambda (pattern)
                                                    (string-match-p pattern file))
                                                  project-tasks-files))))
                              project-files)))
    files))

;;;###autoload
(defun project-tasks ()
  "Open project tasks and display tasks."
  (interactive)
  (let* ((default-directory (funcall project-tasks-root-func))
         (src-block-names (mapcar (lambda (file)
                                    (let ((file-in-project (file-relative-name file default-directory)))
                                      (mapcar (lambda (x)
                                                (concat file-in-project project-tasks-separator x))
                                              (with-current-buffer (find-file-noselect file)
                                                (org-babel-src-block-names)))))
                                  (project-tasks--get-task-files))))
    (unless src-block-names
      (error "No source blocks found in project"))
    (let ((task (completing-read "Select task: " (apply #'append src-block-names) nil t)))
      (let* ((task-split (split-string task project-tasks-separator))
             (org-default-notes-file (car task-split))
             ;; task-name is rest of the string after the first project-tasks-separator
             (task-name (mapconcat #'identity (cdr task-split) project-tasks-separator)))
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (project-tasks--eval task-name))))))

(provide 'project-tasks)
;;; project-tasks.el ends here
