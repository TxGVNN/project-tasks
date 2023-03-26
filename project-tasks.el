;;; project-tasks.el --- Efficient task management for your project  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giap Tran

;; Author: Giap Tran <txgvnn@gmail.com>
;; Keywords: project, workflow, tools
;; Homepage: https://github.com/TxGVNN/project-tasks
;; Package-Requires: ((emacs "26.1") (transient "0.3.7"))
;; Version: 0.1.1

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
;; (use-package project-tasks
;;   :ensure t :defer t
;;   ;; :custom (project-tasks-file "tasks.org")
;;   :commands (project-tasks))

;;; Code:
(require 'project)
(require 'org)
(require 'transient)

;;; Custom vars
(defgroup project-tasks nil
  "Project tasks."
  :group 'project)

(defcustom project-tasks-file "tasks.org"
  "File to store project tasks."
  :type 'string
  :group 'project-tasks)

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
    (transient-define-prefix project-tasks-eval-source-block-transient ()
      "Select source block to evaluate."
      ["Source block selection"
       ("q" "Quit" transient-quit-all)]
      [:setup-children
       (lambda (_)
         (transient-parse-suffixes
          'project-tasks-eval-source-block-transient
          (seq-map-indexed
           (lambda (src-block-name idx)
             (list (format "%d" idx) src-block-name
                   (lambda ()
                     (interactive)
                     (project-tasks--eval
                      (oref (transient-suffix-object) :description)))))
           src-block-names)))])
    (transient-setup 'project-tasks-eval-source-block-transient)))


;;;###autoload
(defun project-tasks ()
  "Open project tasks file and display tasks."
  (interactive)
  (let* ((project (project-root (project-current t)))
         (org-default-notes-file (concat project project-tasks-file)))
    (find-file org-default-notes-file)
    (with-current-buffer (current-buffer)
      (project-tasks-current-buffer))))

;;;###autoload
(defun project-tasks-capture ()
  "Org capture to project."
  (interactive)
  (unless (featurep 'org-capture) (require 'org-capture))
  (let* ((project (project-root (project-current t)))
         (org-default-notes-file (concat project project-tasks-file)))
    (call-interactively 'org-capture)))

;;;###autoload
(defun project-tasks-jump ()
  "Jump to the project task file."
  (interactive)
  (let* ((project (project-root (project-current t)))
         (org-default-notes-file (concat project project-tasks-file)))
    (find-file org-default-notes-file)))

(provide 'project-tasks)
;;; project-tasks.el ends here
