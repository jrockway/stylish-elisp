;;; stylish-project.el --- integrate eproject and stylish

;; Copyright (C) 2010  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: convenience, hating the default list of keywords

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

;;; Commentary:

;;

;;; Code:

(require 'eproject)
(require 'stylish)
(require 'cl)

(defvar stylish-projects nil
  "List of project roots that have been registered.")

(add-hook 'stylish-reconnect-hook
          (lambda () (setf stylish-projects nil)))
          
(defun stylish-handle-project-responses (command result closure)
  (let ((on-change      (or (getf closure :on-change) 'stylish-null-function))
        (on-repl-change (or (getf closure :on-repl-change) 'stylish-null-function))
        (on-output      (or (getf closure :on-output) 'stylish-null-function)))
    (cond ((equal command "repl_output")
           (funcall on-output
                    (getf result :data)
                    (getf closure :repl)))
          ((equal command "repl_generation_change")
           (funcall on-repl-change
                    (getf result :generation)
                    (getf closure :repl)))
          ((equal command "project_change")
           (funcall on-change))
          ((equal command "register_project")
           (push (file-name-as-directory (getf result :root)) stylish-projects)
           (message "Registered '%s' successfully" (getf result :name)))))
  :keep-handler)

(defun stylish-register-project (root &optional name on-change on-output on-repl-change)
  "Register a project at ROOT with NAME.
Optional argument ON-CHANGE is called whenever the project changes.
Optional argument ON-OUTPUT is called when the inferior REPL produces output.
Optional argument ON-REPL-CHANGE is called when the REPL is successfully reloaded."
  (setf root (file-name-as-directory (expand-file-name root)))
  (when (find root stylish-projects :test #'equal)
    (error "%s is already registered" root))

  (stylish-send-message
   "register_project"
   (if name `(:root ,root :name ,name) `(:root ,root))
   #'stylish-handle-project-responses
   #'stylish-null-function
   `(:root ,root
     :on-change ,on-change
     :on-output ,on-output
     :on-repl-change ,on-repl-change)))

(defun stylish-handle-unregistration-response (&rest dont-care)
  (let ((root (getf closure :root)))
    (setf stylish-outstanding-request-handlers
          (delete-if (lambda (row)
                       (and (eq (nth 2 row) 'stylish-handle-project-responses)
                            (equal (getf (nth 1 row) :root) root)))
                     stylish-outstanding-request-handlers))
    (setf stylish-projects
          (delete root stylish-projects))))

(defun stylish-unregister-project (root)
  "Stop watching ROOT in Stylish, and cleanup Emacs' handlers."
  (setf root (file-name-as-directory (expand-file-name root)))
  (when (not (find root stylish-projects :test #'equal))
    (error "%s is not a registered project" root))

  (stylish-send-message
   "unregister_project"
   `(:root ,root)
   'stylish-handle-unregistration-response
   'stylish-handle-unregistration-response
   `(:root ,root)))

(provide 'stylish-project)
;;; stylish-project.el ends here
