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
(require 'stylish-repl)
(require 'cl)

(defvar stylish-projects nil
  "List of project roots that have been registered.")

(defun stylish-project-root (root)
  (file-name-as-directory (expand-file-name root)))

(defun stylish-project-name (root &optional subproject)
  (let ((base
         (file-name-nondirectory (directory-file-name (stylish-project-root root)))))
    (if subproject
        (concat base "/" subproject)
      base)))

(add-hook 'stylish-reconnect-hook
          (lambda () (setf stylish-projects nil)))

(defun stylish-handle-project-responses (command result closure)
  (let ((on-change      (or (getf closure :on-change) 'stylish-null-function))
        (on-repl-change (or (getf closure :on-repl-change) 'stylish-null-function))
        (on-output      (or (getf closure :on-output) 'stylish-null-function)))
    (cond ((equal command "repl_output")
           (with-current-buffer (getf closure :repl-buffer)
             (funcall on-output (getf result :data))))
          ((equal command "repl_generation_change")
           (with-current-buffer (getf closure :repl-buffer)
             (funcall on-repl-change (getf result :generation))))
          ((equal command "project_change")
           (funcall on-change))
          ((equal command "register_project")
           (push (file-name-as-directory (getf result :root)) stylish-projects)
           (message "Registered '%s' successfully" (getf result :name)))))
  :keep-handler)

(defun stylish-register-project (root &optional subproject on-change on-output on-repl-change closure)
  "Register a project at ROOT.
Optional argument ON-CHANGE is called whenever the project changes.
Optional argument ON-OUTPUT is called when the inferior REPL produces output.
Optional argument ON-REPL-CHANGE is called when the REPL is successfully reloaded.
Optional argument CLOSURE is data to pass to the callbacks."
  (setf root (stylish-project-root root))
  (when (find root stylish-projects :test #'equal)
    (error "%s is already registered" root))

  (stylish-send-message
   "register_project"
   `(:root ,root ,@(when subproject (list :subproject subproject)))
   #'stylish-handle-project-responses
   #'stylish-null-function
   `(:root ,root
     :on-change ,on-change
     :on-output ,on-output
     :on-repl-change ,on-repl-change
     ,@closure)))

(defun stylish-handle-unregistration-response (&rest dont-care)
  (let ((root (getf closure :root)))
    (setf stylish-outstanding-request-handlers
          (delete-if (lambda (row)
                       (and (eq (nth 2 row) 'stylish-handle-project-responses)
                            (equal (getf (nth 1 row) :root) root)))
                     stylish-outstanding-request-handlers))
    (setf stylish-projects
          (delete root stylish-projects))))

(defun stylish-unregister-project (root &optional force)
  "Stop watching ROOT in Stylish, and cleanup Emacs' handlers.
Optional argument FORCE skips the existence check."
  (setf root (stylish-project-root root))
  (when (and (not force) (not (find root stylish-projects :test #'equal)))
    (error "%s is not a registered project" root))

  (stylish-send-message
   "unregister_project"
   `(:root ,root)
   'stylish-handle-unregistration-response
   'stylish-handle-unregistration-response
   `(:root ,root)))

(defun stylish-project-repl (&optional subproject)
  "Start or switch to this project's Stylish REPL.
Optional argument SUBPROJECT is the subproject to attempt to load."
  (interactive "p")

  ;; fixup the subproject when called interactively
  (cond ((and (called-interactively-p) (= subproject 4))
         (setf subproject (read-from-minibuffer "Subproject: ")))
        ((called-interactively-p)
         (setf subproject nil)))

  (let* ((root (eproject-root))
         (buf (stylish-repl-get-buffer (stylish-project-name root subproject))))

    (with-current-buffer buf
      (setf eproject-root root)
      (eproject-mode 1)
      (add-hook 'kill-buffer-hook
                ;; FIXME: incorrect for subprojects
                (lambda () (ignore-errors
                            (stylish-unregister-project (eproject-root))))
                nil t))

    (ignore-errors
      (stylish-register-project root subproject nil
       (lambda (output)
         (stylish-repl-insert-beforeprompt output 'stylish-repl-output-face))
       (lambda (gen)
         (stylish-repl-insert-beforeprompt (format "Now at generation %s\n" gen) 'stylish-repl-message-face))

       `(:repl-buffer ,buf)))
    (pop-to-buffer buf)))

(provide 'stylish-project)
;;; stylish-project.el ends here
