;;; stylish.el --- talk to a stylish server

;; Copyright (C) 2010  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: programming, repl

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

;;; See-also:

;; stylish-repl.el, a generic read-eval-print-loop for stylish.

;;; Wire protocol:

;; A bit about the wire protocol.  All requests and responses are JSON
;; objects separated by \r\n.
;;
;; When the client (stylish.el, etc.) connects to a server
;; (Stylish::Server for Perl, MozRepl in Stylish mode, etc.), the
;; server sends a welcome message, which is a JSON object of the form:
;;
;; { command: "welcome", session: <session id>, ... }
;;
;; session is some sort of session id.  <...> can be anything.
;; Typically the version number of the server is sent, but stylish.el
;; does not care right now.
;;
;; The interaction then continues asynchronously; the client may send
;; commands at any time, and the server may send responses (or other
;; data) at any time.
;;
;; The client initiates a command by first generating a unique number,
;; the cookie.  It then sends:
;;
;; { command: <command name>, cookie: <cookie>, args ... }
;;
;; args are arbitrary key/value pairs that are the args for command.
;; The cookie is mandatory.
;;
;; The server will reply to this command with a message of the form:
;;
;; { command: <reply name>, cookie: <cookie>, result: { <result> } }
;;
;; or
;;
;; { command: <reply name>, cookie: <cookie>, error: "<error message>" }
;;
;; The reply name is typically the command that the server is replying
;; to, but some commands have multiple reply types.  The cookie is the
;; cookie that the client sent to the server, identifying that this
;; message is a reply to that command.
;;
;; A successful result is indicated by the presense of a result key
;; containing a dictionary of results.  An error response is indicated
;; by the error key, which maps to an error message.
;;
;; See the code below to see how to connect to something that speaks
;; the Stylish protocol.

;;; Commentary:

;; The basic model for the Emacs binding is to first connect with
;; `stylish-connect' or `stylish-tcp-connect', and then to send
;; commands with `stylish-send-message'.  When a reply is ready (based
;; on the auto-generated cookie), your callback will be called with
;; the results merged with closure.  If there is an error, the
;; error-callback will be called instead.

;;; Code:

(require 'cl)
(require 'json)

(defvar stylish-outstanding-request-handlers nil
  "Alist of cookie -> (handler . args) representing handlers for Stylish requests.")

(defvar stylish-next-cookie 1
  "Cookie to be used for the next request.")

(defvar stylish-partial-message "")

(defvar stylish-process nil)

(defvar stylish-properties nil
  "List of server properties sent by the server upon connection,
  typically the language, etc.")

(defvar stylish-last-connectinfo nil
  "Cons cell of (connect-function . args) to be used for reconnection.")

(defvar stylish-reconnect-hook nil
  "Hook to run on connect or reconnect.")

(defgroup stylish nil
  "Stylish: make working foreign programming languages more enjoyable"
  :prefix "stylish-"
  :group 'tools
  :link '(url-link :tag "Project repository" "http://github.com/jrockway/stylish"))

(add-hook 'stylish-reconnect-hook
          (lambda () (setf stylish-outstanding-request-handlers nil)))

(defun stylish-null-function (&rest args)
  "This function works like Radioactive Man's goggles."
  nil)

(defun stylish-get-next-cookie ()
  "Return a unique cookie for the next request."
  (prog1 stylish-next-cookie (incf stylish-next-cookie)))

(defun stylish-filter (process message)
  "Tend to the Stylish connection.
Argument PROCESS is the stylish process.
Argument MESSAGE is the text we got from stylish."
  (setf stylish-partial-message (concat stylish-partial-message message))
  (stylish-process-buffer))

(defun stylish-sentinel (&rest args)
  "Tend to the Stylish connection.
Optional argument ARGS is optional.")

(defun stylish-pre-connect ()
  "Stuff that needs to happen before we connect to Stylish."
  (when stylish-process
    (delete-process stylish-process)
    (setf stylish-properties nil)
    (setf stylish-partial-message nil)))

(defun stylish-post-connect (connectinfo)
  "Stuff that needs to happen after we have a network connection to Stylish."
  (setq stylish-last-connectinfo connectinfo)
  (run-hooks 'stylish-reconnect-hook))

(defun stylish-connect ()
  "Connect to a stylish Stylish server."
  (interactive)
  (stylish-pre-connect)
  (prog1 (setf stylish-process
               (make-network-process :name "stylish"
                                     :family 'local
                                     :service "/tmp/stylish"
                                     :noquery t
                                     :filter #'stylish-filter
                                     :sentinel #'stylish-sentinel
                                     :coding 'utf-8))
    (stylish-post-connect (list #'stylish-connect))))

(defun stylish-tcp-connect (&optional port)
  "Connect to a stylish Stylish server running over TCP."
  (interactive "NPort number: ")
  (stylish-pre-connect)
  (prog1 (setf stylish-process
               (make-network-process :name "stylish"
                                     :host "localhost"
                                     :family 'ipv4
                                     :service port
                                     :noquery t
                                     :filter #'stylish-filter
                                     :sentinel #'stylish-sentinel
                                     :coding 'utf-8 ))
    (stylish-post-connect (list #'stylish-tcp-connect port))))

(defun stylish-running-p ()
  "Return non-nil if connected to Stylish, nil otherwise."
  (and stylish-process (eq (process-status stylish-process) 'open)))

(defun stylish (&optional reconnect)
  "Connect to Stylish, if not already connected.
Prefix argument RECONNECT forces a reconnect."
  (interactive "p")
  (when (or (not (stylish-running-p)) (eq reconnect 4))
    (when (null stylish-last-connectinfo)
      (error "Never connected to Stylish in this session!"))
    (destructuring-bind (func &rest args) stylish-last-connectinfo
      (apply func args))))

(defun stylish-process-buffer ()
  (with-temp-buffer ;; todo: make the buffer non-temp?
    (while
        (progn
          (delete-region (point-min) (point-max))
          (insert stylish-partial-message)
          (goto-char (point-min))
          (let* ((result (or (ignore-errors (stylish-json-decode))
                             (progn (goto-char (point-min)) nil)))
                 (rest   (progn
                           (skip-chars-forward "\n")
                           (buffer-substring-no-properties (point) (point-max)))))
            (setf stylish-partial-message rest)
            (when result
              (stylish-process-message result))
            result)))))

(defun stylish-json-decode ()
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read)))

(defun stylish-parse-integer (int)
  (case (type-of int)
    ('integer int)
    ('string (parse-integer int))
    ('sequence (parse-integer int))
    (t nil)))

(defun stylish-process-message (message)
  "Process the Stylish response represented MESSAGE."
  (let* ((cookie  (stylish-parse-integer (getf message :cookie)))
         (command (getf message :command))
         (result  (getf message :result))
         (error   (getf message :error)))
    (cond ((not (null cookie))
           (let* ((handler-row (cdr (assq cookie stylish-outstanding-request-handlers)))
                  (closure (nth 0 handler-row))
                  (handler (or (nth 1 handler-row)
                               (lambda (&rest args)
                                 (message "stylish: unhandled message: %s %s %s %s"
                                          cookie command result args))))
                  (error-handler (or (nth 2 handler-row)
                                     (lambda (&rest args)
                                       (message "stylish: error: %s" error))))
                  (handler-result
                   (condition-case e
                       (if error
                           (funcall error-handler error closure)
                         (funcall handler command result closure))
                     (error
                      (error "Problem handling message (%s) from Stylish server: %s"
                             message e)))))
             (when (not (eq handler-result :keep-handler))
               (setf stylish-outstanding-request-handlers
                     (assq-delete-all cookie stylish-outstanding-request-handlers)))))
          ((equal command "welcome")
           ;; a hat tip to SLIME ;)
           (setf stylish-properties (getf message :result))
           (message "Connected. Let the hacking begin.")))))

(defun stylish-message-encode (command cookie args)
  "Encode a message for transmission over the wire.
Argument COMMAND is the command to run.
Argument COOKIE is the unique cookie identifying this query.
Argument ARGS are the args for the command, a plist."
  (json-encode (append (list :cookie cookie :command command) args)))

(defun stylish-send-message (command &optional args callback error-callback closure)
  "Send COMMAND with ARGS (a plist) to the stylish server, and call CALLBACK with CLOSURE when the command is finished.  Call ERROR-CALLBACK on error."
  (stylish) ;; ensure that we are connected
  (let* ((cookie (stylish-get-next-cookie))
         (message (stylish-message-encode command cookie args)))
    (setf stylish-outstanding-request-handlers
          (cons (list cookie closure callback error-callback)
                stylish-outstanding-request-handlers))
    (process-send-string stylish-process (concat message "\015\012"))))

(provide 'stylish)
;;; stylish.el ends here
