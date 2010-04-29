(require 'stylish)

(defvar stylish-repl-history nil
  "History of commands you've entered into the REPL.")

(defvar stylish-repl-history-id -1
  "Which history element we're using right now.  Reset by `stylish-repl-send'.")

(defvar stylish-repl-name nil
  "The name of this REPL, according to the Stylish server.")

(make-variable-buffer-local 'stylish-repl-history)
(make-variable-buffer-local 'stylish-repl-history-id)
(make-variable-buffer-local 'stylish-repl-name)

(defvar stylish-repl-prompt-map nil
  "Keymap used when at the PERL> prompt")

(defvar stylish-repl-internal-commands-alist nil
  "Dispatch table for repl internal commands, elements are of the form:
   (name . function")

; custom

(defgroup stylish-repl nil
  "Stylish REPL"
  :prefix "stylish-repl-"
  :group 'stylish)

(defface stylish-repl-result-face
  '((t (:inherit font-lock-type-face)))
  "The face for the result of a REPL evaluation"
  :group 'stylish-repl)

(defface stylish-repl-error-face
  '((t (:inherit font-lock-warning-face)))
  "The face for a perl error returned as the result of a REPL evaluation"
  :group 'stylish-repl)

(defface stylish-repl-message-face
  '((t (:inherit font-lock-comment-face)))
  "The face for messages generated by the Stylish REPL"
  :group 'stylish-repl)

(defface stylish-repl-sent-face
  '((t (:underline t)))
  "The face the query is changed to after its sent to the REPL."
  :group 'stylish-repl)

(defface stylish-repl-output-face
  '((t nil))
  "The face for anything sent to STDOUT/STDERR on the perl side."
  :group 'stylish-repl)

(defface stylish-repl-interaction-face
  '((t nil))
  "The face for REPL command results generated by emacs (not from
the Perl REPL)"
  :group 'stylish-repl)

(defun stylish-repl-name-for (name)
  (concat "*Stylish REPL " name "*"))

(defun stylish-repl (&optional name no-select)
  "Spawn a Stylish REPL buffer.
Optional argument NAME is the name of the REPL to attach to.
Optional argument NO-SELECT inhibits popping to the buffer."
  (interactive)
  (when (not name) (setf name "default"))
  (let ((buf (get-buffer-create (stylish-repl-name-for name))))
    (with-current-buffer buf
      (stylish-repl-mode)
      (setf stylish-repl-history (make-ring 50))
      (setf stylish-repl-history-id -1)
      (setf stylish-repl-name name))
    (when (not no-select) (display-buffer buf))
    buf))

(define-derived-mode stylish-repl-mode fundamental-mode "Stylish[REPL]"
  "The major mode for the Stylish REPL buffer."

  (setq stylish-repl-prompt-map
        (let ((k (make-sparse-keymap)))
          (set-keymap-parent k stylish-repl-mode-map)))

  (define-key stylish-repl-prompt-map (kbd "<RET>") 'stylish-repl-send)
  (define-key stylish-repl-prompt-map (kbd "C-a") 'stylish-repl-beginning-of-line)
  (define-key stylish-repl-prompt-map (kbd "C-c C-c") 'stylish-repl-OH-NOES!!11!)
  (define-key stylish-repl-prompt-map (kbd "M-p") 'stylish-repl-history-up)
  (define-key stylish-repl-prompt-map (kbd "M-n") 'stylish-repl-history-down)

  (when (boundp 'cperl-mode-syntax-table)
    (set-syntax-table cperl-mode-syntax-table))

  (stylish)
  (stylish-repl-message "Welcome to the Stylish REPL!")
  (stylish-repl-insert-prompt))

(defun stylish-repl-register-command (command function)
  "Add a new command to the repl internal commands dispatch table."
  (add-to-list 'stylish-repl-internal-commands-alist  (cons command function)))

(defun stylish-repl-command-help nil
  "Show this help message."
  (stylish-repl-message "Known commands:")
  (loop for item in stylish-repl-internal-commands-alist
        do
        (stylish-repl-message
         (format ",%s\t\t%s (%s)"
                 (car item)
                 (documentation (cdr item))
                 (cdr item))))
  t)

(stylish-repl-register-command "help" 'stylish-repl-command-help)

(defun stylish-repl-usual-properties (start end &optional face)
  (let ((inhibit-read-only t)) ; fuck you, read-only.
    (when face (put-text-property start end 'face face))
    ;(put-text-property start end 'intangible t)
    (put-text-property start end 'rear-nonsticky '(face intangible))
    (put-text-property start end 'read-only t)))

(defun stylish-repl-insert (text &optional face)
  "Insert immutable text into the Stylish REPL buffer"
  (let ((inhibit-read-only t) (begin (point)))
    (insert text)
    (stylish-repl-usual-properties begin (point) face)))

(defun stylish-repl-message (message)
  "Insert a system-generated message"
  (stylish-repl-insert (concat message "\n") 'stylish-repl-message-face))

(defun stylish-repl-input-region-bounds nil
  "Determine the Stylish input region"
  ;; XXX: rewrite this to use properties instead of regexen!
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (re-search-backward "PERL> "))
    (let ((start (match-end 0))
          (end   (point-max)))
      (cons start end))))

(defun stylish-repl-input-region-text nil
  "Return the text inside `stylish-repl-input-region-bounds'."
  (let* ((region (stylish-repl-input-region-bounds))
         (start (car region))
         (end (cdr region))
         (text (buffer-substring-no-properties start end)))
    text))

(defun* stylish-handler-repl (command (&key result success data repl) (&key buffer))
  "Handle a return from the REPL"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (cond ((equal command "repl")
             (let ((face (if (= 0 success)
                             'stylish-repl-error-face 'stylish-repl-result-face)))
               (stylish-repl-insert (concat result "\n") face))
             (stylish-repl-insert-prompt))

          ((equal command "repl_output")
           (stylish-repl-insert data 'stylish-repl-output-face))))
    (goto-char (point-max)))
    :keep-handler)

(defun stylish-repl-eval-perl (perl)
  (stylish-send-message "repl" `(:code ,perl :name ,stylish-repl-name)
                        'stylish-handler-repl nil
                        `(:buffer ,(current-buffer))))

(defun stylish-repl-send (&optional nosave)
  "Send a command to the REPL"
  (interactive)
  (let* ((region (stylish-repl-input-region-bounds))
         (start (car region))
         (end (cdr region))
         (text (stylish-repl-input-region-text)))
    (stylish-repl-usual-properties start end 'stylish-repl-sent-face)
    (unless nosave (stylish-repl-history-add text))
    (end-of-line)
    (stylish-repl-insert "\n")
    (if (string-match "^," text) ; perl or internal command?
        (stylish-repl-process-internal-command text)
      (stylish-repl-eval-perl text))))

(defun stylish-repl-get-buffer (&optional name)
  (cond (name (let ((buf (get-buffer (stylish-repl-name-for name))))
                (if buf buf
                  (stylish-repl name t))))
        (t (current-buffer))))

(defun stylish-repl-process-internal-command (command)
  "Run the internal command COMMAND."
  (setq command (substring command 1)) ; remove leading ,
  (let* ((c (assoc command stylish-repl-internal-commands-alist))
         (handler (cdr c))
         (prompt t))
    (if (not c)
        (stylish-repl-insert (format "No command called %s\n" command)
                             'font-lock-warning-face)
      (setq prompt (funcall handler)))
    (if prompt (stylish-repl-insert-prompt))))

;; (defun stylish-repl-send-file (&optional buffer)
;;   "Send a file to the REPL to load"
;;   (interactive)
;;   (or buffer (setq buffer (current-buffer)))
;;   (let ((fn (buffer-file-name buffer)))
;;     (stylish-send-command 'repl-load-file :filename fn)
;;     (with-current-buffer (stylish-repl-get-buffer)
;;       (stylish-repl-message (format "\n# Sending %s\n" fn)))))

(defun stylish-repl-send-region-to-stylish (start end)
  "Send region to stylish buffer.
Argument START and END define the region to send."
  (interactive "r")
  (let ((string (buffer-substring-no-properties start end)))
    (with-current-buffer (stylish-repl-get-buffer (ignore-errors (eproject-root)))
      (insert "\n")
      (stylish-repl-eval-perl string)))
  (when (eq (point) start) (exchange-point-and-mark))
  (deactivate-mark))

(defun stylish-repl-insert-prompt nil
  "Insert the REPL prompt"
  (stylish-repl-insert "PERL>" font-lock-keyword-face)
  (let ((inhibit-read-only t)) (insert (propertize " " 'read-only nil)))
  (goto-char (point-max))
  (set-window-point (get-buffer-window (current-buffer)) (point-max)))

(defun stylish-repl-beginning-of-line nil
  (interactive)
  (goto-char (car (stylish-repl-input-region-bounds))))

(defun stylish-repl-OH-NOES!!11! nil
  "Reconnect to the stylish server if output gets out of sync or something."
  (interactive)
  (stylish-repl-message "\nRestarting the Stylish REPL")
  (stylish-send-message "kill_repl" `(:name ,stylish-repl-name)
                        (lambda (command result closure)
                          (with-current-buffer (getf closure :buffer)
                            (stylish-repl-insert-prompt)))
                        nil
                        `(:name ,stylish-repl-name
                          :buffer ,(current-buffer))))

(defun stylish-repl-history-cleanup nil
  "Remove all elements from the history ring that have a true
cdr."
  ;; clean out the partials
  (catch :done
    (loop for i from 0 to 50
          do
          (ignore-errors
            (when (cdr (ring-ref stylish-repl-history i))
              (ring-remove stylish-repl-history i)
              (throw :done))))))

(defun stylish-repl-history-reset nil
  "Cleanup the REPL history; remove temporary items, reset index
to -1."
  (stylish-repl-history-cleanup)
  (setq stylish-repl-history-id -1))

(defun stylish-repl-history-add (text &optional temp)
  "Add TEXT as the most recent REPL history item.  TEMP should be
true if we should remove this element in
`stylish-repl-history-cleanup'."
  (stylish-repl-history-reset)
  (ring-insert stylish-repl-history (cons text temp)))

(defun stylish-repl-history-get (&optional id)
  "Return the IDth most recent history entry.  If ID is nil,
return the `stylish-repl-history-id'-th history item.  0 is the
most recent, 50 is the oldest."
  (or id (setq id stylish-repl-history-id))
  (car (ring-ref stylish-repl-history id)))

(defun stylish-repl--replace-region ()
  "Replace the region from START to END with H"
  (goto-char start)
  (delete-region start end)
  (insert h)
  (goto-char (point-max)))

(add-hook 'stylish-repl-history-pre-hook
          (lambda nil (goto-char (point-max))))

(defun stylish-repl-history-up nil
  (interactive)
  (run-hooks 'stylish-repl-history-pre-hook)
  ;; first time we go up, save the half-entred line
  (when (eq stylish-repl-history-id -1)
    (let ((current (stylish-repl-input-region-text)))
      (stylish-repl-history-add current t))
    (incf stylish-repl-history-id))
  ;; other times, continue up the loop
  (incf stylish-repl-history-id)
  (let* ((bounds (stylish-repl-input-region-bounds))
         (start (car bounds)) (end (cdr bounds))
         (h (stylish-repl-history-get)))
    (if (not h)
        (error "No more history!")
      (stylish-repl--replace-region))))

(defun stylish-repl-history-down nil
  (interactive)
  (run-hooks 'stylish-repl-history-pre-hook)
  (when (< stylish-repl-history-id 1)
    (error "Can't look into the future!"))
  (decf stylish-repl-history-id)
  (let* ((bounds (stylish-repl-input-region-bounds))
         (start (car bounds)) (end (cdr bounds))
         (h (stylish-repl-history-get)))
    (stylish-repl--replace-region)))

;;
;;(defun stylish-repl-highlight-input ()
;;  (interactive)
;;  (let ((bounds (stylish-repl-input-region-bounds)))
;;    (stylish-syntaxify (car bounds) (cdr bounds) (stylish-repl-get-buffer))))
;;
;;(defun turn-on-stylish-syntax nil
;;  "Turn on stylish syntaxifier in the REPL"
;;  (interactive)
;;  (with-current-buffer (stylish-repl-get-buffer)
;;    (add-hook 'post-command-hook 'stylish-repl-highlight-input)))

(defun stylish-repl-clear nil
  "Clear the REPL buffer"
  (interactive)
  (with-current-buffer (stylish-repl-get-buffer)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))))
  t)
(stylish-repl-register-command "clear" 'stylish-repl-clear)

(provide 'stylish-repl)
