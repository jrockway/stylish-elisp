;;; stylish-repl-minor-mode.el --- interact with a Stylish REPL from another buffer

;; Copyright (C) 2011  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: convenience

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

(defvar stylish-repl-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-\M-x" 'stylish-repl-minor-mode-send-defun)
    ;; this is the binding that SLIME picks, so I thought I would be
    ;; consistent, even though it is already used for "compile".
    (define-key map "\C-c\C-k" 'stylish-repl-minor-mode-send-buffer)
    (define-key map "\C-xx" 'stylish-repl-minor-mode-snap-to-repl)
    map))

(define-minor-mode stylish-repl-minor-mode
"Send code from the current buffer to a Stylish REPL.

The following keys are bound in this minor mode:
\\{stylish-repl-minor-mode-map}"
  nil
  " Stylish[REPL]"
  :keymap stylish-repl-minor-mode-map
  :group 'stylish)

(defun stylish-repl-minor-mode-get-repl ()
  (get-buffer "*Stylish REPL default*")) ;; XXX

;; stolen from SLIME
(defun stylish-repl-minor-mode-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun stylish-repl-minor-mode-send-defun ()
  "Send the current function to the current Stylish REPL instance."
  (interactive)
  (save-excursion
    (when (not (use-region-p))
      (mark-defun))
    (stylish-repl-minor-mode-flash-region (region-beginning) (region-end)))
    (stylish-repl-send-region-to-stylish (region-beginning) (region-end)))

(defun stylish-repl-minor-mode-send-buffer ()
  "Send the text of the current buffer to the current Stylish REPL instance."
  (interactive)
  (stylish-repl-send-region-to-stylish (point-min) (point-max)))

;; C-x x snapping between REPL and code.  Idea stolen and improved
;; from my own term-extras.el.

(defvar stylish-repl-snapped-from nil)
(make-variable-buffer-local 'stylish-repl-snapped-from)

(defun stylish-repl-minor-mode-snap-to-repl ()
  "Move the window focus to the Stylish REPL window associted with this buffer."
  (interactive)
  (let* ((from (selected-window))
         (to   (stylish-repl-minor-mode-get-repl))
         (w    (get-buffer-window to)))
    (when w
      (with-current-buffer to
        (setq stylish-repl-snapped-from from)
        (select-window w)
        (goto-char (point-max))))))

(define-key stylish-repl-mode-map "\C-xx" #'stylish-repl-snap-from-repl)

(defun stylish-repl-snap-from-repl ()
  "Move the window focus back to the window that we snapped from."
  (interactive)
  (when (not stylish-repl-snapped-from)
    (error "Don't know where to go back to!"))
  (let ((repl-buf (current-buffer)))
    (select-window stylish-repl-snapped-from)
    (with-current-buffer repl-buf
      (setq stylish-repl-snapped-from nil))))

(provide 'stylish-repl-minor-mode)
;;; stylish-repl-minor-mode.el ends here
