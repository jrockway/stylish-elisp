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
    (define-key map "\C-\M-x"  'stylish-repl-minor-mode-send-defun)
    map))

(define-minor-mode stylish-repl-minor-mode
"Send code from the current buffer to a Stylish REPL.

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}"
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
  "Send the current function to the Stylish REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (stylish-repl-minor-mode-flash-region (point) (mark))
    (stylish-repl-send-region-to-stylish (point) (mark))))

(provide 'stylish-repl-minor-mode)
;;; stylish-repl-minor-mode.el ends here
