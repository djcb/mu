;;; mu4e-raw-view.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:
(eval-when-compile (require 'cl))

(require 'mu4e-utils)    ;; utility functions

;; raw mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some buffer-local variables
(defconst mu4e-raw-view-buffer-name "*mu4e-raw-view*"
  "*internal* Name for the raw message view buffer")

(defvar mu4e-raw-view-buffer nil "*internal* The raw view buffer.")

(defvar mu4e-raw-view-mode-map nil
  "Keymap for \"*mu4e-raw-view*\" buffers.")

(unless mu4e-raw-view-mode-map
  (setq mu4e-raw-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mu4e-raw-view-quit-buffer)
      (define-key map "." 'mu4e-raw-view-quit-buffer)

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'scroll-up)
      (define-key map (kbd "<home>")
	'(lambda () (interactive) (goto-char (point-min))))
      (define-key map (kbd "<end>")
	'(lambda () (interactive) (goto-char (point-max))))
      (define-key map (kbd "RET")
	'(lambda () (interactive) (scroll-up 1)))
      (define-key map (kbd "<backspace>")
	'(lambda () (interactive) (scroll-up -1)))
      map)))

(fset 'mu4e-raw-view-mode-map mu4e-raw-view-mode-map)

(define-derived-mode mu4e-raw-view-mode special-mode
  "mu4e:raw"
  "Major mode for viewing of raw e-mail message in mu4e.
\\{mu4e-raw-view-mode-map}.")


(defun mu4e-raw-view-message (msg view-buffer)
  "Display the raw contents of message MSG in a new buffer."
  (let ((buf (get-buffer-create mu4e-raw-view-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents file)
      ;; initialize view-mode
      (mu4e-raw-view-mode)
      (setq mu4e-raw-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-view-shell-command-on-raw-message (msg view-buffer cmd)
  "Process the raw message with shell command CMD."
  (let ((buf (get-buffer-create mu4e-raw-view-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (process-file-shell-command cmd file buf)
      (mu4e-raw-view-mode)
      (setq mu4e-raw-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-raw-view-quit-buffer ()
  "Quit the raw view and return to the message."
  (interactive)
  (kill-buffer))

(provide 'mu4e-raw-view)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
