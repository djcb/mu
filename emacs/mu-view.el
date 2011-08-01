;;; mu-view.el -- use `mu' from emacs
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

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

;; mu message has functions to display a message

;;; Code:

(require 'mu-common)

(defvar mu-view-header-fields
  '( :from
     :to
     :subject
     :date)
  "list of header fields to display in the message view")

(defconst mu-view-buffer-name " *mu-view*")

(defun mu-view-get (path)
  "display the email message at PATH"
  (let* ((cmd (concat mu-binary " view --format=sexp " path))
	  (str (shell-command-to-string cmd))
	  (msglst (read-from-string str)))
    (when msglst (car msglst))))

(defun mu-view-header (field val val-face)
  "get a header string (like 'Subject: foo')"
  (when val
    (concat (propertize field 'face 'mu-header-face) ": "
      (propertize val 'face val-face) "\n")))

(defun mu-view-header-contact (field lst face)
  (when lst
    (let* ((header (concat (propertize field 'face 'mu-header-face) ": "))
	    (val (mapconcat (lambda(c)
			      (propertize (or (car c) (cdr c) "?") 'face face))
		   lst ",")))
      (concat header val "\n"))))

(defun mu-view-body (msg face)
  "view the body; try text first, if that does not work, try html"
  (cond
    ((plist-get msg :body-txt)  (propertize (plist-get msg :body-txt) 'face face))
    ((plist-get msg :body-html)
      (propertize
	(with-temp-buffer
	  (insert (plist-get msg :body-html))
	  (html2text)
	  (buffer-string))'face face))
    (t "")))
    
(defun mu-view-message (path)
  "display the email message at PATH"
  (let ((msg (mu-view-get path)))
    (when msg
      (concat
	(mapconcat
	  (lambda (field)
	    (case field
	      (:from (mu-view-header-contact "From"
		       (plist-get msg :from) 'mu-from-face))
	      (:to
		(mu-view-header-contact "To" (plist-get msg :to) 'mu-to-face))
	      (:cc
		(mu-view-header-contact "Cc" (plist-get msg :cc)  'mu-to-face))
	      (:bcc
		(mu-view-header-contact "Bcc" (plist-get msg :bcc) 'mu-to-face))
	      (:subject
		(mu-view-header "Subject" (plist-get msg :subject) 'mu-subject-face))
	      (:date
		(mu-view-header "Date"
		  (format-time-string mu-date-format-long
		    (plist-get msg :date)) 'mu-date-face))))
	  mu-view-header-fields "")
	"\n"
	(mu-view-body msg 'mu-body-face)
      ))))

(defun mu-view (path)
  "display message at PATH in a new buffer"
  (interactive)
  (let ((str (mu-view-message path))
	 (buf (get-buffer mu-view-buffer-name))) 
    (when str
      (when buf (kill-buffer buf))
      (get-buffer-create mu-view-buffer-name)
      (with-current-buffer mu-view-buffer-name
	(let ((inhibit-read-only t)) (insert str))
	(switch-to-buffer mu-view-buffer-name)
	(mu-view-mode)
	(goto-char (point-min))))))

(defun mu-view-mode ()
  "major mode for viewing an e-mail message"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu-view-mode-map)
  (setq major-mode 'mu-view-mode mode-name "*mu-view*")
  (setq truncate-lines t buffer-read-only t))

(defvar mu-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mu-view-quit)
    (define-key map "s" 'mu-find)
    (define-key map "n" 'mu-view-next)
    (define-key map "p" 'mu-view-prev)  
    map)
  "Keymap for \"mu-view\" buffers.")
(fset 'mu-view-mode-map mu-view-mode-map)

(defun mu-view-quit ()
  "kill this headers buffer"
  (interactive)
  (when (equalp major-mode 'mu-view-mode)
    (kill-buffer)
    (if (get-buffer mu-find-buffer-name)
      (switch-to-buffer mu-find-buffer-name))))

(defun mu-view-next ()
  (interactive)
  (with-current-buffer mu-find-buffer-name
    (when (mu-find-next)
      (mu-view (mu-find-get-path)))))

(defun mu-view-prev ()
  (interactive)
  (with-current-buffer mu-find-buffer-name
    (when (mu-find-prev)
      (mu-view (mu-find-get-path)))))

(provide 'mu-view)
