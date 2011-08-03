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

;; mu message has functions to display a single message

;;; Code:

(require 'mu-common)

(defvar mu-view-header-fields
  '( :from
     :to
     :subject
     :date
     :path)
  "list of header fields to display in the message view")

(defconst mu-view-buffer-name " *mu-view*")
(defvar mu-view-headers-buffer nil "the headers buffer (if any)
from which this buffer was invoked (buffer local)")

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
	  (buffer-string)) 'face face))
    (t "")))
    
(defun mu-view-message (path)
  "display the email message at PATH"
  (let ((msg (mu-get-message path)))
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
	      (:path
		(mu-view-header "Path" (plist-get msg :path) 'mu-path-face))
	      (:date
		(mu-view-header "Date"
		  (format-time-string mu-date-format-long
		    (plist-get msg :date)) 'mu-date-face))))
	  mu-view-header-fields "")
	"\n"
	(mu-view-body msg 'mu-body-face)
      ))))

;; note: mu-view sets path as a text-property ('path) for the whole buffer, just
;; like mu-headers does it per-header
(defun mu-view (path parentbuf)
  "display message at PATH in a new buffer; note that the action
of viewing a message may cause it to be moved/renamed; this
function returns the resulting name. PARENTBUF refers to the
buffer who invoked this view; this allows us to return there when
we quit from this view. Also, if PARENTBUF is a find buffer (ie.,
has mu-headers-mode as its major mode), this allows various
commands (navigation, marking etc.) to be applied to this
buffer."
  (let ((str (mu-view-message path))
	 (buf (mu-get-new-buffer mu-view-buffer-name)))
    (when str
      (with-current-buffer buf	
    
	(let ((inhibit-read-only t))
	  ;; note, we set the path as a text-property
	  (insert (propertize str 'path path))))
      
      (switch-to-buffer buf)
      (mu-view-mode)

      ;; these are buffer-local
      (setq mu-parent-buffer parentbuf)
      (setq mu-view-headers-buffer parentbuf)

      (goto-char (point-min)))))

(defvar mu-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mu-quit-buffer)
    (define-key map "s" 'mu-headers)
    (define-key map "f" 'mu-forward)
    (define-key map "r" 'mu-reply)

    ;; navigation between messages
    (define-key map "n" 'mu-view-next)
    (define-key map "p" 'mu-view-prev)

    ;; marking/unmarking
    (define-key map "d" 'mu-view-mark-for-trash)
    (define-key map "D" 'mu-view-mark-for-deletion)
    (define-key map "m" 'mu-view-mark-for-move)
    (define-key map "u" 'mu-view-unmark)
    (define-key map "x" 'mu-view-marks-execute)
    
    map)
  "Keymap for \"mu-view\" buffers.")
(fset 'mu-view-mode-map mu-view-mode-map)

(defun mu-view-mode ()
  "major mode for viewing an e-mail message"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu-view-mode-map)
  (make-variable-buffer-local 'mu-parent-buffer)
  (make-variable-buffer-local 'mu-headers-buffer)
  (setq major-mode 'mu-view-mode mode-name "*mu-view*")
  (setq truncate-lines t buffer-read-only t))

(defmacro with-current-headers-buffer (&rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(if (and mu-view-headers-buffer (buffer-live-p mu-view-headers-buffer))
     (save-current-buffer
       (set-buffer mu-view-headers-buffer)
       ,@body)
     (message "No headers-buffer connected")))

(defun mu-view-next ()
  "move to the next message"
  (interactive)
  (with-current-headers-buffer
    (when (mu-headers-next)
      (mu-view (mu-get-path) (current-buffer)))))

(defun mu-view-prev ()
  "move to the previous message"
  (interactive)
  (with-current-headers-buffer
    (when (mu-headers-prev)
      (mu-view (mu-get-path) (current-buffer)))))

(defun mu-view-mark-for-trash ()
  "mark for thrashing"
  (interactive)
  (with-current-headers-buffer
    (mu-headers-mark 'trash)))

(defun mu-view-mark-for-deletion ()
  "mark for deletion"
  (interactive)
  (with-current-headers-buffer
    (mu-headers-mark 'delete)))

(defun mu-view-mark-for-move ()
  "mark for moving"
  (interactive)
    (with-current-headers-buffer
      (mu-headers-mark 'move)))

(defun mu-view-unmark ()
  "unmark this message"
  (interactive)
  (with-current-headers-buffer
    (mu-headers-mark 'none)))

;; we don't allow executing marks from the view buffer, to protect user from
;; accidentally deleting stuff...
(defun mu-view-marks-execute ()
  "give user a warning"
  (interactive)
  (message "Please go back to the headers list to execute your marks"))
  
(provide 'mu-view)
