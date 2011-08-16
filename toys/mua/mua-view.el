;;; mua-view.el -- part of mua, the mu mail user agent
;;
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

;; mu

;;; Code:


(eval-when-compile (require 'cl))

(require 'mua-common)
(require 'mua-msg)

(defconst mua/view-buffer-name " *mua-view*"
  "buffer name for mua/view buffers")

(defvar mua/view-headers
  '(:from :to :cc :subject :flags :date :maildir :path  :attachments)
 "Fields to display in the message view buffer.")

(defvar mua/hdrs-buffer nil
  "Headers buffer for the view in this buffer.")

(defvar mua/view-uid nil
  "The UID for the message being viewed in this buffer.")


(defun mua/view (uid headersbuf)
  "display message identified by UID in a new buffer. Note that
the action of viewing a message may cause it to be moved/renamed;
this function returns the resulting name. PARENTBUF refers to the
buffer who invoked this view; this allows us to return there when
we quit from this view. Also, if PARENTBUF is a find buffer (ie.,
has mu-headers-mode as its major mode), this allows various
commands (navigation, marking etc.) to be applied to this
buffer.

For the reasoning to use UID here instead of just the path, see
`mua/msg-file-map'.
"
  (let* ((path (mua/msg-file-get-path uid))
	  (sexp (and path (mua/mu-view-sexp path)))
	  (msg (and sexp (mua/msg-from-string sexp))))
    (if (not msg)
      (mua/warn "Cannot view message %S %S" uid path)
      (progn
	(switch-to-buffer (get-buffer-create mua/view-buffer-name))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert (mua/view-message msg)))
	
	(mua/view-mode)
	
	(setq ;; these are buffer-local
	  mua/view-uid uid
	  mua/hdrs-buffer headersbuf
	  mua/parent-buffer headersbuf)
	
	(goto-char (point-min))
	(mua/msg-file-mark-as-read uid)))))

(defun mua/view-message (msg)
  "construct a display string for the message"
  (let ((hdrs
	  (mapconcat
	   (lambda (field)
	     (case field
	       (:subject	(mua/view-header  msg "Subject" :subject))
	       (:path		(mua/view-header  msg "Path" :path))
	       (:to		(mua/view-contacts msg field))
	       (:from		(mua/view-contacts msg field))
	       (:cc		(mua/view-contacts msg field))
	       (:bcc		(mua/view-contacts msg field))
	       (:date		(mua/view-date  msg))
	       (:flags		(mua/view-flags msg))
	       (:maildir	(mua/view-header msg "Maildir" :maildir))
	       (:size		(mua/view-size  msg))
	       (:attachments    (mua/view-attachments msg))
	       (t               (error "Unsupported field: %S" field))))
	   mua/view-headers ""))
	 (body (mua/msg-body-txt-or-html msg)))
    (concat hdrs "\n" body)))

(defun mua/view-header-string (key val face)
  (if val
    (concat
      (propertize key 'face 'mua/header-title-face) ": "
      (propertize val 'face face) "\n")
    ""))
  
(defun mua/view-header (msg key field)
  "show header FIELD for MSG with KEY. ie. <KEY>: value-of-FIELD\n"
  (mua/view-header-string key (mua/msg-field msg field) 'mua/header-face))

(defun mua/view-contacts (msg field)
  (unless (member field '(:to :from :bcc :cc))
    (error "Illegal type for contact"))
  (let* ((lst (mua/msg-field msg field))
	  (contacts
	    (when lst
	      (mapconcat
		(lambda(c) (let ((name (car c)) (email (cdr c)))
			     (if name
			       (format "%s <%s>" name email)
			       (format "%s" email)))) lst ", "))))
    (if contacts
      (mua/view-header-string
	(case field (:to "To") (:from "From") (:bcc "Bcc") (:cc "Cc"))
	contacts 'mua/contacts-face)
      "")))

(defun mua/view-date (msg)
  (let* ((date (mua/msg-field msg :date))
	  (datestr (when date (format-time-string "%c" date))))
    (mua/view-header-string "Date" datestr 'mua/header-face)))

(defun mua/view-size (msg)
  (let* ((size (mua/msg-field msg :size))
	  (sizestr (when size (format "%d bytes"))))
    (mua/view-header-string "Size" sizestr 'mua-header-face)))

(defun mua/view-flags (msg)
  ""
  "" ;; todo
)

(defun mua/view-attachments (msg)
  ""
  "" ;; todo
)
  

(defvar mua/view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mua/quit-buffer)
    (define-key map "s" 'mua/view-search)

    (define-key map "f" 'mua/view-forward)
    (define-key map "r" 'mua/view-reply)
    (define-key map "c" 'mua/view-compose)
    
    ;; navigation between messages
    (define-key map "n" 'mua/view-next)
    (define-key map "p" 'mua/view-prev)
    
    ;; marking/unmarking
    (define-key map "d" '(lambda()(interactive)(mua/view-mark 'trash)))
    (define-key map "D" '(lambda()(interactive)(mua/view-mark 'delete)))
    (define-key map "m" '(lambda()(interactive)(mua/view-mark 'move)))
    (define-key map "u" '(lambda()(interactive)(mua/view-mark 'unmark)))
    (define-key map "x" 'mua/view-marked-execute)    
    map)
  "Keymap for \"*mua-view*\" buffers.")
(fset 'mua/view-mode-map mua/view-mode-map)

(defun mua/view-mode ()
  "major mode for viewing an e-mail message"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mua/view-mode-map)
  
  (make-local-variable 'mua/parent-buffer)
  (make-local-variable 'mua/hdrs-buffer)
  (make-local-variable 'mua/view-uid)
  
  (setq major-mode 'mua/view-mode mode-name "*mu-view*")
  (setq truncate-lines t buffer-read-only t))


(defmacro mua/with-hdrs-buffer (&rest body)
  "Execute the forms in BODY with the mua/hdrs-buffer temporarily current.
Note that this actually switches the buffer, and changes to point
etc. persist."
  (declare (indent 1) (debug t))
  `(let ((oldbuf (current-buffer)))
     (if (buffer-live-p mua/hdrs-buffer)
       (progn
	 (set-buffer mua/hdrs-buffer)
	 (progn ,@body)
	 (set-buffer oldbuf))
       (mua/warn "hdrs buffer is dead"))))


(defun mua/view-mark (action)
  "Set/unset marks for the current message."
  (interactive)
  (mua/with-hdrs-buffer (mua/hdrs-mark action)))

(defun mua/view-marked-execute ()
  "Warn user that marks cannot be executed from here (for his/her
own safety)."
  (interactive)
  (mua/warn "You cannot execute marks from here"))


(defun mua/view-search()
  "Start a new search."
  (interactive)
  (mua/with-hdrs-buffer
    (call-interactively 'mua/hdrs-search)))

(defun mua/view-next ()
  "move to the next message; note, this will replace the current
buffer"
  (interactive)
  (mua/with-hdrs-buffer
    (when (mua/hdrs-next) (mua/hdrs-view))))

(defun mua/view-prev ()
  "move to the previous message; note, this will replace the
current buffer"
  (interactive)
  (mua/with-hdrs-buffer
    (when (mua/hdrs-prev) (mua/hdrs-view))))

(defun mua/view-reply ()
  "Reply to the current message."
  (interactive) (mua/with-hdrs-buffer (mua/hdrs-reply)))
  
(defun mua/view-forward ()
  "Reply to the current message."
  (interactive) (mua/with-hdrs-buffer (mua/hdrs-forward)))

(defun mua/view-compose ()
  "Write a new message."
  (interactive) (mua/with-hdrs-buffer (mua/hdrs-compose)))


(provide 'mua-view)
