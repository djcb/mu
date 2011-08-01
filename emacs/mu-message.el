;;; mu-message.el -- use `mu' from emacs
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

;; mu-message contains code to generate a message for composing, replying or
;; forwarding

;;; Code:

(defvar mu-message-citation-prefix " > "
  "string to prefix cited message parts with")

(defvar mu-message-reply-prefix "Re:"
  "string to prefix the subject of replied messages with")

(defvar mu-message-forward-prefix "Fwd:"
  "string to prefix the subject of forwarded messages with")


(defun mu-message-user-agent ()
  (format "mu %s; emacs %s" (mu-binary-version) emacs-version))7

(defun mu-message-attribution (msg)
  "get an attribution line for a quoted message"
  (format "On %s, %s wrote:\n"
    (format-time-string mu-date-format-long (plist-get msg :date))
    (cdr (car (plist-get msg :from)))))
  
(defun mu-message-cite (msg)
  "cite an existing message"
  (let ((body
	  (or (plist-get msg :body-txt)
	    (let ((html (plist-get msg :body-html)))
	      (when html
		(with-temp-buffer (insert html) (html2text) (buffer-string))))
	    "")))
    (replace-regexp-in-string "^" " > " body)))


(defun mu-message-hidden-header (hdr val)
  "return user-invisible header to the message (HDR: VAL\n)"
  (propertize (format "%s: %s\n" hdr val) 'invisible t))


(defun mu-message-reply-or-forward (path &optional forward)
  "create a reply to the message at PATH; if FORWARD is non-nil,
create a forwarded message. After creation, switch to the message editor"
  (let* ((cmd (concat mu-binary " view --format=sexp " path))
	  (str (shell-command-to-string cmd))
	  (msglst (read-from-string str))
	  (msg (car msglst))
	  (buf (get-buffer-create (generate-new-buffer-name "*mu-draft*"))))
    (with-current-buffer buf
      (insert
	(format "From: %s <%s>\n" user-full-name user-mail-address)
	(mu-message-hidden-header "User-agent" (mu-message-user-agent)))

      (when mail-reply-to
	(insert (format "Reply-To: %s\n" mail-reply-to)))

      (if forward
	(insert
	  "To:\n"
	  "Subject: " mu-message-forward-prefix (plist-get msg :subject) "\n")
	(insert 
	  "To: " (car (car (plist-get msg :from))) "\n"
	  "Subject: " mu-message-reply-prefix (plist-get msg :subject) "\n"))

      (insert
	"--text follows this line--\n\n"
	(mu-message-attribution msg)
	(mu-message-cite msg))
      
      (when mail-signature (insert mail-signature))
      
      (message-mode)

      (if forward
	(message-goto-to)
	(message-goto-body))

    (switch-to-buffer buf))))


(defun mu-message-reply (path)
  "create a reply to the message at PATH; After creation, switch
to the message editor"
  (mu-message-reply-or-forward path))

(defun mu-message-forward (path)
  "create a forward-message to the message at PATH; After
creation, switch to the message editor"
  (mu-message-reply-or-forward path t))
