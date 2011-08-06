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
(require 'mu-common)

(defvar mu-message-citation-prefix "> "
  "string to prefix cited message parts with")

(defvar mu-message-reply-prefix "Re:"
  "string to prefix the subject of replied messages with")

(defvar mu-message-forward-prefix "Fwd:"
  "string to prefix the subject of forwarded messages with")

(defun mu-message-user-agent ()
  (format "mu %s; emacs %s" (mu-binary-version) emacs-version))

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

(defun mu-message-recipients-remove (lst email-to-remove)
  "remove the recipient with EMAIL from the recipient list (of
form '( (\"A\" . \"a@example.com\") (\"B\" . \"B@example.com\"))"
  (remove-if (lambda (name-email)
	       (string= email-to-remove (downcase (cdr name-email))))
    lst))

(defun mu-message-recipients-to-string (lst)
  "convert a recipient list (of form '( (\"A\"
. \"a@example.com\") (\"B\" . \"B@example.com\") into a string
useful for from/to headers"
  (message "recips: %S" lst)
  (mapconcat
    (lambda (recip)
      (let ((name (car recip)) (email (cdr recip)))
	(format "%s <%s>" (or name "") email))) lst ", "))

(defun mu-message-hidden-header (hdr val)
  "return user-invisible header to the message (HDR: VAL\n)"
  (propertize (format "%s: %s\n" hdr val) 'invisible t))

(defun mu-message-reply (path)
  "create a reply to the message at PATH. After creation, switch
to the message editor"
  (let* ((cmd (concat mu-binary " view --format=sexp " path))
	  (str (shell-command-to-string cmd))
	  (msg (car (read-from-string str)))
	  (buf (get-buffer-create
		 (generate-new-buffer-name "*mu-draft*")))
	  (to-lst (mu-message-recipients-remove
		    (append (plist-get msg :from) (plist-get msg :to))
		    user-mail-address))
	  (cc-lst (mu-message-recipients-remove (plist-get msg :cc)
		    user-mail-address)))
    
    (with-current-buffer buf
      (insert
	(format "From: %s <%s>\n" user-full-name user-mail-address)
	(mu-message-hidden-header "User-agent" (mu-message-user-agent))
	(if (boundp 'mail-reply-to) (insert (format "Reply-To: %s\n"
					      mail-reply-to)) "")
	(format "To: %s\n" (if to-lst (mu-message-recipients-to-string to-lst) ""))
	(if cc-lst
	  (format "Cc: %s\n" (mu-message-recipients-to-string cc-lst)))
	"Subject: " mu-message-reply-prefix (plist-get msg :subject) "\n"
	"--text follows this line--\n\n"
	  
	(mu-message-attribution msg)
	(mu-message-cite msg)))
    
    (switch-to-buffer buf)
    (message-mode)
    (message-goto-body)))


(defun mu-message-forward (path)
  "create a forward to the message at PATH. After creation, switch
to the message editor"
  (let* ((cmd (concat mu-binary " view --format=sexp " path))
	  (str (shell-command-to-string cmd))
	  (msg (car (read-from-string str)))
	  (buf (get-buffer-create
		 (generate-new-buffer-name "*mu-draft*"))))
	  
    (with-current-buffer buf
      (insert
	(format "From: %s <%s>\n" user-full-name user-mail-address)
	(mu-message-hidden-header "User-agent" (mu-message-user-agent))
	"To: \n" 
	"Subject: " mu-message-forward-prefix (plist-get msg :subject) "\n"
	"--text follows this line--\n\n"
	  
	(mu-message-attribution msg)
	(mu-message-cite msg)))
    
    (switch-to-buffer buf)
    (message-mode)
    (message-goto-to)))

(defun mu-message-move (src targetdir)
  "move message at PATH using 'mu mv'; if targetdir is
'/dev/null', move immediately. Return t if succeeded, nil
otherwise"
  (let* ((cmd (concat
		mu-binary " mv --printtarget "
		(shell-quote-argument src) " "
		(shell-quote-argument targetdir)))
	  (fulltarget (shell-command-to-string cmd)))
    (mu-log cmd)
    
    (mu-log
      (if fulltarget (concat "Message has been moved to " fulltarget)
	"Message moving failed"))
    ;; now, if saving worked, anynchronously try to update the database
    (when fulltarget
      (mu-log "Removing from database: %s" src)
      (start-process " *mu-remove*" nil mu-binary "remove" src)
      
      (if (string= targetdir "/dev/null")
	t
	(mu-log "Adding to database: %s" fulltarget)
	(start-process " *mu-add*" nil mu-binary "add" fulltarget) t))))

;; note, we don't check the result of the db output

(provide 'mu-message)
