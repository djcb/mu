;;; mu4e-message.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2012 Dirk-Jan C. Binnema

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

;; Functions to get data from mu4e-message plist structure

;;; Code:
(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)
(require 'html2text)
 

(defcustom mu4e-html2text-command nil
  "Shell command that converts HTML from stdin into plain text on
stdout. If this is not defined, the emacs `html2text' tool will be
used when faced with html-only message. If you use htmltext, it's
recommended you use \"html2text -utf8 -width 72\"."
  :type 'string
  :group 'mu4e-view
  :safe 'stringp)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the HTML-version of the
e-mail message (if there is any."
  :type 'boolean
  :group 'mu4e-view)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG. FIELD is one
of :from, :to, :cc, :bcc, :subject, :data, :message-id, :path, :maildir,
:priority, :attachments, :references, :in-reply-to, :body-txt, :body-html

A message plist looks something like:
\(:docid 32461
 :from ((\"Nikola Tesla\" . \"niko@example.com\"))
 :to ((\"Thomas Edison\" . \"tom@example.com\"))
 :cc ((\"Rupert The Monkey\" . \"rupert@example.com\"))
 :subject \"RE: what about the 50K?\"
 :date (20369 17624 0)
 :size 4337
 :message-id \"6BDC23465F79238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com\"
 :path  \"/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S\"
 :maildir \"/INBOX\"
 :priority normal
 :flags (seen)
 :attachments
     ((:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)
      (:index 3 :name \"book.pdf\" :mime-type \"application/pdf\" :size 192220))
 :references  (\"6BDC23465F79238C8384574032D81EE81AF0114E4E74@123213.mail.example.com\"
 \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\")
 :in-reply-to \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\"
 :body-txt \"Hi Tom, ...\"
\)).
Some  notes on the format:
- The address fields are lists of pairs (NAME . EMAIL), where NAME can be nil.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments. :body-txt or :body-html fields. Message in the
  Message view use the actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (plist-get msg field))





(defun mu4e-message-for-each (msg field func)
  "Call FUNC for each element in the field FIELD (which must be a
lists-type field). FUNC takes the element as its arg."
  (let ((lst (mu4e-message-field msg field)))
    (unless (listp lst)
      (error "Not a list type"))
    (dolist (elm (mu4e-message-field msg field))
      (funcall func elm))))

(defun mu4e-message-for-each-contact-field (msg field func)
  "Call FUNC for each element of contact
FIELD (:to, :cc, :bcc, :from). FUNC takes two args, strings
name (possibly nil) and an email address."
  (unless (member field '(:to :from :bcc :cc))
    (error "Not a contacts field"))
  (mu4e-message-for-each msg field
    (lambda (contact)
      (funcall func (car contact) (cdr contact)))))


(defun mu4e-message-body-text (msg)
  "Get the body in text form for this message, which is either :body-txt,
or if not available, :body-html converted to text. By default, it
uses the emacs built-in `html2text'. Alternatively, if
`mu4e-html2text-command' is non-nil, it will use that. Normally,
function prefers the text part, but this can be changed by setting
`mu4e-view-prefer-html'."
  (let* ((txt (mu4e-message-field msg :body-txt))
	  (html (mu4e-message-field msg :body-html))
	  (body
	    (cond
	      ;; does it look like some text? ie., 10x the length of the text
	      ;; should be longer than the html, an heuristic to guard against
	      ;; 'This messages requires html' text bodies.
	      ((and (> (* 10 (length txt)) (length html))
		 ;; use html if it's prefered, unless there is no html
		 (or (not mu4e-view-prefer-html) (not html)))
		txt)
	      ;; otherwise, it there some html?
	      (html
		(with-temp-buffer
		  (insert html)
		  ;; if defined, use the external tool
		  (if mu4e-html2text-command
		    (shell-command-on-region (point-min) (point-max)
		      mu4e-html2text-command nil t)
		    ;; otherwise...
		    (html2text))
		  (buffer-string)))
	      (t ;; otherwise, an empty body
		""))))
    ;; and finally, remove some crap from the remaining string; it seems
    ;; esp. outlook lies about its encoding (ie., it says 'iso-8859-1' but
    ;; really it's 'windows-1252'), thus giving us these funky chars. here, we
    ;; either remove them, or replace with 'what-was-meant' (heuristically)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward "[ ]" nil t)
	(replace-match
	  (cond
	    ((string= (match-string 0) "") "'")
	    (t		                       ""))))
      (buffer-string))))



(defsubst mu4e-message-part-field  (msgpart field)
  "Get some field in a message part; a part would look something like:
   (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get msgpart field))

(provide 'mu4e-message)
