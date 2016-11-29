;;; mu4e-message.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2012-2016 Dirk-Jan C. Binnema

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

(require 'mu4e-vars)
(require 'mu4e-utils)

(require 'cl)
(require 'html2text)


(defcustom mu4e-html2text-command
  (if (fboundp 'shr-insert-document) 'mu4e-shr2text 'html2text)

  "Either a shell command or a function that converts from html to plain text.

If it is a shell-command, the command reads html from standard
input and outputs plain text on standard output. If you use the
htmltext program, it's recommended you use \"html2text -utf8
-width 72\". Alternatives are the python-based html2markdown, w3m
and on MacOS you may want to use textutil.

It can also be a function, which takes the current buffer in html
as input, and transforms it into html (like the `html2text'
function).

In both cases, the output is expected to be in UTF-8 encoding.

Newer emacs has the shr renderer, and when it's available
conversion defaults to `mu4e-shr2text'; otherwise, the default is
emacs' built-in `html2text' function."
  :type '(choice string function)
  :group 'mu4e-view)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the html-version.
If the e-mail message has no html-version the plain-text version
is always used."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-html-plaintext-ratio-heuristic 5
  "Ratio between the length of the html and the plain text part
below which mu4e will consider the plain text part to be 'This
messages requires html' text bodies. You can neutralize
it (always show the text version) by using
`most-positive-fixnum'."
  :type 'integer
  :group 'mu4e-view)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst mu4e-message-field-raw (msg field)
  "Retrieve FIELD from message plist MSG.
FIELD is one of :from, :to, :cc, :bcc, :subject, :data,
:message-id, :path, :maildir, :priority, :attachments,
:references, :in-reply-to, :body-txt, :body-html

Returns `nil' if the field does not exist.

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
Some notes on the format:
- The address fields are lists of pairs (NAME . EMAIL), where NAME can be nil.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments, :body-txt or :body-html fields. Message in the
  Message view use the actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (if msg
    (plist-get msg field)
    (mu4e-error "message must be non-nil")))

(defsubst mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG.
Like `mu4e-message-field-nil', but will sanitize `nil' values:
- all string field except body-txt/body-html: nil -> \"\"
- numeric fields + dates                    : nil -> 0
- all others                                : return the value
Thus, function will return nil for empty lists, non-existing body-txt or body-html."
  (let ((val (mu4e-message-field-raw msg field)))
    (cond
      (val
	val)   ;; non-nil -> just return it
      ((member field '(:subject :message-id :path :maildir :in-reply-to))
	"")    ;; string fields except body-txt, body-html: nil -> ""
      ((member field '(:body-html :body-txt))
	val)
      ((member field '(:docid :size))
	0)     ;; numeric type: nil -> 0
      (t
	val)))) ;; otherwise, just return nil

(defsubst mu4e-message-has-field (msg field)
  "Return t if MSG contains FIELD, nil otherwise."
  (plist-member msg field))

(defsubst mu4e-message-at-point (&optional noerror)
  "Get the message s-expression for the message at point in either
the headers buffer or the view buffer, or nil if there is no such
message. If optional NOERROR is non-nil, do not raise an error when
there is no message at point."
  (let ((msg (or (get-text-property (point) 'msg) mu4e~view-msg)))
    (if msg
      msg
      (unless noerror (mu4e-warn "No message at point")))))

(defsubst mu4e-message-field-at-point (field)
  "Get the field FIELD from the message at point.
This is equivalent to:
  (mu4e-message-field (mu4e-message-at-point) FIELD)."
  (mu4e-message-field (mu4e-message-at-point) field))

(defvar mu4e~message-body-html nil
  "Whether the body text uses HTML.")


(defun mu4e~message-use-html-p (msg prefer-html)
  "Determine whether we want to use html or text; this is based
on PREFER-HTML and whether the message supports the given
representation."
  (let* ((txt (mu4e-message-field msg :body-txt))
	  (html (mu4e-message-field msg :body-html))
	  (txt-len (length txt))
	  (html-len (length html))
	  (txt-limit (* mu4e-view-html-plaintext-ratio-heuristic txt-len))
	  (txt-limit (if (>= txt-limit 0) txt-limit most-positive-fixnum)))
    (cond
      ; user prefers html --> use html if there is
      (prefer-html (> html-len 0))
      ;; otherwise (user prefers text) still use html if there is not enough
      ;; text
      ((< txt-limit html-len) t)
      ;; otherwise, use text
      (t nil))))


(defun mu4e-message-body-text (msg &optional prefer-html)
  "Get the body in text form for this message.
This is either :body-txt, or if not available, :body-html
converted to text, using `mu4e-html2text-command' is non-nil, it
will use that. Normally, this function prefers the text part,
unless PREFER-HTML is non-nil."
  (setq mu4e~message-body-html (mu4e~message-use-html-p msg prefer-html))
  (let ((body
	  (if mu4e~message-body-html
	    ;; use an HTML body
	    (with-temp-buffer
	      (insert (mu4e-message-field msg :body-html))
	      (cond
		((stringp mu4e-html2text-command)
		  (let* ((tmp-file (mu4e-make-temp-file "html")))
		    (write-region (point-min) (point-max) tmp-file)
		    (erase-buffer)
		    (call-process-shell-command mu4e-html2text-command tmp-file t t)
		    (delete-file tmp-file)))
		((functionp mu4e-html2text-command)
		  (funcall mu4e-html2text-command))
		(t (mu4e-error "Invalid `mu4e-html2text-command'")))
	      (setq mu4e~message-body-html t)
	      (buffer-string))
	    ;; use a text body
	    (or (mu4e-message-field msg :body-txt) ""))))
    ;; and finally, remove some crap from the remaining string; it seems
    ;; esp. outlook lies about its encoding (ie., it says 'iso-8859-1' but
    ;; really it's 'windows-1252'), thus giving us these funky chars. here, we
    ;; either remove them, or replace with 'what-was-meant' (heuristically)
    (with-temp-buffer
	   (insert body)
	   (goto-char (point-min))
	   (while (re-search-forward "[��]" nil t)
	     (replace-match
	       (cond
		 ((string= (match-string 0) "�") "'")
		 (t		                       ""))))
	   (buffer-string)))) 

(defun mu4e-message-contact-field-matches (msg cfield rx)
  "Checks whether any of the of the contacts in field
CFIELD (either :to, :from, :cc or :bcc, or a list of those) of
msg MSG matches (with their name or e-mail address) regular
expressions RX. If there is a match, return non-nil; otherwise
return nil. RX can also be a list of regular expressions, in
which case any of those are tried for a match."
  (if (and cfield (listp cfield))
    (or (mu4e-message-contact-field-matches msg (car cfield) rx)
      (mu4e-message-contact-field-matches msg (cdr cfield) rx))
    (when cfield
      (if (listp rx)
	;; if rx is a list, try each one of them for a match
	(find-if
	  (lambda (a-rx) (mu4e-message-contact-field-matches msg cfield a-rx))
	  rx)
	;; not a list, check the rx
	(find-if
	  (lambda (ct)
	    (let ((name (car ct)) (email (cdr ct)))
	      (or
		(and name  (string-match rx name))
		(and email (string-match rx email)))))
	  (mu4e-message-field msg cfield))))))

(defun mu4e-message-contact-field-matches-me (msg cfield)
  "Checks whether any of the of the contacts in field
CFIELD (either :to, :from, :cc or :bcc) of msg MSG matches *me*,
that is, any of the e-mail address in
`mu4e-user-mail-address-list'. Returns the contact cell that
matched, or nil."
  (find-if
    (lambda (cc-cell)
      (member-if
	(lambda (addr)
	  (string= (downcase addr) (downcase (cdr cc-cell))))
	mu4e-user-mail-address-list))
    (mu4e-message-field msg cfield)))

(defsubst mu4e-message-part-field  (msgpart field)
  "Get some field in a message part; a part would look something like:
  (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get msgpart field))

;; backward compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'mu4e-msg-field 'mu4e-message-field)
(defalias 'mu4e-body-text 'mu4e-message-body-text) ;; backward compatibility

(defun mu4e-field-at-point (field)
  "Get FIELD (a symbol, see `mu4e-header-info') for the message at
point in eiter the headers buffer or the view buffer."
  (plist-get (mu4e-message-at-point) field))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e-shr2text ()
  "Html to text using the shr engine; this can be used in
`mu4e-html2text-command' in a new enough emacs. Based on code by
Titus von der Malsburg."
  (interactive)
  (let (
	 ;; When HTML emails contain references to remote images,
	 ;; retrieving these images leaks information. For example,
	 ;; the sender can see when I openend the email and from which
	 ;; computer (IP address). For this reason, it is preferrable
	 ;; to not retrieve images.
	 ;; See this discussion on mu-discuss:
	 ;; https://groups.google.com/forum/#!topic/mu-discuss/gr1cwNNZnXo
	 (shr-inhibit-images t))
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))))

(provide 'mu4e-message)
