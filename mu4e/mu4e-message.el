;;; mu4e-message.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions to get data from mu4e-message plist structure

;;; Code:

(require 'cl-lib)
(require 'mu4e-vars)
(require 'flow-fill)
(require 'shr)

(declare-function mu4e-error "mu4e-utils")
(declare-function mu4e-warn  "mu4e-utils")
(declare-function mu4e-personal-address-p "mu4e-utils")
(declare-function mu4e-make-temp-file  "mu4e-utils")

(defvar mu4e~view-message)
(defvar shr-inhibit-images)
 
(make-obsolete-variable 'mu4e-html2text-command "No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-view-prefer-html "No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-view-html-plaintext-ratio-heuristic
			"No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-message-body-rewrite-functions
			"No longer in use" "1.7.0")
 
;;; Message fields

(defsubst mu4e-message-field-raw (msg field)
  "Retrieve FIELD from message plist MSG.
FIELD is one of :from, :to, :cc, :bcc, :subject, :data,
:message-id, :path, :maildir, :priority, :attachments,
:references, :in-reply-to, :body-txt, :body-html

Returns nil if the field does not exist.

A message plist looks something like:
\(:docid 32461
 :from ((\"Nikola Tesla\" . \"niko@example.com\"))
 :to ((\"Thomas Edison\" . \"tom@example.com\"))
 :cc ((\"Rupert The Monkey\" . \"rupert@example.com\"))
 :subject \"RE: what about the 50K?\"
 :date (20369 17624 0)
 :size 4337
 :message-id \"238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com\"
 :path  \"/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S\"
 :maildir \"/INBOX\"
 :priority normal
 :flags (seen)
 :attachments
     ((:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)
      (:index 3 :name \"book.pdf\" :mime-type \"application/pdf\" :size 192220))
 :references  (\"238C8384574032D81EE81AF0114E4E74@123213.mail.example.com\"
 \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\")
 :in-reply-to \"238203498230942D81EE81AF0114E4E74@123213.mail.example.com\"
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
    (mu4e-error "Message must be non-nil")))

(defsubst mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG.
Like `mu4e-message-field-nil', but will sanitize nil values:
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
  "If MSG has a FIELD return t, nil otherwise."
  (plist-member msg field))

(defsubst mu4e-message-at-point (&optional noerror)
  "Get the message s-expression for the message at point.
Either the headers buffer or the view buffer, or nil if there is
no such message. If optional NOERROR is non-nil, do not raise an
error when there is no message at point."
  (let ((msg (or (get-text-property (point) 'msg) mu4e~view-message)))
    (if msg
        msg
      (unless noerror (mu4e-warn "No message at point")))))

(defsubst mu4e-message-field-at-point (field)
  "Get the field FIELD from the message at point.
This is equivalent to:
  (mu4e-message-field (mu4e-message-at-point) FIELD)."
  (mu4e-message-field (mu4e-message-at-point) field))

(defun mu4e-message-body-text (_msg &optional _prefer-html)
  "Get the body in text form for message MSG."
  "" ;; not implemented for Gnus mode.
)
  

(defun mu4e-message-contact-field-matches (msg cfield rx)
  "Does MSG's contact-field CFIELD match rx?
Check if any of the of the CFIELD in MSG matches RX. I.e.
anything in field CFIELD (either :to, :from, :cc or :bcc, or a
list of those) of msg MSG matches (with their name or e-mail
address) regular expressions RX. If there is a match, return
non-nil; otherwise return nil. RX can also be a list of regular
expressions, in which case any of those are tried for a match."
  (if (and cfield (listp cfield))
      (or (mu4e-message-contact-field-matches msg (car cfield) rx)
          (mu4e-message-contact-field-matches msg (cdr cfield) rx))
    (when cfield
      (if (listp rx)
          ;; if rx is a list, try each one of them for a match
          (cl-find-if
           (lambda (a-rx) (mu4e-message-contact-field-matches msg cfield a-rx))
           rx)
        ;; not a list, check the rx
        (cl-find-if
         (lambda (ct)
           (let ((name (car ct)) (email (cdr ct))
                 ;; the 'rx' may be some `/rx/` from mu4e-personal-addresses;
                 ;; so let's detect and extract in that case.
                 (rx (if (string-match-p  "^\\(.*\\)/$" rx)
                         (substring rx  1 -1) rx)))
             (or
              (and name  (string-match rx name))
              (and email (string-match rx email)))))
         (mu4e-message-field msg cfield))))))

(defun mu4e-message-contact-field-matches-me (msg cfield)
  "Does contact-field CFIELD in MSG match me?
Checks whether any
of the of the contacts in field CFIELD (either :to, :from, :cc or
:bcc) of msg MSG matches *me*, that is, any of the addresses for
which `mu4e-personal-address-p' return t. Returns the contact
cell that matched, or nil."
  (cl-find-if (lambda (cell) (mu4e-personal-address-p (cdr cell)))
                (mu4e-message-field msg cfield)))

(defun mu4e-message-sent-by-me (msg)
  "Is this MSG (to be) sent by me?
Checks if the from field matches user's personal addresses."
  (mu4e-message-contact-field-matches-me msg :from))

(defun mu4e-message-personal-p (msg)
  "Does MSG have user's personal address?
In any of the contact
 fields?"
  (cl-some
   (lambda (field)
     (mu4e-message-contact-field-matches-me msg field))
   '(:from :to :cc :bcc)))

(defsubst mu4e-message-part-field (msgpart field)
  "Get some FIELD from MSGPART.
A part would look something like:
  (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get msgpart field))

;; backward compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'mu4e-msg-field 'mu4e-message-field)
(defalias 'mu4e-body-text 'mu4e-message-body-text) ;; backward compatibility

(defun mu4e-field-at-point (field)
  "Get FIELD for the message at point.
Either in the headers buffer or the view buffer. Field is a
symbol, see `mu4e-header-info'."
  (plist-get (mu4e-message-at-point) field))

;;; Html2Text
(make-obsolete 'mu4e-shr2text "No longer in use" "1.7.0")

(defun mu4e-copy-message-path ()
  "Copy the message-path of message at point to the kill ring."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path)))
    (kill-new path)
    (mu4e-message "Saved '%s' to kill-ring" path)))

;;; _
(provide 'mu4e-message)
;;; mu4e-message.el ends here
