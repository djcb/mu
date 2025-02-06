;;; mu4e-message.el --- Working with mu4e-message plists -*- lexical-binding: t -*-

;; Copyright (C) 2012-2025 Dirk-Jan C. Binnema

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

(require 'mu4e-vars)
(require 'mu4e-contacts)
(require 'mu4e-window)
(require 'mu4e-helpers)
(require 'flow-fill)
(require 'shr)
(require 'pp)


(declare-function mu4e-determine-attachment-dir  "mu4e-helpers")
(declare-function mu4e-personal-address-p "mu4e-contacts")

;;; Message fields

(defun mu4e-message-field-raw (msg field)
  "Retrieve FIELD from message plist MSG.

See \"mu fields\" for the full list of field, in particular the
\"sexp\" column.

Returns nil if the field does not exist.

A message plist looks something like:
\(:docid 32461
 :from ((:name \"Nikola Tesla\" :email \"niko@example.com\"))
 :to ((:name \"Thomas Edison\" :email \"tom@example.com\"))
 :cc ((:name \"Rupert The Monkey\" :email \"rupert@example.com\"))
 :subject \"RE: what about the 50K?\"
 :date (20369 17624 0)
 :size 4337
 :message-id \"238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com\"
 :path  \"/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S\"
 :maildir \"/INBOX\"
 :priority normal
 :flags (seen)
\)).
Some notes on the format:
- The address fields are lists of plist (:name NAME :email EMAIL),
  where the :name part can be absent. The `mu4e-contact-name' and
  `mu4e-contact-email' accessors can be useful for this.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments or :body fields. Message in the Message view use the
  actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (if msg
      (plist-get msg field)
    (mu4e-error "Message must be non-nil")))

(defun mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG.
Like `mu4e-message-field-nil', but will sanitize nil values:
- all string field except body: nil -> \"\"
- numeric fields + dates      : nil -> 0
- all others                  : return the value
Thus, function will return nil for empty lists, or non-existing body."
  (let ((val (mu4e-message-field-raw msg field)))
    (cond
     (val
      val)   ;; non-nil -> just return it
     ((member field '(:subject :message-id :path :maildir :in-reply-to))
      "")    ;; string fields except body: nil -> ""
     ((member field '(:body))
      val)
     ((member field '(:docid :size))
      0)     ;; numeric type: nil -> 0
     (t
      val)))) ;; otherwise, just return nil

(defun mu4e-message-has-field (msg field)
  "If MSG has a FIELD return t, nil otherwise."
  (plist-member msg field))

(defun mu4e-message-at-point (&optional noerror)
  "Get the message s-expression for the message at point.
Either the headers buffer or the view buffer, or nil if there is
no such message. If optional NOERROR is non-nil, do not raise an
error when there is no message at point."
  (or (cond
       ((eq major-mode 'mu4e-headers-mode) (get-text-property (point) 'msg))
       ((eq major-mode 'mu4e-view-mode) mu4e--view-message))
      (unless noerror (mu4e-warn "No message at point"))))

(defun mu4e-message-p ()
  "Are we on a message?
Either in headers or view mode."
  (mu4e-message-at-point 'no-error))

(defsubst mu4e-message-field-at-point (field)
  "Get the field FIELD from the message at point.
This is equivalent to:
  (mu4e-message-field (mu4e-message-at-point) FIELD)."
  (mu4e-message-field (mu4e-message-at-point) field))

(defun mu4e-message-contact-field-matches (msg cfield rx)
  "Does MSG's contact-field CFIELD match regexp RX?
Check if any of the of the CFIELD in MSG matches RX. I.e.
anything in field CFIELD (either :to, :from, :cc or :bcc, or a
list of those) of msg MSG matches (with their name or e-mail
address) regular expressions RX. If there is a match, return
non-nil; otherwise return nil. RX can also be a list of regular
expressions, in which case any of those are tried for a match."
  (cond
   ((null cfield))
   ((listp cfield)
    (seq-find (lambda (cf) (mu4e-message-contact-field-matches msg cf rx))
              cfield))
   ((listp rx)
    ;; if rx is a list, try each one of them for a match
    (seq-find
     (lambda (a-rx) (mu4e-message-contact-field-matches msg cfield a-rx))
     rx))
   (t
    ;; not a list, check the rx
    (seq-find
     (lambda (ct)
       (let ((name (mu4e-contact-name ct))
             (email (mu4e-contact-email ct))
             ;; the 'rx' may be some `/rx/` from mu4e-personal-addresses;
             ;; so let's detect and extract in that case.
             (rx (if (string-match-p  "^\\(.*\\)/$" rx)
                     (substring rx  1 -1) rx)))
         (or
          (and name  (string-match rx name))
          (and email (string-match rx email)))))
     (mu4e-message-field msg cfield)))))

(defun mu4e-message-contact-field-matches-me (msg cfield)
  "Does contact-field CFIELD in MSG match me?
Checks whether any
of the of the contacts in field CFIELD (either :to, :from, :cc or
:bcc) of msg MSG matches *me*, that is, any of the addresses for
which `mu4e-personal-address-p' return t. Returns the contact
cell that matched, or nil."
  (seq-find (lambda (cell)
              (mu4e-personal-address-p (mu4e-contact-email cell)))
            (mu4e-message-field msg cfield)))

(defun mu4e-message-sent-by-me (msg)
  "Is this MSG (to be) sent by me?
Checks if the from field matches user's personal addresses."
  (mu4e-message-contact-field-matches-me msg :from))

(defun mu4e-message-personal-p (msg)
  "Does MSG have user's personal address?
In any of the contact
 fields?"
  (seq-some
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

(defun mu4e-field-at-point (field)
  "Get FIELD for the message at point.
Either in the headers buffer or the view buffer. Field is a
symbol, see `mu4e-header-info'."
  (plist-get (mu4e-message-at-point) field))

(defun mu4e-message-readable-path (&optional msg)
  "Get a readable path to MSG or raise an error.
If MSG is nil, use `mu4e-message-at-point'."
  (let ((path (plist-get (or msg (mu4e-message-at-point)) :path)))
    (unless (file-readable-p path)
      (mu4e-error "No readable message at %s; database outdated?" path))
    path))

(defun mu4e-copy-message-path ()
  "Copy the message-path of message at point to the kill ring."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path)))
    (kill-new path)
    (mu4e-message "Saved '%s' to kill-ring" path)))

(defun mu4e-save-message (&optional auto-path auto-overwrite)
  "Save a copy of the message-at-point.

If AUTO-PATH is non-nil, save to the attachment directory for
message/rfc822 files as per `mu4e-determine-attachment-dir'.
Otherwise, ask user.

If AUTO-OVERWRITE is non-nil, automatically overwrite if a file
with the same name already exist in the target directory.
Otherwise, ask for user confirmation.

Returns the full path."
  (interactive "P")
  (let* ((srcpath (mu4e-message-readable-path))
         (srcname (file-name-nondirectory srcpath))
         (destdir (file-name-as-directory
                   (mu4e-determine-attachment-dir
                    srcpath "message/rfc822")))
         (destpath (mu4e-join-paths destdir srcname))
         (destpath
          (if auto-path destpath
              (read-file-name "Save message as: "
                              destdir nil nil srcname))))
    (when destpath
      (copy-file srcpath destpath (if auto-overwrite t 0))
      (mu4e-message "Saved %s" destpath)
      destpath)))

(defun mu4e-sexp-at-point ()
  "Show or hide the s-expression for the message-at-point, if any."
  (interactive)
  (if-let* ((win (get-buffer-window mu4e--sexp-buffer-name)))
      (delete-window win)
    (when-let* ((msg (mu4e-message-at-point 'noerror)))
      (when (buffer-live-p mu4e--sexp-buffer-name)
        (kill-buffer mu4e--sexp-buffer-name))
      (with-current-buffer-window
          (get-buffer-create mu4e--sexp-buffer-name) nil nil
        (if (fboundp 'lisp-data-mode)
            (lisp-data-mode)
          (lisp-mode))
        (insert (pp-to-string msg))
        (font-lock-ensure)
        ;; add basic `quit-window' bindings
        (view-mode 1)))))

(declare-function mu4e--decoded-message "mu4e-compose")

(defun mu4e-fetch-field (msg hdr &optional first)
  "Find the value for an arbitrary header field HDR from MSG.

If the header appears multiple times, the field values are
concatenated, unless FIRST is non-nil, in which case only the
first value is returned. See `message-field-value' and
`nessage-fetch-field' for details.

Note: this loads the full message file such that any available
message header can be used. If the header is part of the MSG
plist, it is much more efficient to get the information from that
plist."
  (with-temp-buffer
    (insert (mu4e--decoded-message msg 'headers-only))
    (message-field-value hdr first)))

;;;
(provide 'mu4e-message)
;;; mu4e-message.el ends here
