;; mm-send.el -- part of mm, the mu mail user agent
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

;; In this file, various functions to compose/send messages, piggybacking on
;; gnus

;; mm

;;; Code:

(eval-when-compile (require 'cl))

;; we use some stuff from gnus...
(require 'message)
(require 'mail-parse)


;; internal variables / constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst mm/msg-draft-name "*mm-draft*"
  "Name for draft messages.")

(defconst mm/msg-separator "--text follows this line--\n\n"
  "separator between headers and body, needed for `message-mode'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME
(defun mm/mu-binary-version () "0.98pre")


(defun mm/msg-user-agent ()
  "Return the User-Agent string for mm. This is either the value
of `mm/user-agent', or, if not set, a string based on the
version of mm and emacs."
  (or mm/user-agent
    (format "mu %s; emacs %s" (mm/mu-binary-version) emacs-version)))

(defun mm/view-body (msg)
  "Get the body for this message, which is either :body-txt,
or if not available, :body-html converted to text)."
  (or (plist-get msg :body-txt)
    (with-temp-buffer
      (plist-get msg :body-html)
      (html2text)
      (buffer-string))
    "No body found"))



(defun mm/msg-cite-original (msg)
  "Cite the body text of MSG, with a \"On %s, %s wrote:\"
  line (with the %s's replaced with the date of MSG and the name
  or e-mail address of its sender (or 'someone' if nothing
  else)), followed of the quoted body of MSG, constructed by by
  prepending `mm/msg-citation-prefix' to each line. If there is
  no body in MSG, return nil."
  (let* ((from (plist-get msg :from))
	  ;; first try plain-text, then html
	  (body  (or (plist-get msg :body-txt)
		   (with-temp-buffer
		     (plist-get msg :body-html)
		     (html2text)
		     (buffer-string)))))
    (when body
      (concat
	(format "On %s, %s wrote:"
	  (format-time-string "%c" (plist-get msg :date))
	  (if (and from (car from)) ;; a list ((<name> . <email>))
	    (or (caar from) (cdar from) "someone")
	    "someone"))
	"\n\n"
	(replace-regexp-in-string "^" " > " body)))))

(defun mm/msg-recipients-remove (lst email-to-remove)
  "Remove the recipient with EMAIL from the recipient list (of form
'( (\"A\" . \"a@example.com\") (\"B\" . \"B@example.com\"))."
  (remove-if
    (lambda (name-email)
      (let ((email (cdr name-email)))
	(when email (string= email-to-remove (downcase email))))) lst))

(defun mm/msg-recipients-to-string (lst)
  "Convert a recipient list (of form '( (\"A\"
. \"a@example.com\") (\"B\" . \"B@example.com\") (nil
. \"c@example.com\")) into a string of form \"A <@aexample.com>, B
<b@example.com>, c@example.com\."
  (mapconcat
    (lambda (recip)
      (let ((name (car recip)) (email (cdr recip)))
	(if name
	  (format "%s <%s>" name email)
	  (format "%s" email)))) lst ", "))

(defun mm/msg-hidden-header (hdr val)
  "Return user-invisible header to the message (HDR: VAL\n)."
  ;;  (format "%s: %s\n" hdr val))
  (propertize (format "%s: %s\n" hdr val) 'invisible t))

(defun mm/msg-header (hdr val)
  "Return a header line of the form HDR: VAL\n. If VAL is nil,
return nil."
  (when val (format "%s: %s\n" hdr val)))

(defun mm/msg-references-create (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of the
existing References (which may be empty) and the message-id. If the
message-id is empty, returns the old References. If both are empty,
return nil."
  (let ((refs (plist-get msg :references))
	 (msgid (plist-get msg :message-id)))
    (if msgid ;; every received message should have one...
      (mapconcat 'identity (append refs (list msgid)) ",")
      (mapconcat 'identity refs ","))))

(defun mm/msg-to-create (msg reply-all)
  "Construct the To: header for a reply-message based on some
message MSG. If REPLY-ALL is nil, this the the Reply-To addresss of
MSG if it exist, or the From:-address othewise.  If reply-all is
non-nil, the To: is what was in the old To: with either the
Reply-To: or From: appended, and then the
receiver (i.e. `user-mail-address') removed.

So:
  reply-all nil: Reply-To: or From: of MSG
  reply-all t  : Reply-To: or From: of MSG + To: of MSG - `user-mail-address'

The result is either nil or a string which can be used for the To:-field."
  (let ((to-lst (plist-get msg :to))
	 (reply-to (plist-get msg :reply-to))
	 (from (plist-get msg :from)))
    (if reply-all
      (progn ;; reply-all
	(setq to-lst ;; append Reply-To:, or if not set, From: if set
	  (if reply-to (cons `(nil . ,reply-to) to-lst)
	    (if from (append to-lst from)
	      to-lst)))

	;; and remove myself from To:
	(setq to-lst (mm/msg-recipients-remove to-lst user-mail-address))
	(mm/msg-recipients-to-string to-lst))

      ;; reply single
      (progn
	(or reply-to (mm/msg-recipients-to-string from))))))


(defun mm/msg-cc-create (msg reply-all)
  "Get the list of Cc-addresses for the reply to MSG. If REPLY-ALL
is nil this is simply empty, otherwise it is the same list as the
one in MSG, minus `user-mail-address'. The result of this function
is either nil or a string to be used for the Cc: field."
  (let ((cc-lst (plist-get msg :cc)))
    (when (and reply-all cc-lst)
      (mm/msg-recipients-to-string
	(mm/msg-recipients-remove cc-lst
	  user-mail-address)))))

(defun mm/msg-from-create ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))

(defun mm/msg-create-reply (msg reply-all)
  "Create a draft message as a reply to MSG; if REPLY-ALL is
non-nil, reply to all recipients.

A reply message has fields:
  From:        - see `mu-msg-from-create'
  To:          - see `mm/msg-to-create'
  Cc:          - see `mm/msg-cc-create'
  Subject:     - `mm/msg-reply-prefix' + subject of MSG

 then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mm/msg-references-create'
  In-Reply-To: - message-id of MSG
  User-Agent   - see  `mm/msg-user-agent'

Then follows `mm/msg-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mm/msg-cite-original'."
     (concat
       (mm/msg-header "From" (or (mm/msg-from-create) ""))
       (when (boundp 'mail-reply-to)
	 (mm/msg-header "Reply-To" mail-reply-to))

       (mm/msg-header "To" (or (mm/msg-to-create msg reply-all) ""))
       (mm/msg-header "Cc" (mm/msg-cc-create msg reply-all))

       (mm/msg-hidden-header "User-agent"  (mm/msg-user-agent))
       (mm/msg-hidden-header "References"  (mm/msg-references-create msg))

       (mm/msg-hidden-header "In-reply-to" (plist-get msg :message-id))

       (mm/msg-header"Subject"
	 (concat mm/msg-reply-prefix (plist-get msg :subject)))

       mm/msg-separator
       (mm/msg-cite-original msg)))

;; TODO: attachments
(defun mm/msg-create-forward (msg)
  "Create a draft forward message for MSG.

A forward message has fields:
  From:        - see `mm/msg-from-create'
  To:          - empty
  Subject:     - `mm/msg-forward-prefix' + subject of MSG

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mm/msg-references-create'
  User-Agent   - see  `mm/msg-user-agent'

Then follows `mm/msg-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mm/msg-cite-original'."
    (concat
      (mm/msg-header "From" (or (mm/msg-from-create) ""))
       (when (boundp 'mail-reply-to)
	 (mm/msg-header "Reply-To" mail-reply-to))

      (mm/msg-header "To" "")
      (mm/msg-hidden-header "User-agent"  (mm/msg-user-agent))
      (mm/msg-hidden-header "References"  (mm/msg-references-for-reply msg))
       (mm/msg-header"Subject"
	 (concat mm/msg-forward-prefix (plist-get msg :subject)))

      mm/msg-separator

      (mm/msg-cite-original msg)))

(defun mm/msg-create-new ()
  "Create a new message.

A new draft message has fields:
  From:        - see `mu-msg-from-create'
  To:          - empty
  Subject:     - empty

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  User-Agent   - see  `mm/msg-user-agent'

Then follows `mm/msg-separator' (for `message-mode' to separate
body from headers)."
  (concat
    (mm/msg-header "From" (or (mm/msg-from-create) ""))
    (when (boundp 'mail-reply-to)
      (mm/msg-header "Reply-To" mail-reply-to))

    (mm/msg-header "To" "")
    (mm/msg-hidden-header "User-agent"  (mm/msg-user-agent))
    (mm/msg-header "Subject" "")
    mm/msg-separator))

(defconst mm/msg-prefix "mm" "prefix for mm-generated
mail files; we use this to ensure that our hooks don't mess
with non-mm-generated messages")

(defun mm/msg-draft-file-name ()
  "Create a Maildir-compatible[1], unique file name for a draft
message.
 [1]: see http://cr.yp.to/proto/maildir.html"
  (format "%s-%s-%x.%s:2,D" ;; 'D': rarely used, but hey, it's available
    mm/msg-prefix
    (format-time-string "%Y%m%d" (current-time))
    (emacs-pid)
    (random t)
    (replace-regexp-in-string "[:/]" "_" (system-name))))


(defvar mm/send-reply-docid nil   "Docid of the message this is a reply to.")
(defvar mm/send-forward-docid nil "Docid of the message being forwarded.")

(defvar mm/mm-msg nil "Whether the current message is an mm msg.")

(defun mm/msg-compose (str &optional parent-docid reply-or-forward)
  "Create a new draft message in the drafts folder with STR as
its contents, and open this message file for editing.

For replies/forewards, you can specify PARENT-DOCID so the
corresponding message can get its Passed or Replied flag set when
this one is sent. If PARENT-DOCID is specified, also
reply-or-forward should be specified, which is a symbol, either
'reply or 'forward.

The name of the draft folder is constructed from the concatenation of
 `mm/maildir' and `mm/drafts-folder' (therefore, these must be set).

The message file name is a unique name determined by
`mm/msg-draft-file-name'.

The initial STR would be created from either `mm/msg-create-reply',
`mm/msg-create-forward' or `mm/msg-create-new'. The editing buffer is
using Gnus' `message-mode'."
  (unless mm/maildir       (error "mm/maildir not set"))
  (unless mm/drafts-folder (error "mm/drafts-folder not set"))

  ;; write our draft message to the the drafts folder
  (let ((draftfile (concat mm/maildir mm/drafts-folder "/cur/"
		     (mm/msg-draft-file-name))))
    (with-temp-file draftfile (insert str))
    (find-file draftfile)
    (rename-buffer mm/msg-draft-name t)

    ;; save our file immediately, add add it to the db; thus, we can retrieve
    ;; the new docid from `mm/path-docid-map'.
    (write-file draftfile)
    (mm/proc-add draftfile mm/drafts-folder)

    (message-mode)

    (make-local-variable 'mm/send-reply-docid)
    (make-local-variable 'mm/send-forward-docid)
    (make-local-variable 'mm/mm-msg)

    (setq mm/mm-msg t)

    (if (eq reply-or-forward 'reply)
      (setq mm/send-reply-docid   parent-docid)
      (setq mm/send-forward-docid parent-docid))

    (message-goto-body)))


(defun mm/send-compose-handler (msg reply-or-forward)
  "This function is registered as the compose handler in
`mm/proc-compose-func', and will be called when a new message is to
be composed, based on some existing one. MSG is a message sexp,
while REPLY-OR-FORWARD is a symbol, either 'reply or 'forward.

In case of 'forward, create a draft forward for MSG, and switch to
an edit buffer with the draft message.

In case of 'reply, create a draft reply to MSG, and swith to an
edit buffer with the draft message"

  (unless (member reply-or-forward '(reply forward))
    (error "unexpected type in compose handler"))
  (let ((parent-docid (plist-get msg :docid)))

    (if (eq reply-or-forward 'forward)

      ;; forward
      (when (mm/msg-compose (mm/msg-create-forward msg) parent-docid 'forward)
	(message-goto-to))

      ;; reply
      (let* ((recipnum (+ (length (plist-get msg :to))
			 (length (plist-get msg :cc))))
	      (replyall (when (> recipnum 1)
			  (yes-or-no-p
			    (format "Reply to all ~%d recipients (y) or only the sender (n)? "
			      (+ recipnum))))))
	;; exact num depends on some more things
	(when (mm/msg-compose (mm/msg-create-reply msg replyall) parent-docid 'reply)
	  (message-goto-body))))))


(defun mm/msg-save-to-sent ()
  "Move the message in this buffer to the sent folder. This is
 meant to be called from message mode's `message-sent-hook'."
  (unless mm/sent-folder (error "mm/sent-folder not set"))
  (when mm/mm-msg ;; only if we are mm
    (let ((docid (gethash (buffer-file-name)  mm/path-docid-map)))
      (unless docid (error "unknown message (%S)" (buffer-file-name)))
      ;; ok, all seems well, well move the message to the sent-folder
      (mm/proc-move-msg docid mm/sent-folder "-T-D+S")
      ;; we can remove the value from the hash now, if we can establish there
      ;; are not other compose buffers using this very same docid...

      ;; mark the buffer as read-only, as its pointing at a non-existing file
      ;; now...
      (message "Message has been sent")
      (setq buffer-read-only t)

      )))

(defun mm/send-set-parent-flag ()
  "Set the 'replied' flag on messages we replied to, and the
'passed' flag on message we have forwarded.

NOTE: This does not handle the case yet of message which are
edited from drafts. That case could be solved by searching for
the In-Reply-To message-id for replies.

This is meant to be called from message mode's
`message-sent-hook'."
  ;; handle the replied-to message
  (when mm/send-reply-docid (mm/proc-flag-msg mm/send-reply-docid "+R"))
  (when mm/send-forward-docid (mm/proc-flag-msg mm/send-forward-docid "+P")))


;; hook our functions up with sending of the message
(add-hook 'message-sent-hook 'mm/msg-save-to-sent)
(add-hook 'message-sent-hook 'mm/send-set-parent-flag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some interactive function

(defun mm/compose-new ()
  "Create a draft message, and switch to an edit buffer with the
draft message."
  (interactive)
  (when (mm/msg-compose (mm/msg-create-new))
    (message-goto-to)))


(provide 'mm-send)



