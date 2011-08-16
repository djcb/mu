;;; mua-msg.el -- part of mua, the mu mail user agent
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

;; mua

;;; Code:
 
(eval-when-compile (require 'cl))

;; we use some stuff from gnus... 
(require 'message)
(require 'mail-parse)

(require 'html2text)
(require 'mua-common)

(defun mua/msg-from-string (str)
  "Get the plist describing an email message, from STR containing
a message sexp.

 a message sexp looks something like:
 \(
  :from ((\"Donald Duck\" . \"donald@example.com\"))
  :to ((\"Mickey Mouse\" . \"mickey@example.com\"))
  :subject \"Wicked stuff\"
  :date (20023 26572 0)
  :size 15165
  :references (\"200208121222.g7CCMdb80690@msg.id\")
  :in-reply-to \"200208121222.g7CCMdb80690@msg.id\"
  :message-id \"foobar32423847ef23@pluto.net\"
  :maildir: \"/archive\"
  :path \"/home/mickey/Maildir/inbox/cur/1312254065_3.32282.pluto,4cd5bd4e9:2,\"
  :priority high
  :flags (new unread)
  :attachments ((2 \"hello.jpg\" \"image/jpeg\") (3 \"laah.mp3\" \"audio/mp3\"))
  :body-txt \" <message body>\"
\)
other fields are :cc, :bcc, :body-html

When the s-expression comes from the database ('mu find'), the
fields :attachments, :body-txt, :body-html, :references, :in-reply-to
are missing (because that information is not stored in the
database -- at least not in a usable way."
  (condition-case nil
    (car (read-from-string str));; read-from-string returns a cons
    (error "Failed to parse message")))


(defun mua/msg-body-txt-or-html (msg)
  "Get :body-txt, or if not available, :body-html converted to
text, using `html2text'."
  (let ((body (mua/msg-field msg :body-txt)))
    (unless body
      (setq body (mua/msg-field msg :body-html))
      (when body
	(setq body (with-temp-buffer
		     (insert body)
		     (html2text)
		     (buffer-string)))))
    body))

(defun mua/msg-field (msg field)
  "Get a field from this message, or nil. The fields are the
fields of the message, which are the various items of the plist
as described in `mua/msg-from-string'

There is also the special field :body (which is either :body-txt,
or if not available, :body-html converted to text)."
  (case field
    (:body
      (mua/msg-body-txt-or-html msg))
    (:maildir ;; messages gotten from mu-view don't have their maildir set...
      (or (plist-get msg :maildir)
	(mua/msg-maildir-from-path (mua/msg-field msg :path))))
    (t (plist-get msg field))))
    

;; functions for composing new messages (forward, reply and new)

(defvar mua/msg-citation-prefix "> "
  "String to prefix cited message parts with.")

(defvar mua/msg-reply-prefix "Re: "
  "String to prefix the subject of replied messages with.")

(defvar mua/msg-forward-prefix "Fwd: "
  "String to prefix the subject of forwarded messages with.")

(defconst mua/msg-draft-name "*mua-draft*"
  "Name for draft messages.")

(defun mua/msg-user-agent ()
  "Return the User-Agent string for mua. This is either the value
of `mua/user-agent', or, if not set, a string based on the
version of mua and emacs."
  (or mua/user-agent
    (format "mu %s; emacs %s" (mua/mu-binary-version) emacs-version)))

(defun mua/msg-cite-original (msg)
  "Cite the body text of MSG, with a \"On %s, %s wrote:\"
  line (with the %s's replaced with the date of MSG and the name
  or e-mail address of its sender (or 'someone' if nothing
  else)), followed of the quoted body of MSG, constructed by by
  prepending `mua/msg-citation-prefix' to each line. If there is
  no body in MSG, return nil."
  (let* ((from (mua/msg-field msg :from))
	 (body (mua/msg-body-txt-or-html msg)))
    (when body
      (concat
	(format "On %s, %s wrote:"
	  (format-time-string "%c" (mua/msg-field msg :date))
	  (if (and from (car from)) ;; a list ((<name> . <email>))
	    (or (caar from) (cdar from) "someone")
	    "someone"))
	"\n\n"
	(replace-regexp-in-string "^" " > " body)))))


(defun mua/msg-recipients-remove (lst email-to-remove)
  "Remove the recipient with EMAIL from the recipient list (of
form '( (\"A\" . \"a@example.com\") (\"B\" . \"B@example.com\"))."
  (remove-if
    (lambda (name-email)
      (let ((email (cdr name-email)))
	(when email (string= email-to-remove (downcase email))))) lst))

(defun mua/msg-recipients-to-string (lst)
  "Convert a recipient list (of form '( (\"A\"
. \"a@example.com\") (\"B\" . \"B@example.com\") (nil
. \"c@example.com\")) into a string of form \"A <@aexample.com>,
B <b@example.com>, c@example.com\."
  (mapconcat
    (lambda (recip)
      (let ((name (car recip)) (email (cdr recip)))
	(if name
	  (format "%s <%s>" name email)
	  (format "%s" email)))) lst ", "))

(defun mua/msg-hidden-header (hdr val)
  "Return user-invisible header to the message (HDR: VAL\n)."
  ;;  (format "%s: %s\n" hdr val))
  (propertize (format "%s: %s\n" hdr val) 'invisible t))

(defun mua/msg-header (hdr val)
  "Return a header line of the form HDR: VAL\n. If VAL is nil,
return nil."
  (when val (format "%s: %s\n" hdr val)))

(defun mua/msg-references-create (msg)
  "Construct the value of the References: header based on MSG as
a comma-separated string. Normally, this the concatenation of the
existing References (which may be empty) and the message-id. If
the message-id is empty, returns the old References. If both are
empty, return nil."
  (let ((refs (mua/msg-field msg :references))
	 (msgid (mua/msg-field msg :message-id)))
    (if msgid ;; every received message should have one...
      (mapconcat 'identity (append refs (list msgid)) ",")
      (mapconcat 'identity refs ",")))) 
  
(defun mua/msg-to-create (msg reply-all)
  "Construct the To: header for a reply-message based on some
message MSG. If REPLY-ALL is nil, this the the Reply-To addresss
of MSG if it exist, or the From:-address othewise.  If reply-all
is non-nil, the To: is what was in the old To: with either the
Reply-To: or From: appended, and then the
receiver (i.e. `user-mail-address') removed.

So:
  reply-all nil: Reply-To: or From: of MSG
  reply-all t  : Reply-To: or From: of MSG + To: of MSG - `user-mail-address'

The result is either nil or a string which can be used for the To:-field."
  (let ((to-lst (mua/msg-field msg :to))
	 (reply-to (mua/msg-field msg :reply-to))
	 (from (mua/msg-field msg :from)))
    
    (if reply-all
      (progn ;; reply-all   
	(setq to-lst ;; append Reply-To:, or if not set, From: if set
	  (if reply-to (cons `(nil . ,reply-to) to-lst) 
	    (if from (append to-lst from)
	      to-lst)))
	
	;; and remove myself from To:
	(setq to-lst (mua/msg-recipients-remove to-lst user-mail-address))
	(mua/msg-recipients-to-string to-lst))
      
      ;; reply single
      (progn
	(or reply-to (mua/msg-recipients-to-string from))))))

(defconst mua/msg-separator "--text follows this line--\n\n"
  "separator between headers and body, needed for `message-mode'")

(defun mua/msg-cc-create (msg reply-all)
  "Get the list of Cc-addresses for the reply to MSG. If
REPLY-ALL is nil this is simply empty, otherwise it is the same
list as the one in MSG, minus `user-mail-address'. The result of
this function is either nil or a string to be used for the Cc:
field."
  (let ((cc-lst (mua/msg-field msg :cc)))
    (when (and reply-all cc-lst) 
      (mua/msg-recipients-to-string
	(mua/msg-recipients-remove cc-lst
	  user-mail-address)))))

(defun mua/msg-from-create ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter
is nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))

(defun mua/msg-create-reply (msg reply-all)
  "Create a draft message as a reply to MSG; if REPLY-ALL is
non-nil, reply to all recipients.

A reply message has fields:
  From:        - see `mu-msg-from-create'
  To:          - see `mua/msg-to-create'
  Cc:          - see `mua/msg-cc-create'
  Subject:     - `mua/msg-reply-prefix' + subject of MSG

 then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mua/msg-references-create'
  In-Reply-To: - message-id of MSG
  User-Agent   - see  `mua/msg-user-agent'

Then follows `mua/msg-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mua/msg-cite-original'."
     (concat       
       (mua/msg-header "From" (or (mua/msg-from-create) ""))
       (when (boundp 'mail-reply-to)
	 (mua/msg-header "Reply-To" mail-reply-to))

       (mua/msg-header "To" (or (mua/msg-to-create msg reply-all) ""))
       (mua/msg-header "Cc" (mua/msg-cc-create msg reply-all))

       (mua/msg-hidden-header "User-agent"  (mua/msg-user-agent))
       (mua/msg-hidden-header "References"  (mua/msg-references-create msg))
       
       (mua/msg-hidden-header "In-reply-to" (mua/msg-field msg :message-id))
       
       (mua/msg-header"Subject"
	 (concat mua/msg-reply-prefix (mua/msg-field msg :subject)))

       mua/msg-separator
       
       (mua/msg-cite-original msg)))

;; TODO: attachments
(defun mua/msg-create-forward (msg)
  "Create a draft forward message for MSG.

A forward message has fields:
  From:        - see `mu-msg-from-create'
  To:          - empty
  Subject:     - `mua/msg-forward-prefix' + subject of MSG

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mua/msg-references-create'
  User-Agent   - see  `mua/msg-user-agent'

Then follows `mua-msg-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mua/msg-cite-original'."
    (concat    
      (mua/msg-header "From" (or (mua/msg-from-for-new) ""))
       (when (boundp 'mail-reply-to)
	 (mua/msg-header "Reply-To" mail-reply-to))
      
      (mua/msg-header "To" "")
      (mua/msg-hidden-header "User-agent"  (mua/msg-user-agent))
      (mua/msg-hidden-header "References"  (mua/msg-references-for-reply msg))
       (mua/msg-header"Subject"
	 (concat mua/msg-forward-prefix (mua/msg-field msg :subject)))
      
      mua/msg-separator

      (mua/msg-cite-original msg)))

(defun mua/msg-create-new ()
  "Create a new message.

A new draft message has fields:
  From:        - see `mu-msg-from-create'
  To:          - empty
  Subject:     - empty

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  User-Agent   - see  `mua/msg-user-agent'

Then follows `mua-msg-separator' (for `message-mode' to separate
body from headers)."
  (concat    
    (mua/msg-header "From" (or (mua/msg-from-create) ""))
    (when (boundp 'mail-reply-to)
      (mua/msg-header "Reply-To" mail-reply-to))
    
    (mua/msg-header "To" "")
    (mua/msg-hidden-header "User-agent"  (mua/msg-user-agent))
    (mua/msg-header "Subject" "")
    mua/msg-separator))

(defconst mua/msg-prefix "mua" "prefix for mua-generated
mail files; we use this to ensure that our hooks don't mess
with non-mua-generated messages")

(defun mua/msg-draft-file-name ()
  "Create a Maildir-compatible[1], unique file name for a draft
message.
 [1]: see http://cr.yp.to/proto/maildir.html"
  (format "%s-%s-%x.%s:2,D" ;; 'D': rarely used, but hey, it's available
    mua/msg-prefix
    (format-time-string "%Y%m%d" (current-time))
    (emacs-pid)
    (random t)
    (replace-regexp-in-string "[:/]" "_" (system-name))))

(defvar mua/msg-reply-uid nil   "UID of the message this is a reply to.")
(defvar mua/msg-forward-uid nil "UID of the message being forwarded.")

(defun mua/msg-compose (str)
  "Create a new draft message in the drafts folder with STR as
its contents, and open this message file for editing. Optionally
specify PARENT-UID, 

The name of the draft folder is constructed from the concatenation of
 `mua/maildir' and `mua/drafts-folder' (therefore, these must be set).

The message file name is a unique name determined by
`mua/msg-draft-file-name'.

The initial STR would be created from either `mua/msg-create-reply',
`mua/msg-create-forward' or `mua/msg-create-new'. The editing buffer is
using Gnus' `message-mode'."
  (unless mua/maildir       (error "mua/maildir not set"))
  (unless mua/drafts-folder (error "mua/drafts-folder not set"))
  
  ;; write our draft message to the the drafts folder
  (let ((draftfile (concat mua/maildir "/" mua/drafts-folder "/cur/"
		     (mua/msg-draft-file-name))))
    (with-temp-file draftfile (insert str))
    (find-file draftfile)  (rename-buffer mua/msg-draft-name t)
    (message-mode)
    (make-local-variable 'mua/msg-forward-uid)
    
    (message-goto-body)))

(defun mua/msg-reply (msg &optional reply-uid)
  "Create a draft reply to MSG, and swith to an edit buffer with
the draft message. PARENT-UID refers to the UID of the message wer"
  (let* ((recipnum (+ (length (mua/msg-field msg :to))
		    (length (mua/msg-field msg :cc))))
	  (replyall (when (> recipnum 1) 
		      (yes-or-no-p (format "Reply to all ~%d recipients? "
				     (+ recipnum))))))
	                      ;; exact num depends on some more things
    (when (mua/msg-compose (mua/msg-create-reply msg replyall))
      (when reply-uid (setq mua/msg-reply-uid reply-uid))
      (message-goto-body))))

(defun mua/msg-forward (msg &optional forward-uid)
  "Create a draft forward for MSG, and swith to an edit buffer with
the draft message."
  (when (mua/msg-compose (mua/msg-create-forward msg))
    (when forward-uid (setq mua/msg-forward-uid forward-uid))
    (message-goto-to)))

(defun mua/msg-compose-new ()
  "Create a draft message, and swith to an edit buffer with the
draft message."
  (when (mua/msg-compose (mua/msg-create-new))
    (message-goto-to)))



(defun mua/msg-save-to-sent ()
  "Move the message in this buffer to the sent folder. This is
meant to be called from message mode's `message-sent-hook'."
  (if (mua/msg-is-mua-message) ;; only if we are mua
    (unless mua/sent-folder (error "mua/sent-folder not set"))
    (let* ;; TODO: remove duplicate flags
      ((newflags ;; remove Draft; maybe set 'Seen' as well?
	 (delq 'draft (mua/msg-flags-from-path (buffer-file-name))))
	;; so, we register path => uid, then we move uid, then check the name
	;; uid is referring to
	(uid (mua/msg-register (buffer-file-name)))
	(if (mua/msg-move uid
		(concat mua/maildir mua/sent-folder) 
		(mua/msg-flags-to-string newflags))
	  (set-visited-file-name (mua/msg-get-path uid) t t)
	  (mua/warn "Failed to save message to the Sent-folder"))))))


(defun mua/msg-set-replied-or-passed-flag ()
  "Set the 'replied' flag on messages we replied to, and the
'passed' flag on message we have forwarded. This uses
`mua/msg-reply-uid' and `mua/msg-forward-uid', repectively.

NOTE: This does not handle the case yet of message which are
edited from drafts. That case could be solved by searching for
the In-Reply-To message-id for replies.

This is meant to be called from message mode's
`message-sent-hook'."
  ;; handle the replied-to message
  (when mua/msg-reply-uid
    (unless (mua/msg-move mua/msg-reply-uid nil "+R")
      (mua/warn "Failed to marked parent message as 'Replied'")))

  ;; handle the forwarded message
  (when mua/msg-forward-uid
    (unless (mua/msg-move mua/msg-forward-uid nil "+P")
      (mua/warn "Failed to marked parent message as 'Passed'"))))


;; hook our functions up with sending of the message
(add-hook 'message-sent-hook 'mua/msg-save-to-sent)
(add-hook 'message-sent-hook 'mua/msg-set-replied-or-passed-flag)


(provide 'mua-msg)
