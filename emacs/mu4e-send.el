;; mu4e-send.el -- part of mm, the mu mail user agent
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
;; gnus' message mode

;; mm

;;; Code:

(eval-when-compile (require 'cl))

;; we use some stuff from gnus...
(require 'message)
(require 'mail-parse)
(require 'smtpmail)

;; internal variables / constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst mu4e-send-draft-name "*mu4e-draft*"
  "Name for draft messages.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-send-user-agent ()
  "Return the User-Agent string for mu4e. This is either the value
of `mu4e-user-agent', or, if not set, a string based on the version
of mm and emacs."
  (or mu4e-user-agent
    (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)))

(defun mu4e-send-cite-original (msg)
  "Cite the body text of MSG, with a \"On %s, %s wrote:\"
  line (with the %s's replaced with the date of MSG and the name
  or e-mail address of its sender (or 'someone' if nothing
  else)), followed of the quoted body of MSG, constructed by by
  prepending `mu4e-send-citation-prefix' to each line. If there is
  no body in MSG, return nil."
  (let* ((from (plist-get msg :from))
	  (body (mu4e-body-text msg)))
    (when body
      (concat
	(format "On %s, %s wrote:"
	  (format-time-string "%c" (plist-get msg :date))
	  (if (and from (car from)) ;; a list ((<name> . <email>))
	    (or (caar from) (cdar from) "someone")
	    "someone"))
	"\n\n"
	(replace-regexp-in-string "^" " > " body)))))

(defun mu4e-send-recipients-remove (lst email-to-remove)
  "Remove the recipient with EMAIL from the recipient list (of form
'( (\"A\" . \"a@example.com\") (\"B\" . \"B@example.com\"))."
  (remove-if
    (lambda (name-email)
      (let ((email (cdr name-email)))
	(when email (string= email-to-remove (downcase email))))) lst))

(defun mu4e-send-recipients-to-string (lst)
  "Convert a recipient list (of form '( (\"A\"
. \"a@example.com\") (\"B\" . \"B@example.com\") (nil
. \"c@example.com\")) into a string of form \"A <@aexample.com>, B
<b@example.com>, c@example.com\."
  (when lst
    (mapconcat
      (lambda (recip)
	(let ((name (car recip)) (email (cdr recip)))
	  (if name
	    (format "%s <%s>" name email)
	    (format "%s" email)))) lst ", ")))


(defun mu4e-send-header (hdr val)
  "Return a header line of the form HDR: VAL\n. If VAL is nil,
return nil."
  (when val (format "%s: %s\n" hdr val)))

(defun mu4e-send-references-create (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of the
existing References (which may be empty) and the message-id. If the
message-id is empty, returns the old References. If both are empty,
return nil."
  (let ((refs (plist-get msg :references))
	 (old-msgid (plist-get msg :message-id)))
    (when old-msgid
      (setq refs (append refs (list old-msgid)))
      (mapconcat
	(lambda (msgid) (format "<%s>" msgid))
	refs ","))))

(defun mu4e-send-to-create (msg)
  "Construct the To: header for a reply-message based on some
message MSG. This takes the Reply-To address of MSG if it exist, or
the From:-address otherwise. The result is either nil or a string
which can be used for the To:-field. Note, when it's present,
Reply-To contains a string of one or more addresses,
comma-separated."
  (or
    (plist-get msg :reply-to)
    (mu4e-send-recipients-to-string (plist-get msg :from))))


(defun mu4e-send-cc-create (msg reply-all)
  "Get the list of Cc-addresses for the reply to MSG. If REPLY-ALL
is nil this is simply empty, otherwise it is the old CC-list
together with the old TO-list, minus `user-mail-address'. The
result of this function is either nil or a string to be used for
the Cc: field."
  (when reply-all
    (let ((cc-lst
	    (mu4e-send-recipients-remove ;; remove myself from cc
	      (append (plist-get msg :cc) (plist-get msg :to))
	      user-mail-address)))
      (mu4e-send-recipients-to-string cc-lst))))


(defun mu4e-send-from-create ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))

(defun mu4e-send-create-reply (msg)
  "Create a draft message as a reply to MSG.

A reply message has fields:
  From:        - see `mu-msg-from-create'
  To:          - see `mu4e-send-to-create'
  Cc:          - see `mu4e-send-cc-create'
  Subject:     - `mu4e-send-reply-prefix' + subject of MSG

 then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mu4e-send-references-create'
  In-Reply-To: - message-id of MSG
  User-Agent   - see  `mu4e-send-user-agent'

Then follows `mail-header-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mu4e-send-cite-original'."
  (let* ((recipnum (+ (length (plist-get msg :to))
		     (length (plist-get msg :cc))))
	  (reply-all (when (> recipnum 1)
		      (yes-or-no-p
			(format "Reply to all ~%d recipients? "
			  (+ recipnum)))))
	  (old-msgid (plist-get msg :message-id))
	  (subject (or (plist-get msg :subject) "")))
    (concat
      (mu4e-send-header "From" (or (mu4e-send-from-create) ""))
      (when (boundp 'mail-reply-to)
	(mu4e-send-header "Reply-To" mail-reply-to))

      (mu4e-send-header "To" (or (mu4e-send-to-create msg) ""))
      (mu4e-send-header "Cc" (mu4e-send-cc-create msg reply-all))

      (mu4e-send-header "User-agent"  (mu4e-send-user-agent))
      (mu4e-send-header "References"  (mu4e-send-references-create msg))

      (when old-msgid
	(mu4e-send-header "In-reply-to" (format "<%s>" old-msgid)))
      (mu4e-send-header "Subject"
	(concat
	  ;; if there's no Re: yet, prepend it
	  (if (string-match (concat "^" mu4e-send-reply-prefix) subject)
	    "" mu4e-send-reply-prefix) 
	  subject))

      (propertize mail-header-separator 'read-only t 'intangible t) '"\n"

      "\n\n"
      (mu4e-send-cite-original msg))))

(defun mu4e-send-create-forward (msg)
  "Create a draft forward message for MSG.

A forward message has fields:
  From:        - see `mu4e-send-from-create'
  To:          - empty
  Subject:     - `mu4e-send-forward-prefix' + subject of MSG

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  References:  - see `mu4e-send-references-create'
  User-Agent   - see  `mu4e-send-user-agent'

Then follows `mail-header-separator' (for `message-mode' to separate
body from headers)

And finally, the cited body of MSG, as per `mu4e-send-cite-original'."
  (let ((subject (or (plist-get msg :subject) "")))
    (concat
      (mu4e-send-header "From" (or (mu4e-send-from-create) ""))
      (when (boundp 'mail-reply-to)
	(mu4e-send-header "Reply-To" mail-reply-to))
      
      (mu4e-send-header "To" "")
      (mu4e-send-header "User-agent"  (mu4e-send-user-agent))
      (mu4e-send-header "References"  (mu4e-send-references-create msg))
      (mu4e-send-header "Subject"
	(concat
	  ;; if there's no Re: yet, prepend it
	  (if (string-match (concat "^" mu4e-send-forward-prefix) subject)
	    "" mu4e-send-forward-prefix) 
	  subject))

      (propertize mail-header-separator 'read-only t 'intangible t) "\n"

      "\n\n"
      (mu4e-send-cite-original msg))))

(defun mu4e-send-create-new ()
  "Create a new message.

A new draft message has fields:
  From:        - see `mu-msg-from-create'
  To:          - empty
  Subject:     - empty

then, the following fields, normally hidden from user:
  Reply-To:    - if `mail-reply-to' has been set
  User-Agent   - see  `mu4e-send-user-agent'

Then follows `mail-header-separator' (for `message-mode' to separate
body from headers)."
  (concat
    (mu4e-send-header "From" (or (mu4e-send-from-create) ""))
    (when (boundp 'mail-reply-to)
      (mu4e-send-header "Reply-To" mail-reply-to))
    (mu4e-send-header "To" "")
    (mu4e-send-header "User-agent"  (mu4e-send-user-agent))
    (mu4e-send-header "Subject" "")
    (propertize mail-header-separator 'read-only t 'intangible t) "\n"))

(defun mu4e-send-open-draft (compose-type &optional msg)
  "Open a draft file for a new message, creating it if it does not
already exist, and optionally fill it with STR. Function also adds
the new message to the database. When the draft message is added to
the database, `mu4e-path-docid-map' will be updated, so that we can
use the new docid. Returns the full path to the new message."
  (let* ((hostname
	   (downcase
	     (save-match-data
	       (substring system-name
		 (string-match "^[^.]+" system-name) (match-end 0)))))
	  (draft
	    (concat mu4e-maildir mu4e-drafts-folder "/cur/"
	      (format "%s-%x%x.%s:2,D" ;; 'D': rarely used, but hey, it's available
		(format-time-string "%Y%m%d" (current-time))
		(emacs-pid) (random t) hostname)))
	  (str (case compose-type
		 (reply   (mu4e-send-create-reply msg))
		 (forward (mu4e-send-create-forward msg))
		 (new     (mu4e-send-create-new))
		 (t (error "unsupported compose-type %S" compose-type)))))
    (when str
      (with-temp-file draft
	(insert str)
	(write-file draft)))

    ;; save our file immediately, add add it to the db; thus, we can retrieve
    ;; the new docid from `mu4e-path-docid-map'.
    (mu4e-proc-add draft mu4e-drafts-folder)
    draft))


(defun mu4e-send-compose-handler (compose-type &optional original-msg includes)
  "Create a new draft message, or open an existing one.

COMPOSE-TYPE determines the kind of message to compose and is a
symbol, either `reply', `forward', `edit', `new'. `edit' is for
editing existing messages.

When COMPOSE-TYPE is `reply' or `forward', MSG should be a message
plist.  If COMPOSE-TYPE is `new', MSG should be nil.

Optionally (when forwarding, replying) ORIGINAL-MSG is the original
message we will forward / reply to.

Optionally (when forwarding) INCLUDES contains a list of
   (:file-name <filename> :mime-type <mime-type> :disposition <disposition>)
for the attachements to include; file-name refers to
a file which our backend has conveniently saved for us (as a
tempfile).

The name of the draft folder is constructed from the concatenation
 of `mu4e-maildir' and `mu4e-drafts-folder' (therefore, these must be
 set).

The message file name is a unique name determined by
`mu4e-send-draft-file-name'.

The initial STR would be created from either `mu4e-send-create-reply',
ar`mu4e-send-create-forward' or `mu4e-send-create-new'. The editing buffer is
using Gnus' `message-mode'."
  (unless mu4e-maildir       (error "mu4e-maildir not set"))
  (unless mu4e-drafts-folder (error "mu4e-drafts-folder not set"))
  (let ((draft
	  (if (member compose-type '(reply forward new))
	    (mu4e-send-open-draft compose-type original-msg)
	    (if (eq compose-type 'edit)
	      (plist-get original-msg :path)
	      (error "unsupported compose-type %S" compose-type)))))

    (unless (file-readable-p draft)
      (error "Cannot read %s" draft))

    (find-file draft)
    (message-mode)

    ;; include files -- e.g. when forwarding a message with attachments,
    ;; we take those from the original.
    (save-excursion
      (goto-char (point-max)) ;; put attachments at the end
      (dolist (att includes)
	(mml-attach-file
	  (plist-get att :file-name) (plist-get att :mime-type))))

    (make-local-variable 'write-file-functions)

    ;; update the db when the file is saved...]
    (add-to-list 'write-file-functions
      (lambda() (mu4e-proc-add (buffer-file-name) mu4e-drafts-folder)))

    ;; hook our functions up with sending of the message
    (add-hook 'message-sent-hook 'mu4e-send-save-to-sent nil t)
    (add-hook 'message-sent-hook 'mu4e-send-set-parent-flag nil t)

    (let ((message-hidden-headers
	    `("^References:" "^Face:" "^X-Face:" "^X-Draft-From:"
	       "^User-agent:")))
      (message-hide-headers))

    (if (member compose-type '(new forward))
      (message-goto-to)
      (message-goto-body))))


(defun mu4e-send-save-to-sent ()
  "Move the message in this buffer to the sent folder. This is
 meant to be called from message mode's `message-sent-hook'."
  (unless mu4e-sent-folder (error "mu4e-sent-folder not set"))
  (save-excursion
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (if (search-forward-regexp (concat "^" mail-header-separator "\n"))
      (replace-match "")
      (error "cannot find mail-header-separator"))
    (save-buffer)
    (let ((docid (gethash (buffer-file-name) mu4e-path-docid-map)))
      (unless docid (error "unknown message (%S)" (buffer-file-name)))
      ;; ok, all seems well, well move the message to the sent-folder
      (mu4e-proc-move-msg docid mu4e-sent-folder "-T-D+S")
      (message "Message has been sent"))))



(defun mu4e-send-set-parent-flag ()
  "Set the 'replied' flag on messages we replied to, and the
'passed' flag on message we have forwarded.

If a message has a 'in-reply-to' header, it is considered a reply
to the message with the corresponding message id. If it does not
have an 'in-reply-to' header, but does have a 'references' header,
it is considered to be a forward message for the message
corresponding with the /last/ message-id in the references header.

Now, if the message has been determined to be either a forwarded
message or a reply, we instruct the server to update that message
with resp. the 'P' (passed) flag for a forwarded message, or the
'R' flag for a replied message.

This is meant to be called from message mode's
`message-sent-hook'."
  (let ((in-reply-to (message-fetch-field "in-reply-to"))
	 (forwarded-from)
	 (references (message-fetch-field "references")))
    (unless in-reply-to
      (when references
	(with-temp-buffer ;; inspired by `message-shorten-references'.
	  (insert references)
	  (goto-char (point-min))
	  (let ((refs))
	    (while (re-search-forward "<[^ <]+@[^ <]+>" nil t)
	      (push (match-string 0) refs))
	    (setq forwarded-from (car-safe (last refs)))))))
    ;; remove the <>
    (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
      (mu4e-proc-flag (match-string 1 in-reply-to) "+R"))
    (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
      (mu4e-proc-flag (match-string 1 forwarded-from) "+P"))))

(provide 'mu4e-send)
