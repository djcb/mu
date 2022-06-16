;;; mu4e-draft.el -- part of mu4e, the mu mail user agent for emacs -*- lexical-binding: t -*-
;;
;; Copyright (C) 2011-2022 Dirk-Jan C. Binnema

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

;; In this file, various functions to create draft messages

;;; Code:

(require 'cl-lib)
(require 'mu4e-message)
(require 'mu4e-contacts)
(require 'mu4e-folders)
(require 'message) ;; mail-header-separator


;;; Configuration
(defgroup mu4e-compose nil
  "Customizations for composing/sending messages."
  :group 'mu4e)

(defcustom mu4e-compose-reply-recipients 'ask
  "Which recipients to use when replying to a message.
May be a symbol `ask', `all', `sender'. Note that that only
applies to non-mailing-list message; for those, mu4e always
asks."
  :type '(choice ask
                 all
                 sender)
  :group 'mu4e-compose)

(defcustom mu4e-compose-reply-to-address nil
  "The Reply-To address.
Useful when this is not equal to the From: address."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-compose-forward-as-attachment nil
  "Whether to forward messages as attachments instead of inline."
  :type 'boolean
  :group 'mu4e-compose)

;; backward compatibility
(make-obsolete-variable 'mu4e-reply-to-address
                        'mu4e-compose-reply-to-address
                        "v0.9.9")

(defcustom mu4e-compose-keep-self-cc nil
  "When non-nil. keep your e-mail address in Cc: when replying."
  :type 'boolean
  :group 'mu4e-compose)

(defvar mu4e-compose-parent-message nil
  "The parent message plist.
This is the message being replied to, forwarded or edited; used
in `mu4e-compose-pre-hook'. For new messages, it is nil.")

(make-obsolete-variable 'mu4e-auto-retrieve-keys  "no longer used." "1.3.1")

(defcustom mu4e-decryption-policy t
  "Policy for dealing with replying/forwarding encrypted parts.
The setting is a symbol:
 * t:     try to decrypt automatically
 * `ask': ask before decrypting anything
 * nil:   don't try to decrypt anything."
  :type '(choice (const :tag "Try to decrypt automatically" t)
                 (const :tag "Ask before decrypting anything" ask)
                 (const :tag "Don't try to decrypt anything" nil))
  :group 'mu4e-compose)

;;; Composing / Sending messages

(defcustom mu4e-sent-messages-behavior 'sent
  "Determines what mu4e does with sent messages.

This is one of the symbols:
* `sent'    move the sent message to the Sent-folder (`mu4e-sent-folder')
* `trash'   move the sent message to the Trash-folder (`mu4e-trash-folder')
* `delete'  delete the sent message.

Note, when using GMail/IMAP, you should set this to either
`trash' or `delete', since GMail already takes care of keeping
copies in the sent folder.

Alternatively, `mu4e-sent-messages-behavior' can be a function
which takes no arguments, and which should return one of the mentioned
symbols, for example:

  (setq mu4e-sent-messages-behavior (lambda ()
  (if (string= (message-sendmail-envelope-from) \"foo@example.com\")
       \='delete \='sent)))

The various `message-' functions from `message-mode' are available
for querying the message information."
  :type '(choice (const :tag "move message to mu4e-sent-folder" sent)
                 (const :tag "move message to mu4e-trash-folder" trash)
                 (const :tag "delete message" delete))
  :group 'mu4e-compose)

(defcustom mu4e-compose-context-policy 'ask
  "Policy for determining the context when composing a new message.

If the value is `always-ask', ask the user unconditionally.

In all other cases, if any context matches (using its match
function), this context is used. Otherwise, if none of the
contexts match, we have the following choices:

- `pick-first': pick the first of the contexts available (ie. the default)
- `ask': ask the user
- `ask-if-none': ask if there is no context yet, otherwise leave it as it is
-  nil: return nil; leaves the current context as is.

Also see `mu4e-context-policy'."
  :type '(choice
          (const :tag "Always ask what context to use" always-ask)
          (const :tag "Ask if none of the contexts match" ask)
          (const :tag "Ask when there's no context yet" ask-if-none)
          (const :tag "Pick the first context if none match" pick-first)
          (const :tag "Don't change the context when none match" nil))
  :safe 'symbolp
  :group 'mu4e-compose)

(defcustom mu4e-compose-crypto-policy
  '(encrypt-encrypted-replies sign-encrypted-replies)
  "Policy to control when messages will be signed/encrypted.

The value is a list, whose members determine the behaviour of
`mu4e~compose-crypto-message'. Specifically, it might contain:

- `sign-all-messages': Always add a signature.
- `sign-new-messages': Add a signature to new message, ie.
  messages that aren't responses to another message.
- `sign-forwarded-messages': Add a signature when forwarding
  a message
- `sign-edited-messages': Add a signature to drafts
- `sign-all-replies': Add a signature when responding to
  another message.
- `sign-plain-replies': Add a signature when responding to
  non-encrypted messages.
- `sign-encrypted-replies': Add a signature when responding
  to encrypted messages.

It should be noted that certain symbols have priorities over one
another. So `sign-all-messages' implies `sign-all-replies', which
in turn implies `sign-plain-replies'. Adding both to the set, is
not a contradiction, but a redundant configuration.

All `sign-*' options have a `encrypt-*' analogue."
  :type '(set :greedy t
              (const :tag "Sign all messages" sign-all-messages)
              (const :tag "Encrypt all messages" encrypt-all-messages)
              (const :tag "Sign new messages" sign-new-messages)
              (const :tag "Encrypt new messages" encrypt-new-messages)
              (const :tag "Sign forwarded messages" sign-forwarded-messages)
              (const :tag "Encrypt forwarded messages" encrypt-forwarded-messages)
              (const :tag "Sign edited messages" sign-edited-messages)
              (const :tag "Encrypt edited messages" edited-forwarded-messages)
              (const :tag "Sign all replies" sign-all-replies)
              (const :tag "Encrypt all replies" encrypt-all-replies)
              (const :tag "Sign replies to plain messages" sign-plain-replies)
              (const :tag "Encrypt replies to plain messages" encrypt-plain-replies)
              (const :tag "Sign replies to encrypted messages" sign-encrypted-replies)
              (const :tag "Encrypt replies to encrypted messages" encrypt-encrypted-replies))
  :group 'mu4e-compose)

(make-obsolete-variable 'mu4e-compose-crypto-reply-encrypted-policy "The use of the
 'mu4e-compose-crypto-reply-encrypted-policy' variable is deprecated.
 'mu4e-compose-crypto-policy' should be used instead" "2020-03-06")

(make-obsolete-variable 'mu4e-compose-crypto-reply-plain-policy "The use of the
 'mu4e-compose-crypto-reply-plain-policy' variable is deprecated.
 'mu4e-compose-crypto-policy' should be used instead"
                        "2020-03-06")

(make-obsolete-variable 'mu4e-compose-crypto-reply-policy "The use of the
 'mu4e-compose-crypto-reply-policy' variable is deprecated.
 'mu4e-compose-crypto-reply-plain-policy' and
 'mu4e-compose-crypto-reply-encrypted-policy' should be used instead"
                        "2017-09-02")

(defcustom mu4e-compose-format-flowed nil
  "Whether to compose messages to be sent as format=flowed.
\(Or with long lines if variable `use-hard-newlines' is set to
nil). The variable `fill-flowed-encode-column' lets you customize
the width beyond which format=flowed lines are wrapped."
  :type 'boolean
  :safe 'booleanp
  :group 'mu4e-compose)

(defcustom mu4e-compose-pre-hook nil
  "Hook run just *before* message composition starts.
If the compose-type is a symbol, either `reply' or `forward', the
variable `mu4e-compose-parent-message' points to the message
replied to / being forwarded / edited, and `mu4e-compose-type'
contains the type of message to be composed.

Note that there is no draft message yet when this hook runs, it
is meant for influencing the how mu4e constructs the draft
message. If you want to do something with the draft messages after
it has been constructed, `mu4e-compose-mode-hook' would be the
place to do that."
  :type 'hook
  :group 'mu4e-compose)

(defcustom mu4e-compose-dont-reply-to-self nil
  "If non-nil, do not include self.
Selfness is decided by `mu4e-personal-address-p'"
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-cite-function
  (or message-cite-function 'message-cite-original-without-signature)
  "The function for citing message in replies and forwards.
This is the mu4e-specific version of
`message-cite-function'."
  :type 'function
  :group 'mu4e-compose)

(defcustom mu4e-compose-signature
  (or message-signature "Sent with my mu4e")
  "The message signature.
\(i.e. the blob at the bottom of messages). This is the
mu4e-specific version of `message-signature'."
  :type '(choice string
                 (const :tag "None" nil)
                 (const :tag "Contents of signature file" t)
                 function sexp)
  :risky t
  :group 'mu4e-compose)

(defcustom mu4e-compose-signature-auto-include t
  "Whether to automatically include a message-signature."
  :type 'boolean
  :group 'mu4e-compose)

(make-obsolete-variable 'mu4e-compose-auto-include-date
                        "This is done unconditionally now" "1.3.5")

(defcustom mu4e-compose-in-new-frame nil
  "Whether to compose messages in a new frame."
  :type 'boolean
  :group 'mu4e-compose)

(defvar mu4e-user-agent-string
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)
  "The User-Agent string for mu4e, or nil.")

(defvar mu4e-view-date-format)

(defvar mu4e-compose-type nil
  "The compose-type for this buffer.
This is a symbol, `new', `forward', `reply' or `edit'.")


(defun mu4e~draft-cite-original (msg)
  "Return a cited version of the original message MSG as a plist.
This function uses `mu4e-compose-cite-function', and as such all
its settings apply."
  (with-temp-buffer
    (when (fboundp 'mu4e-view-message-text) ;; keep bytecompiler happy
      (let ((mu4e-view-date-format "%Y-%m-%dT%T%z"))
        (insert (mu4e-view-message-text msg)))
      (message-yank-original)
      (goto-char (point-min))
      (push-mark (point-max))
      ;; set the the signature separator to 'loose', since in the real world,
      ;; many message don't follow the standard...
      (let ((message-signature-separator "^-- *$")
            (message-signature-insert-empty-line t))
        (funcall mu4e-compose-cite-function))
      (pop-mark)
      (goto-char (point-min))
      (buffer-string))))

(defun mu4e~draft-header (hdr val)
  "Return a header line of the form \"HDR: VAL\".
If VAL is nil, return nil."
  ;; note: the propertize here is currently useless, since gnus sets its own
  ;; later.
  (when val (format "%s: %s\n"
                    (propertize hdr 'face 'mu4e-header-key-face)
                    (propertize val 'face 'mu4e-header-value-face))))

(defconst mu4e~max-reference-num 21
  "Specifies the maximum number of References:.
As suggested by `message-shorten-references'.")

(defun mu4e~shorten-1 (list cut surplus)
  "Cut SURPLUS elements out of LIST.
Beginning with CUTth
one. Code borrowed from `message-shorten-1'."
  (setcdr (nthcdr (- cut 2) list)
          (nthcdr (+ (- cut 2) surplus 1) list)))


(defun mu4e~fontify-signature ()
  "Give the message signatures a distinctive color. This is used
in the view and compose modes and will color each signature in
digest messages adhering to RFC 1153."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (while (re-search-forward "^-- *$" nil t)
        (let ((p (point))
              (end (or ;; 30 by RFC1153
                    (re-search-forward "\\(^-\\{30\\}.*$\\)" nil t)
                    (point-max))))
          (add-text-properties p end '(face mu4e-footer-face)))))))

(defun mu4e~draft-references-construct (msg)
  "Construct the value of the References: header based on MSG.
This assumes a comma-separated string. Normally, this the concatenation of the
existing References + In-Reply-To (which may be empty, an note
that :references includes the old in-reply-to as well) and the
message-id. If the message-id is empty, returns the old
References. If both are empty, return nil."
  (let* ( ;; these are the ones from the message being replied to / forwarded
         (refs (mu4e-message-field msg :references))
         (msgid (mu4e-message-field msg :message-id))
         ;; now, append in
         (refs (if (and msgid (not (string= msgid "")))
                   (append refs (list msgid)) refs))
         ;; no doubles
         (refs (cl-delete-duplicates refs :test #'equal))
         (refnum (length refs))
         (cut 2))
    ;; remove some refs when there are too many
    (when (> refnum mu4e~max-reference-num)
      (let ((surplus (- refnum mu4e~max-reference-num)))
        (mu4e~shorten-1 refs cut surplus)))
    (mapconcat (lambda (id) (format "<%s>" id)) refs " ")))


;;; Determine the recipient fields for new messages

(defun mu4e~draft-recipients-list-to-string (lst)
  "Convert a lst LST of address cells into a string.
This is specified as a comma-separated list of e-mail addresses.
If LST is nil, returns nil."
  (when lst
    (mapconcat
     (lambda (contact) (mu4e-contact-full contact)) lst ", ")))

(defun mu4e~draft-address-cell-equal (cell1 cell2)
  "Return t if CELL1 and CELL2 have the same e-mail address.
The comparison is done case-insensitively. If the cells done
match return nil. CELL1 and CELL2 are cons cells of the
form (NAME . EMAIL)."
  (string=
   (downcase (or (mu4e-contact-email cell1) ""))
   (downcase (or (mu4e-contact-email cell2) ""))))


(defun mu4e~draft-create-to-lst (origmsg)
  "Create a list of address for the To: in a new message.
This is based on the original message ORIGMSG. If the Reply-To
address is set, use that, otherwise use the From address. Note,
whatever was in the To: field before, goes to the Cc:-list (if
we're doing a reply-to-all). Special case: if we were the sender
of the original, we simple copy the list form the original."
  (let ((reply-to
         (or (plist-get origmsg :reply-to) (plist-get origmsg :from))))
    (cl-delete-duplicates reply-to :test #'mu4e~draft-address-cell-equal)
    (if mu4e-compose-dont-reply-to-self
        (cl-delete-if
         (lambda (to-cell)
           (mu4e-personal-address-p (mu4e-contact-email to-cell)))
         reply-to)
      reply-to)))


(defun mu4e~strip-ignored-addresses (addrs)
  "Return all addresses that are not to be ignored.
I.e. return all the addresses in ADDRS not matching
`mu4e-compose-reply-ignore-address'."
  (cond
   ((null mu4e-compose-reply-ignore-address)
    addrs)
   ((functionp mu4e-compose-reply-ignore-address)
    (cl-remove-if
     (lambda (elt)
       (funcall mu4e-compose-reply-ignore-address (mu4e-contact-email elt)))
     addrs))
   (t
    ;; regexp or list of regexps
    (let* ((regexp mu4e-compose-reply-ignore-address)
           (regexp (if (listp regexp)
                       (mapconcat (lambda (elt) (concat "\\(" elt "\\)"))
                                  regexp "\\|")
                     regexp)))
      (cl-remove-if
       (lambda (elt)
         (string-match regexp (mu4e-contact-email elt)))
       addrs)))))

(defun mu4e~draft-create-cc-lst (origmsg &optional reply-all include-from)
  "Create a list of address for the Cc: in a new message.
This is based on the original message ORIGMSG, and whether it's a
REPLY-ALL."
  (when reply-all
    (let* ((cc-lst ;; get the cc-field from the original, remove dups
            (cl-delete-duplicates
             (append
              (plist-get origmsg :to)
              (plist-get origmsg :cc)
              (when include-from(plist-get origmsg :from))
              (plist-get origmsg :list-post))
             :test #'mu4e~draft-address-cell-equal))
           ;; now we have the basic list, but we must remove
           ;; addresses also in the To: list
           (cc-lst
            (cl-delete-if
             (lambda (cc-cell)
               (cl-find-if
                (lambda (to-cell)
                  (mu4e~draft-address-cell-equal cc-cell to-cell))
                (mu4e~draft-create-to-lst origmsg)))
             cc-lst))
           ;; remove ignored addresses
           (cc-lst (mu4e~strip-ignored-addresses cc-lst))
           ;; finally, we need to remove ourselves from the cc-list
           ;; unless mu4e-compose-keep-self-cc is non-nil
           (cc-lst
            (if (or mu4e-compose-keep-self-cc (null user-mail-address))
                cc-lst
              (cl-delete-if
               (lambda (cc-cell)
                 (mu4e-personal-address-p (mu4e-contact-email cc-cell)))
               cc-lst))))
      cc-lst)))

(defun mu4e~draft-recipients-construct (field origmsg &optional reply-all include-from)
  "Create value (a string) for the recipient FIELD.
\(which is a symbol, :to or :cc), based on the original message ORIGMSG,
and (optionally) REPLY-ALL which indicates this is a reply-to-all
message. Return nil if there are no recipients for the particular field."
  (mu4e~draft-recipients-list-to-string
   (cl-case field
     (:to
      (mu4e~draft-create-to-lst origmsg))
     (:cc
      (mu4e~draft-create-cc-lst origmsg reply-all include-from))
     (otherwise
      (mu4e-error "Unsupported field")))))

(defun mu4e~draft-from-construct ()
  "Construct a value for the From:-field of the reply.
This is based on the variable `user-full-name' and
`user-mail-address'; if the latter is nil, function returns nil."
  (when user-mail-address
    (mu4e-contact-full (mu4e-contact-make
                        user-full-name
			user-mail-address))))

;;; Header separators

(defun mu4e~draft-insert-mail-header-separator ()
  "Insert `mail-header-separator' in the first empty line of the message.
`message-mode' needs this line to know where the headers end and
the body starts. Note, in `mu4e-compose-mode', we use
`before-save-hook' and `after-save-hook' to ensure that this
separator is never written to the message file. Also see
`mu4e-remove-mail-header-separator'."
    ;; we set this here explicitly, since (as it has happened) a wrong
    ;; value for this (such as "") breaks address completion and other things
    (set (make-local-variable 'mail-header-separator) "--text follows this line--")
    (put 'mail-header-separator 'permanent-local t)
    (save-excursion
      ;; make sure there's not one already
      (mu4e~draft-remove-mail-header-separator)
      (let ((sepa (propertize mail-header-separator
                              'intangible t
                              ;; don't make this read-only, message-mode
                              ;; seems to require it being writable in some cases
                              ;;'read-only "Can't touch this"
                              'rear-nonsticky t
                              'font-lock-face 'mu4e-compose-separator-face)))
	(widen)
	;; search for the first empty line
	(goto-char (point-min))
	(if (search-forward-regexp "^$" nil t)
            (progn
              (replace-match sepa)
              ;; `message-narrow-to-headers` searches for a
              ;; `mail-header-separator` followed by a new line. Therefore, we
              ;; must insert a newline if on the last line of the buffer.
              (when (= (point) (point-max))
		(insert "\n")))
          (progn ;; no empty line? then prepend one
            (goto-char (point-max))
            (insert "\n" sepa))))))

(defun mu4e~draft-remove-mail-header-separator ()
  "Remove `mail-header-separator'.
We do this before saving a
file (and restore it afterwards), to ensure that the separator
never hits the disk. Also see
`mu4e~draft-insert-mail-header-separator."
  (save-excursion
    (widen)
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (when (search-forward-regexp (concat "^" mail-header-separator) nil t)
      (let ((inhibit-read-only t))
        (replace-match "")))))

(defun mu4e~draft-reply-all-p (origmsg)
  "Ask user whether she wants to reply to *all* recipients.
If there is just one recipient of ORIGMSG do nothing."
  (let* ((recipnum
          (+ (length (mu4e~draft-create-to-lst origmsg))
             (length (mu4e~draft-create-cc-lst origmsg t))))
         (response
          (if (< recipnum 2)
              'all ;; with less than 2 recipients, we can reply to 'all'
            (mu4e-read-option
             "Reply to "
             `( (,(format "all %d recipients" recipnum) . all)
                ("sender only" . sender-only))))))
    (eq response 'all)))

(defun mu4e~draft-message-filename-construct (&optional flagstr)
  "Construct a randomized name for a message file with flags FLAGSTR.
It looks something like
  <time>-<random>.<hostname>:2,
You can append flags."
  (let* ((sysname (if (fboundp 'system-name)
                      (system-name)
                    (with-no-warnings system-name)))
         (sysname (if (string= sysname "") "localhost" sysname))
         (hostname (downcase
                    (save-match-data
                      (substring sysname
                                 (string-match "^[^.]+" sysname)
                                 (match-end 0))))))
    (format "%s.%04x%04x%04x%04x.%s%s2,%s"
            (format-time-string "%s" (current-time))
            (random 65535) (random 65535) (random 65535) (random 65535)
            hostname mu4e-maildir-info-delimiter (or flagstr ""))))

(defun mu4e~draft-common-construct ()
  "Construct the common headers for each message."
  (concat
   (when-let ((organization (message-make-organization)))
     (mu4e~draft-header "Organization" organization))
   (when mu4e-user-agent-string
     (mu4e~draft-header "User-agent" mu4e-user-agent-string))
   (mu4e~draft-header "Date" (message-make-date))))

(defconst mu4e~draft-reply-prefix "Re: "
  "String to prefix replies with.")

(defun mu4e~draft-reply-construct-recipients (origmsg)
  "Determine the to/cc recipients for a reply message."
  (let* ((reply-to-self (mu4e-message-contact-field-matches-me origmsg :from))
         ;; reply-to-self implies reply-all
         (reply-all (or reply-to-self
                        (eq mu4e-compose-reply-recipients 'all)
                        (and (not (eq mu4e-compose-reply-recipients 'sender))
                             (mu4e~draft-reply-all-p origmsg)))))
    (concat
     (if reply-to-self
         ;; When we're replying to ourselves, simply keep the same headers.
         (concat
          (mu4e~draft-header "To" (mu4e~draft-recipients-list-to-string
                                   (mu4e-message-field origmsg :to)))
          (mu4e~draft-header "Cc" (mu4e~draft-recipients-list-to-string
                                   (mu4e-message-field origmsg :cc))))

       ;; if there's no-one in To, copy the CC-list
       (if (zerop (length (mu4e~draft-create-to-lst origmsg)))
           (mu4e~draft-header "To" (mu4e~draft-recipients-construct
                                    :cc origmsg reply-all))
         ;; otherwise...
         (concat
          (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
          (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg reply-all))))))))

(defun mu4e~draft-reply-construct-recipients-list (origmsg)
  "Determine the to/cc recipients for a reply message to a
mailing-list."
  (let* ( ;; reply-to-self implies reply-all
         (list-post (plist-get origmsg :list-post))
         (from      (plist-get origmsg :from))
         (recipnum
          (+ (length (mu4e~draft-create-to-lst origmsg))
             (length (mu4e~draft-create-cc-lst origmsg t t))))
	 (sender (mu4e-contact-full (car from)))
         (reply-type
          (mu4e-read-option
           "Reply to mailing-list "
           `( (,(format "all %d recipient(s)" recipnum)    . all)
              (,(format "list-only (%s)" (cdar list-post)) . list-only)
              (,(format "sender-only (%s)" sender)         . sender-only)))))
    (cl-case reply-type
      (all
       (concat
        (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
        (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg t t))))
      (list-only
       (mu4e~draft-header "To"
                          (mu4e~draft-recipients-list-to-string list-post)))
      (sender-only
       (mu4e~draft-header "To"
                          (mu4e~draft-recipients-list-to-string from))))))

(defun mu4e~draft-reply-construct (origmsg)
  "Create a draft message as a reply to ORIGMSG.
Replying-to-self is special; in that case, the To and Cc fields
will be the same as in the original."
  (let* ((old-msgid (plist-get origmsg :message-id))
         (subject (concat mu4e~draft-reply-prefix
                          (message-strip-subject-re
                           (or (plist-get origmsg :subject) ""))))
         (list-post (plist-get origmsg :list-post)))
    (concat
     (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
     (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)

     (if list-post ;; mailing-lists are a bit special.
         (mu4e~draft-reply-construct-recipients-list origmsg)
       (mu4e~draft-reply-construct-recipients origmsg))

     (mu4e~draft-header "Subject" subject)
     (mu4e~draft-header "References"
                        (mu4e~draft-references-construct origmsg))
     (mu4e~draft-common-construct)
     (when old-msgid
       (mu4e~draft-header "In-reply-to" (format "<%s>" old-msgid)))
     "\n\n"
     (mu4e~draft-cite-original origmsg))))

(defconst mu4e~draft-forward-prefix "Fwd: "
  "String to prefix replies with.")

(defun mu4e~draft-forward-construct (origmsg)
  "Create a draft forward message for original message ORIGMSG."
  (let ((subject
         (or (plist-get origmsg :subject) "")))
    (concat
     (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
     (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
     (mu4e~draft-header "To" "")
     (mu4e~draft-common-construct)
     (mu4e~draft-header "References"
                        (mu4e~draft-references-construct origmsg))
     (mu4e~draft-header "Subject"
                        (concat
                         ;; if there's no Fwd: yet, prepend it
                         (if (string-match "^Fwd:" subject)
                             ""
                           mu4e~draft-forward-prefix)
                         subject))
     (unless mu4e-compose-forward-as-attachment
       (concat
        "\n\n"
        (mu4e~draft-cite-original origmsg))))))

(defun mu4e~draft-newmsg-construct ()
  "Create a new message."
  (concat
   (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
   (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
   (mu4e~draft-header "To" "")
   (mu4e~draft-header "Subject" "")
   (mu4e~draft-common-construct)))

(defvar mu4e~draft-drafts-folder nil
  "The drafts-folder for this compose buffer.
This is based on `mu4e-drafts-folder', which is evaluated once.")

(defun mu4e~draft-open-file (path switch-function)
  "Open the the draft file at PATH."
  (let ((buf (find-file-noselect path)))
    (funcall (or
              switch-function
              (and mu4e-compose-in-new-frame 'switch-to-buffer-other-frame)
              'switch-to-buffer)
             buf)))


(defun mu4e~draft-determine-path (draft-dir)
  "Determines the path for a new draft file in DRAFT-DIR."
  (format "%s/%s/cur/%s"
          (mu4e-root-maildir) draft-dir (mu4e~draft-message-filename-construct "DS")))


(defun mu4e-draft-open (compose-type &optional msg switch-function)
  "Open a draft file for a message MSG.
In case of a new message (when COMPOSE-TYPE is `reply', `forward'
 or `new'), open an existing draft (when COMPOSE-TYPE is `edit'),
 or re-send an existing message (when COMPOSE-TYPE is `resend').

The name of the draft folder is constructed from the
concatenation of `(mu4e-root-maildir)' and `mu4e-drafts-folder' (the
latter will be evaluated). The message file name is a unique name
determined by `mu4e-send-draft-file-name'. The initial contents
will be created from either `mu4e~draft-reply-construct', or
`mu4e~draft-forward-construct' or `mu4e~draft-newmsg-construct'."
  (let ((draft-dir nil))
    (cl-case compose-type

      (edit
       ;; case-1: re-editing a draft messages. in this case, we do know the
       ;; full path, but we cannot really know 'drafts folder'... we make a
       ;; guess
       (setq draft-dir (mu4e--guess-maildir (mu4e-message-field msg :path)))
       (mu4e~draft-open-file (mu4e-message-field msg :path) switch-function))

      (resend
       ;; case-2: copy some exisisting message to a draft message, then edit
       ;; that.
       (setq draft-dir (mu4e--guess-maildir (mu4e-message-field msg :path)))
       (let ((draft-path (mu4e~draft-determine-path draft-dir)))
         (copy-file (mu4e-message-field msg :path) draft-path)
         (mu4e~draft-open-file draft-path switch-function)))

      ((reply forward new)
       ;; case-3: creating a new message; in this case, we can determine
       ;; mu4e-get-drafts-folder
       (setq draft-dir (mu4e-get-drafts-folder msg))
       (let ((draft-path (mu4e~draft-determine-path draft-dir))
             (initial-contents
              (cl-case compose-type
                (reply   (mu4e~draft-reply-construct msg))
                (forward (mu4e~draft-forward-construct msg))
                (new     (mu4e~draft-newmsg-construct)))))
         (mu4e~draft-open-file draft-path switch-function)
         (insert initial-contents)
         (newline)
         ;; include the message signature (if it's set)
         (if (and mu4e-compose-signature-auto-include mu4e-compose-signature)
             (let ((message-signature mu4e-compose-signature))
               (save-excursion
                 (message-insert-signature)
                 (mu4e~fontify-signature))))))
      (t (mu4e-error "Unsupported compose-type %S" compose-type)))
    ;; if we didn't find a draft folder yet, try some default
    (unless draft-dir
      (setq draft-dir (mu4e-get-drafts-folder msg)))
    ;; evaluate mu4e~drafts-drafts-folder once, here, and use that value
    ;; throughout.
    (set (make-local-variable 'mu4e~draft-drafts-folder) draft-dir)
    (put 'mu4e~draft-drafts-folder 'permanent-local t)
    (unless mu4e~draft-drafts-folder
      (mu4e-error "Failed to determine drafts folder"))))

;;; _
(provide 'mu4e-draft)
;;; mu4e-draft.el ends here
