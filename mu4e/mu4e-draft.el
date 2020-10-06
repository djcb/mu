;;; mu4e-draft.el -- part of mu4e, the mu mail user agent for emacs -*- lexical-binding: t -*-
;;
;; Copyright (C) 2011-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

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

;; In this file, various functions to create draft messages

;;; Code:

(require 'cl-lib)
(require 'mu4e-vars)
(require 'mu4e-utils)
(require 'mu4e-message)
(require 'message) ;; mail-header-separator

;;; Options

(defcustom mu4e-compose-dont-reply-to-self nil
  "If non-nil, don't include self.
\(that is, member of `(mu4e-personal-addresses)') in replies."
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
     (lambda (addrcell)
       (let ((name (car addrcell))
             (email (cdr addrcell)))
         (if name
             (format "%s <%s>" (mu4e~rfc822-quoteit name) email)
           (format "%s" email))))
     lst ", ")))

(defun mu4e~draft-address-cell-equal (cell1 cell2)
  "Return t if CELL1 and CELL2 have the same e-mail address.
The comparison is done case-insensitively. If the cells done
match return nil. CELL1 and CELL2 are cons cells of the
form (NAME . EMAIL)."
  (string=
   (downcase (or (cdr cell1) ""))
   (downcase (or (cdr cell2) ""))))


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
           (cl-member-if
            (lambda (addr)
              (string= (downcase addr) (downcase (cdr to-cell))))
            (mu4e-personal-addresses)))
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
       (funcall mu4e-compose-reply-ignore-address (cdr elt)))
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
         (string-match regexp (cdr elt)))
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
                 (cl-member-if
                  (lambda (addr)
                    (string= (downcase addr) (downcase (cdr cc-cell))))
                  (mu4e-personal-addresses)))
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

;;; RFC2822 handling of phrases in mail-addresses
;;
;; The optional display-name contains a phrase, it sits before the
;; angle-addr as specified in RFC2822 for email-addresses in header
;; fields.  Contributed by jhelberg.

(defun mu4e~rfc822-phrase-type (ph)
  "Return an atom or quoted-string for the phrase PH.
This checks for empty string first. Then quotes around the phrase
\(returning 'rfc822-quoted-string). Then whether there is a quote
inside the phrase (returning 'rfc822-containing-quote).
The reverse of the RFC atext definition is then tested.
If it matches, nil is returned, if not, it is an 'rfc822-atom, which
is returned."
  (cond
   ((= (length ph) 0) 'rfc822-empty)
   ((= (aref ph 0) ?\")
    (if (string-match "\"\\([^\"\\\n]\\|\\\\.\\|\\\\\n\\)*\"" ph)
        'rfc822-quoted-string
      'rfc822-containing-quote)) ; starts with quote, but doesn't end with one
   ((string-match-p "[\"]" ph) 'rfc822-containing-quote)
   ((string-match-p "[\000-\037()\*<>@,;:\\\.]+" ph) nil)
   (t 'rfc822-atom)))

(defun mu4e~rfc822-quoteit (ph)
  "Quote an RFC822 phrase PH only if necessary.
Atoms and quoted strings don't need quotes. The rest do.  In
case a phrase contains a quote, it will be escaped."
  (let ((type (mu4e~rfc822-phrase-type ph)))
    (cond
     ((eq type 'rfc822-atom) ph)
     ((eq type 'rfc822-quoted-string) ph)
     ((eq type 'rfc822-containing-quote)
      (format "\"%s\""
              (replace-regexp-in-string "\"" "\\\\\"" ph)))
     (t (format "\"%s\"" ph)))))


(defun mu4e~draft-from-construct ()
  "Construct a value for the From:-field of the reply.
This is based on the variable `user-full-name' and
`user-mail-address'; if the latter is nil, function returns nil."
  (when user-mail-address
    (if user-full-name
        (format "%s <%s>" (mu4e~rfc822-quoteit user-full-name) user-mail-address)
      (format "%s" user-mail-address))))


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
         (reply-type
          (mu4e-read-option
           "Reply to mailing-list "
           `( (,(format "all %d recipient(s)" recipnum)     . all)
              (,(format "list-only (%s)" (cdar list-post))  . list-only)
              (,(format "sender-only (%s)" (cdar from))     . sender-only)))))
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

(defun mu4e~draft-open-file (path)
  "Open the the draft file at PATH."
  (if mu4e-compose-in-new-frame
      (find-file-other-frame path)
    (find-file path)))

(defun mu4e~draft-determine-path (draft-dir)
  "Determines the path for a new draft file in DRAFT-DIR."
  (format "%s/%s/cur/%s"
          (mu4e-root-maildir) draft-dir (mu4e~draft-message-filename-construct "DS")))


(defun mu4e-draft-open (compose-type &optional msg)
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
       (setq draft-dir (mu4e~guess-maildir (mu4e-message-field msg :path)))
       (mu4e~draft-open-file (mu4e-message-field msg :path)))

      (resend
       ;; case-2: copy some exisisting message to a draft message, then edit
       ;; that.
       (setq draft-dir (mu4e~guess-maildir (mu4e-message-field msg :path)))
       (let ((draft-path (mu4e~draft-determine-path draft-dir)))
         (copy-file (mu4e-message-field msg :path) draft-path)
         (mu4e~draft-open-file draft-path)))

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
         (mu4e~draft-open-file draft-path)
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
