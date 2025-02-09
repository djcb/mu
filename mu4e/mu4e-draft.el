;;; mu4e-draft.el --- Helpers for m4e-compose -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Dirk-Jan C. Binnema

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

;; Implements various helper functions for mu4e-compose. This all
;; look a little convoluted since we need to subvert the gnus/message
;; functions a bit to work with mu4e.

(require 'message)
(require 'mu4e-config)
(require 'mu4e-helpers)
(require 'mu4e-contacts)
(require 'mu4e-folders)
(require 'mu4e-message)
(require 'mu4e-context)
(require 'mu4e-window)

;;; Code:

(declare-function mu4e-compose-mode "mu4e-compose")
(declare-function mu4e "mu4e")

(defcustom mu4e-compose-crypto-policy
  '(encrypt-encrypted-replies sign-encrypted-replies)
  "Policy to control when messages will be signed/encrypted.

The value is a list which influence the way draft messages are
created. Specifically, it might contain:

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
              (const :tag "Encrypt forwarded messages"
                     encrypt-forwarded-messages)
              (const :tag "Sign edited messages" sign-edited-messages)
              (const :tag "Encrypt edited messages" edited-forwarded-messages)
              (const :tag "Sign all replies" sign-all-replies)
              (const :tag "Encrypt all replies" encrypt-all-replies)
              (const :tag "Sign replies to plain messages" sign-plain-replies)
              (const :tag "Encrypt replies to plain messages"
                     encrypt-plain-replies)
              (const :tag "Sign replies to encrypted messages"
                     sign-encrypted-replies)
              (const :tag "Encrypt replies to encrypted messages"
                     encrypt-encrypted-replies))
  :group 'mu4e-compose)

;;; Crypto
(defun mu4e--prepare-crypto (parent compose-type)
  "Possibly encrypt or sign a message based on PARENT and COMPOSE-TYPE.
See `mu4e-compose-crypto-policy' for more details."
  (let* ((encrypted-p
          (and parent (memq 'encrypted (mu4e-message-field parent :flags))))
         (encrypt
          (or (memq 'encrypt-all-messages mu4e-compose-crypto-policy)
              (and (memq 'encrypt-new-messages mu4e-compose-crypto-policy)
                   (eq compose-type 'new))    ;; new messages
              (and (eq compose-type 'forward) ;; forwarded
                   (memq 'encrypt-forwarded-messages mu4e-compose-crypto-policy))
              (and (eq compose-type 'edit) ;; edit
                   (memq 'encrypt-edited-messages mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) ;; all replies
                   (memq 'encrypt-all-replies mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) (not encrypted-p) ;; plain replies
                   (memq 'encrypt-plain-replies mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) encrypted-p
                   (memq 'encrypt-encrypted-replies
                         mu4e-compose-crypto-policy)))) ;; encrypted replies
         (sign
          (or (memq 'sign-all-messages mu4e-compose-crypto-policy)
              (and (eq compose-type 'new) ;; new messages
                   (memq 'sign-new-messages mu4e-compose-crypto-policy))
              (and (eq compose-type 'forward) ;; forwarded messages
                   (memq 'sign-forwarded-messages mu4e-compose-crypto-policy))
              (and (eq compose-type 'edit) ;; edited messages
                   (memq 'sign-edited-messages mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) ;; all replies
                   (memq 'sign-all-replies mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) (not encrypted-p) ;; plain replies
                   (memq 'sign-plain-replies mu4e-compose-crypto-policy))
              (and (eq compose-type 'reply) encrypted-p ;; encrypted replies
                   (memq 'sign-encrypted-replies mu4e-compose-crypto-policy)))))
    (cond ((and sign encrypt) (mml-secure-message-sign-encrypt))
          (sign (mml-secure-message-sign))
          (encrypt (mml-secure-message-encrypt)))))

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
       \\='delete \\='sent)))

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

;;
;; display the ready-to-go display buffer in the desired way.
;;
(defun mu4e--display-draft-buffer (cbuf)
  "Display the message composition buffer CBUF.
Display is influenced by `mu4e-compose-switch'."
  (let ((func
         (pcase mu4e-compose-switch
           ('nil             #'switch-to-buffer)
           ('window          #'switch-to-buffer-other-window)
           ((or 'frame 't)   #'switch-to-buffer-other-frame)
           ('display-buffer  #'display-buffer)
           (_             (mu4e-error "Invalid mu4e-compose-switch")))))
    (funcall func cbuf)))

(defvar mu4e-user-agent-string
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)
  "The User-Agent string for mu4e, or nil.")

;;; Runtime variables; useful for user-hooks etc.
;; mu4e-compose-parent-message & mu4e-compose-type are buffer-local and
;; permanent-local so they'll survive the mode change to mu4e-compose-mode and
;; we can use them in the corresponding mode-hook.
(defvar-local mu4e-compose-parent-message nil
  "The parent message plist.
This is the message being replied to, forwarded or edited; used
in `mu4e-compose-pre-hook'. For new (non-reply, forward etc.)
messages, it is nil.")
(put 'mu4e-compose-parent-message 'permanent-local t)

(defvar-local mu4e-compose-type nil
  "The compose-type for the current message.")
(put 'mu4e-compose-type 'permanent-local t)

;;; Filenames
(defun mu4e--draft-basename()
  "Construct a randomized filename for a message with flags FLAGSTR.
It looks something like
    <time>-<random>.<hostname>

This filename is used for the draft message and the sent message,
depending on `mu4e-sent-messages-behavior'."
  (let* ((sysname (if (fboundp 'system-name)
                      (system-name) (with-no-warnings system-name)))
         (sysname (if (string= sysname "") "localhost" sysname))
         (hostname (downcase
                    (save-match-data
                      (substring sysname
                                 (string-match "^[^.]+" sysname)
                                 (match-end 0))))))
    (format "%s.%04x%04x%04x%04x.%s"
            (format-time-string "%s" (current-time))
            (random 65535) (random 65535) (random 65535) (random 65535)
            hostname)))

(defun mu4e--draft-message-path (base-name &optional parent)
  "Construct a draft message path, based on PARENT if provided.

PARENT is either nil or the original message (being replied
 to/forwarded etc.), and is used to determine the draft folder.
BASE-NAME is the base filename without any Maildir decoration."
  (let ((draft-dir (mu4e-get-drafts-folder parent)))
    (mu4e-join-paths
     (mu4e-root-maildir) draft-dir "cur"
     (format "%s%s2,DS" base-name mu4e-maildir-info-delimiter))))

(defun mu4e--fcc-path (base-name &optional parent)
  "Construct an Fcc: path, based on PARENT and `mu4e-sent-messages-behavior'.

PARENT is either nil or the original message (being replied
to/forwarded etc.), and is used to determine the sent folder,
together with `mu4e-sent-messages-behavior'. BASE-NAME is the
base filename without any Maildir decoration.

Returns the path for the sent message, either in the sent or
trash folder, or nil if the message should be removed after
sending."
  (let* ((behavior
          (if (and (functionp mu4e-sent-messages-behavior)
                   ;; don't interpret 'delete as a function...
                   (not (eq mu4e-sent-messages-behavior 'delete)))
              (funcall mu4e-sent-messages-behavior)
            mu4e-sent-messages-behavior))
         (sent-dir
          (pcase behavior
            ('delete nil)
            ('trash (mu4e-get-trash-folder parent))
            ('sent (mu4e-get-sent-folder parent))
            (_ (mu4e-error "Error in `mu4e-sent-messages-behavior'")))))
    (when sent-dir
         (mu4e-join-paths
          (mu4e-root-maildir) sent-dir "cur"
          (format "%s%s2,S" base-name mu4e-maildir-info-delimiter)))))


(defconst mu4e--header-separator
  ;; XX properties don't show... why not?
  (propertize "--text follows this line--" 'read-only t 'intangible t)
  "Line used to separate headers from text in messages being composed.")

(defun mu4e--delimit-headers (&optional undelimit)
  "Delimit or undelimit (with UNDELIMIT) headers."
  (let ((mail-header-separator (substring-no-properties mu4e--header-separator))
        (inhibit-read-only t))
    (save-excursion (mail-sendmail-undelimit-header)) ;; clear first
    (unless undelimit (save-excursion (mail-sendmail-delimit-header)))))

(defun mu4e--decoded-message (msg &optional headers-only)
  "Get the message MSG, decoded as a string.
With HEADERS-ONLY non-nil, only include the headers part."
  (with-temp-buffer
    (setq-local gnus-article-decode-hook
                '(article-decode-charset
                  article-decode-encoded-words
                  article-decode-idna-rhs
                  article-treat-non-ascii
                  article-remove-cr
                  article-de-base64-unreadable
                  article-de-quoted-unreadable)
                gnus-inhibit-mime-unbuttonizing nil
                gnus-unbuttonized-mime-types '(".*/.*")
                gnus-original-article-buffer (current-buffer))
    (insert-file-contents-literally
     (mu4e-message-readable-path msg) nil nil nil t)
    ;; remove the body / attachments and what not.
    (when headers-only
      (rfc822-goto-eoh)
      (delete-region (point) (point-max)))
    ;; in rare (broken) case, if a message-id is missing use the generated one
    ;; from mu.
    (mu4e--delimit-headers)
    (unless (message-field-value "Message-Id")
      (goto-char (point-min))
      (insert (format "Message-Id: <%s>\n" (plist-get msg :message-id))))
    (mu4e--delimit-headers 'undelimit)
    (ignore-errors (run-hooks 'gnus-article-decode-hook))
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar mu4e--draft-buffer-max-name-length 48)
(defun mu4e--draft-set-friendly-buffer-name ()
  "Use some friendly name for this draft buffer."
  (let* ((subj (message-field-value "subject"))
         (subj (if (or (not subj) (string-match "^[:blank:]*$" subj))
                   "No subject"  subj)))
    (rename-buffer (generate-new-buffer-name
                    (format "\"%s\""
                            (truncate-string-to-width subj
                             mu4e--draft-buffer-max-name-length
                             0 nil t)))
                   (buffer-name))))

;; hook impls

(defun mu4e--fcc-handler (msgpath)
  "Handle Fcc: for MSGPATH.
This ensures that a copy of a sent messages ends up in the
appropriate sent-messages folder. If MSGPATH is nil, do nothing."
  (when msgpath
    (let* ((target-dir (file-name-directory msgpath))
           (target-mdir (file-name-directory target-dir)))
      ;; create maildir if needed
      (unless (file-exists-p target-mdir)
        (make-directory
         (mu4e-join-paths target-mdir "cur" 'parents))
        (make-directory
         (mu4e-join-paths target-mdir "new" 'parents)))
      (write-file msgpath)
      (mu4e--server-add msgpath))))

;; save / send hooks

(defvar-local mu4e--compose-undo nil
  "Remember the undo-state.")

(defun mu4e--compose-before-save ()
  "Function called just before the draft buffer is saved."
  ;; This does 3 things:
  ;;  - set the Message-Id if not already
  ;;  - set the Date if not already
  ;;  - (temporarily) remove the mail-header separator
  (setq mu4e--compose-undo buffer-undo-list)
  (save-excursion
    (unless (message-field-value "Message-ID")
      (message-generate-headers '(Message-ID)))
    ;; older Emacsen (<= 28 perhaps?) won't update the Date
    ;; if there already is one; so make sure it's gone.
    (message-remove-header "Date")
    (message-generate-headers '(Date Subject From))
    (mu4e--delimit-headers 'undelimit))) ;; remove separator

(defun mu4e--set-parent-flags (path)
  "Set flags for replied-to and forwarded for the message at PATH.
That is, set the `replied' \"R\" flag on messages we replied to,
and the `passed' \"F\" flag on message we have forwarded.

If a message has an \"In-Reply-To\" header, it is considered a
reply to the message with the corresponding message id.
Otherwise, if it does not have an \"In-Reply-To\" header, but
does have a \"References:\" header, it is considered to be a
forward message for the message corresponding with the /last/
message-id in the references header.

If the message has been determined to be either a forwarded
message or a reply, we instruct the server to update that message
with resp. the \"P\" (passed) flag for a forwarded message, or
the \"R\" flag for a replied message. The original messages are
also marked as Seen.

Function assumes that it is executed in the context of the
message buffer."
  ;; note that we can't use mu4e-compose-parent-message here, since it
  ;; no longer available when editing a draft. So we scan the outgoing
  ;; message for the information.
  (when-let* ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (let* ((in-reply-to (message-field-value "in-reply-to"))
             (references (message-field-value "references"))
             (forwarded-from
              (unless (or in-reply-to (not references))
                (with-temp-buffer ;; inspired by `message-shorten-references'.
                  (insert references)
                  (goto-char (point-min))
                  (let ((refs))
                    (while (re-search-forward "<[^ <]+@[^ <]+>" nil t)
                      (push (match-string 0) refs))
                    (car refs))))) ;; the last shall be the first
             ;; remove the <>
             (in-reply-to (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to)
                               (match-string 1 in-reply-to)))
             (forwarded-from (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from)
                                  (match-string 1 forwarded-from))))
        ;; mark parents.
        (when in-reply-to
          (mu4e-log 'misc "mark %s as Replied" in-reply-to)
          (mu4e--server-move in-reply-to nil "+R-N"))
        (when forwarded-from
          (mu4e-log 'misc "mark %s as Passed (forwarded)" forwarded-from)
          (mu4e--server-move forwarded-from nil "+P-N"))))))

(defun mu4e--compose-after-save()
  "Function called immediately after the draft buffer is saved."
  ;; This does 3 things:
  ;; - restore the mail-header-separator (see mu4e--compose-before-save)
  ;; - update the buffer name (based on the message subject
  ;; - tell the mu server about the updated draft message
  (mu4e--delimit-headers)
  (mu4e--draft-set-friendly-buffer-name)
  ;; tell the server
  (mu4e--server-add (buffer-file-name))
  ;; restore history.
  (set-buffer-modified-p nil)
  (setq buffer-undo-list mu4e--compose-undo))

(defun mu4e-sent-handler (docid path)
  "Handler called with DOCID and PATH for the just-sent message.
For Forwarded ('Passed') and Replied messages, try to set the
appropriate flag at the message forwarded or replied-to."
  ;; XXX we don't need this function anymore here, but we have an external
  ;; caller in mu4e-icalendar... we should update that.
  (mu4e--set-parent-flags path)
  ;; if the draft file exists, remove it now.
  (when (file-exists-p path)
    (mu4e--server-remove docid)))

(defun mu4e--send-harden-newlines ()
  "Set the hard property to all newlines."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(defun mu4e--compose-message-sent ()
  "Mu4e's `message-sent-hook' handling."
  ;; typically, draft is gone and the sent message appears in sent. Update flags
  ;; for related messages, i.e. for Forwarded ('Passed') and Replied messages,
  ;; try to set the appropriate flag at the message forwarded or replied-to.
  (when-let* ((fcc-path (message-field-value "Fcc")))
    (mu4e--set-parent-flags fcc-path)
    ;; we end up with a ((buried) buffer here, visiting the
    ;; fcc-path; not quite sure why. But let's get rid of it (#2681)
    (when-let* ((buf (find-buffer-visiting fcc-path)))
      (kill-buffer buf)))
  ;; remove draft
  (when-let* ((draft (buffer-file-name)))
    (mu4e--server-remove draft)))

(defun mu4e--compose-before-send ()
  "Function called just before sending a message."
  ;; Remove References: if In-Reply-To: is missing.
  ;; This allows the user to effectively start a new message-thread by
  ;; removing the In-Reply-To header.
  (when (eq mu4e-compose-type 'reply)
    (unless (message-field-value "In-Reply-To")
      (message-remove-header "References")))
  (when use-hard-newlines
    (mu4e--send-harden-newlines))
  ;; in any case, make sure to save the message; this will also trigger
  ;; before/after save hooks, which fixes up various fields.
  (set-buffer-modified-p t)
  (save-buffer)
  ;; now handle what happens _after_ sending
  (add-hook 'message-sent-hook #'mu4e--compose-message-sent nil t))

;; overrides for message-* functions
;;
;; mostly some magic because the message-reply/-forward/... functions want to
;; create and switch to buffer by themselves; but mu4e wants to control
;; when/where the buffers are shown so we subvert the message-functions and get
;; the buffer without display it.

(defvar mu4e--message-buf nil
  "The message buffer created by (overridden) message-* functions.")

(defun mu4e--message-pop-to-buffer (name &optional _switch)
  "Mu4e override for `message-pop-to-buffer'.
Creates a buffer NAME and returns it."
  (set-buffer (get-buffer-create name))
  (erase-buffer)
  (setq mu4e--message-buf (current-buffer)))

(defun mu4e--message-is-yours-p ()
  "Did you send the message we're responding to?
Mu4e's override for `message-is-yours-p'. This only considers the
From: address, not the Sender: address.

Note that we check the parent, message-at-point so this should
work both in headers-view and message-view."
  (seq-some (lambda (addr)
              (mu4e-personal-or-alternative-address-p addr))
            (seq-map (lambda (addr) (plist-get addr :email))
                     (plist-get (or mu4e-compose-parent-message
                                    (mu4e-message-at-point 'noerror))
                                :from))))

(defmacro mu4e--validate-hidden-buffer (&rest body)
  "Macro to evaluate BODY and asserts that it yields a valid buffer.
Where valid means that it is a live an non-active buffer.
Returns said buffer."
  `(let ((buf (progn ,@body)))
     (cl-assert (buffer-live-p buf))
     (cl-assert (not (eq buf (window-buffer (selected-window)))))
     buf))

(defun mu4e--message-call (func &rest params)
  "Call message/gnus functions from a mu4e-context.
E.g., functions such as `message-reply' or `message-forward', but
manipulate such that they do *not* switch to the created buffer,
but merely return it.

FUNC is the function to call and PARAMS are its parameters.

For replying/forwarding, this functions expects to be called
while in a buffer with the to-be-forwarded/replied-to message."
  (let* ((message-this-is-mail t)
         (message-generate-headers-first nil)
         (message-newsreader mu4e-user-agent-string)
         (message-mail-user-agent nil)
         (mam message-alternative-emails)
         (message-alternative-emails
          (lambda (addr)
            (or (mu4e-personal-address-p addr)
                (cond
                 ((functionp mam) (funcall mam addr))
                 ((stringp mam)   (string-match mam addr)))))))
    (cl-letf
        ;; `message-pop-to-buffer' attempts switching the visible buffer;
        ;; instead, we manipulate it to _return_ the buffer.
        (((symbol-function #'message-pop-to-buffer)
          #'mu4e--message-pop-to-buffer)
         ;; teach `message-is-yours-p' about how mu4e defines that
         ((symbol-function #'message-is-yours-p)
          #'mu4e--message-is-yours-p))
      ;; also turn off all the gnus crypto handling, we do that ourselves..
      (setq-local gnus-message-replysign nil
                  gnus-message-replyencrypt nil
                  gnus-message-replysignencrypted nil)
      (setq mu4e--message-buf nil)
      (apply func params))
    (mu4e--validate-hidden-buffer mu4e--message-buf)))
;;
;; make the draft buffer ready for use.
;;

(defun mu4e--jump-to-a-reasonable-place ()
  "Jump to a reasonable place for writing an email."
  (if (not (message-field-value "To"))
      (message-goto-to)
    (if (not (message-field-value "Subject"))
        (message-goto-subject)
      (pcase message-cite-reply-position
        ((or 'above 'traditional) (message-goto-body))
        (_ (when (message-goto-signature) (forward-line -2)))))))

(defvar mu4e-draft-hidden-headers
  (append message-hidden-headers '("^User-agent:" "^Fcc:"))
  "Message headers to hide when composing.
This is mu4e's version of `message-hidden-headers'.")

(defun mu4e--prepare-draft (&optional parent)
  "Get ready for message composition.
PARENT is the parent message, if any."
  (unless (mu4e-running-p) (mu4e 'background)) ;; start if needed
  (mu4e--context-autoswitch parent mu4e-compose-context-policy))

(defun mu4e--prepare-draft-headers (compose-type)
  "Add extra headers for message based on COMPOSE-TYPE."
  (let ((message-newsreader mu4e-user-agent-string))
    (message-generate-headers
     (seq-filter #'identity ;; ensure needed headers are generated.
                 `(From Subject Date Message-ID
                        ,(when (memq compose-type '(reply forward)) 'References)
                        ,(when (eq compose-type 'reply) 'In-Reply-To)
                        ,(when message-newsreader 'User-Agent)
                        ,(when message-user-organization 'Organization))))))

(defun mu4e--prepare-draft-buffer (compose-type parent)
  "Prepare the current buffer as a draft-buffer.
COMPOSE-TYPE and PARENT are as in `mu4e--draft'."
  (cl-assert (member compose-type '(reply forward edit new)))
  (cl-assert (eq (if parent t nil)
                 (if (member compose-type '(reply forward)) t nil)))
  ;; remember some variables, e.g for user hooks. These are permanent-local
  ;; hence survive the mode-switch below (we do this so these useful vars are
  ;; available in mode-hooks.
  (setq-local
   mu4e-compose-parent-message parent
   mu4e-compose-type compose-type)

  ;; draft path
  (unless (eq compose-type 'edit)
    (set-visited-file-name ;; make it a draft file
     (mu4e--draft-message-path (mu4e--draft-basename) parent)))
  ;; fcc
  (when-let* ((fcc-path (mu4e--fcc-path (mu4e--draft-basename) parent)))
    (message-add-header (concat "Fcc: " fcc-path "\n")))

  (mu4e--prepare-draft-headers compose-type)
  (mu4e--prepare-crypto parent compose-type)
  ;; set the attachment dir to something more reasonable than the draft
  ;; directory.
  (setq default-directory (mu4e-determine-attachment-dir))
  (mu4e--draft-set-friendly-buffer-name)

  ;; now, switch to compose mode
  (mu4e-compose-mode)

  ;; hide some internal headers; we use the special mu4e-- version of
  ;; message-hide-headers, since older versions of the latter trigger some bug,
  ;; #2661.
  (let ((message-hidden-headers mu4e-draft-hidden-headers))
    (mu4e--message-hide-headers))

  ;; hooks
  (add-hook 'before-save-hook  #'mu4e--compose-before-save nil t)
  (add-hook 'after-save-hook   #'mu4e--compose-after-save nil t)
  (add-hook 'message-send-hook #'mu4e--compose-before-send nil t)
  (setq-local message-fcc-handler-function #'mu4e--fcc-handler)

  (mu4e--jump-to-a-reasonable-place)

  (set-buffer-modified-p nil)
  (undo-boundary))

;;
;; mu4e-compose-post-hook helpers

(defvar mu4e--before-draft-window-config nil
  "The window configuration just before creating the draft.")

(defun mu4e-compose-post-restore-window-configuration()
  "Function that might restore the window configuration.
I.e. the configuration just before the draft buffer appeared.
This is for use in `mu4e-compose-post-hook'.
See `set-window-configuration' for further details."
  (when mu4e--before-draft-window-config
    ;;(message "RESTORE to %s" mu4e--before-draft-window-config)
    (set-window-configuration mu4e--before-draft-window-config)
    (setq mu4e--before-draft-window-config nil)))

(defvar mu4e--draft-activation-frame nil
  "Frame from which composition was activated.
Used internally for mu4e-compose-post-kill-frame.")

(defun mu4e-compose-post-kill-frame ()
  "Function that might kill the composition frame.
This is for use in `mu4e-compose-post-hook'."
  (let ((msgframe (selected-frame)))
    ;;(message "kill frame? %s %s" mu4e--draft-activation-frame msgframe)
    (when (and (frame-live-p msgframe)
               (not (eq mu4e--draft-activation-frame msgframe)))
      (delete-frame msgframe))))

(defvar mu4e-message-post-action nil
  "Runtime variable for use with `mu4e-compose-post-hook'.
It contains a symbol denoting the action that triggered the hook,
either `send', `exit', `kill' or `postpone'.")

(defvar mu4e-compose-post-hook)
(defun mu4e--message-post-actions (trigger)
  "Invoked after we're done with a message with TRIGGER.

See `mu4e-message-post-action' for the available triggers.

I.e. this multiplexes the `message-(send|exit|kill|postpone)-actions';
with the mu4e-message-post-action set accordingly."
  (setq mu4e-message-post-action trigger)
  (run-hooks 'mu4e-compose-post-hook))

(defun mu4e--prepare-post (&optional oldframe oldwindconf)
    "Prepare the `mu4e-compose-post-hook` handling.

Set up some message actions. In particular, handle closing frames
when we created it. OLDFRAME is the frame from which the
message-composition was triggered. OLDWINDCONF is the current
window configuration."
    ;; remember current frame & window conf
    (setq mu4e--draft-activation-frame oldframe
          mu4e--before-draft-window-config oldwindconf)

    ;; make message's "post" hooks local, and multiplex them
    (make-local-variable 'message-send-actions)
    (make-local-variable 'message-postpone-actions)
    (make-local-variable 'message-exit-actions)
    (make-local-variable 'message-kill-actions)

    (push (lambda () (mu4e--message-post-actions 'send))
          message-send-actions)
    (push (lambda () (mu4e--message-post-actions 'postpone))
          message-postpone-actions)
    (push (lambda () (mu4e--message-post-actions 'exit))
          message-exit-actions)
    (push (lambda () (mu4e--message-post-actions 'kill))
          message-kill-actions))

;;
;; creating drafts
;;

(defun mu4e--draft (compose-type compose-func &optional parent)
  "Create a new message draft.

This is the central access point for creating new mail buffers;
when there's a parent message, use `mu4e--compose-with-parent'.

COMPOSE-TYPE is the type of message to create. COMPOSE-FUNC is a
function that must return a buffer that satisfies
`mu4e--validate-hidden-buffer'.

Optionally, PARENT is the message parent or nil. For compose-type
`reply' and `forward' we require a PARENT; for the other compose
it must be nil.

After this, user is presented with a message composition buffer.

Returns the new buffer."
  ;; run pre-hook early, so user can influence later steps.
  (let ((mu4e-compose-parent-message parent)
        (mu4e-compose-type compose-type))
    (run-hooks 'mu4e-compose-pre-hook))

  (mu4e--prepare-draft parent)
  ;; evaluate BODY; this must yield a hidden, live buffer. This is evaluated in
  ;; a temp buffer with contains the parent-message, if any. if there's a
  ;; PARENT, load the corresponding message into a temp-buffer before calling
  ;; compose-func
  (let ((draft-buffer)
        (oldframe (selected-frame))
        (oldwinconf (current-window-configuration)))
    (with-temp-buffer
      ;; provide a temp buffer so the compose-func can do its thing
      (setq draft-buffer (mu4e--validate-hidden-buffer (funcall compose-func)))
      (with-current-buffer draft-buffer
        ;; we have our basic buffer; turn it into a full mu4e composition
        ;; buffer.
        (mu4e--prepare-draft-buffer compose-type parent)))
    ;; we're ready for composition; let's display it in the way user configured
    ;; things: directly through display buffer (via pop-t or otherwise through
    ;; mu4e-window.
    (if (eq mu4e-compose-switch 'display-buffer)
        (pop-to-buffer draft-buffer)
      (mu4e-display-buffer draft-buffer 'do-select))
    ;; prepare possible message actions (such as cleaning-up)
    (mu4e--prepare-post oldframe oldwinconf)
    draft-buffer))

(defun mu4e--draft-with-parent (compose-type parent compose-func)
  "Draft a message based on some parent message.
COMPOSE-TYPE, COMPOSE-FUNC and PARENT are as in `mu4e--draft',
but note the different order."
  (mu4e--draft
   compose-type
   (lambda ()
     (let ( ;; only needed for Fwd. Gnus has a bad default.
           (message-make-forward-subject-function
            (list #'message-forward-subject-fwd))
           ;; e.g. supersede needs parent for 'message-is-yours-p
           (mu4e-compose-parent-message parent)
           (mu4e-compose-type compose-type))
       (insert (mu4e--decoded-message parent))
       ;; let's make sure we don't use message-reply-headers from
       ;; some unrelated message.
       (setq message-reply-headers nil)
       (funcall compose-func)))
   parent))

(provide 'mu4e-draft)
;;; mu4e-draft.el ends here
