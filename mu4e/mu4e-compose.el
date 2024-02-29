;;; mu4e-compose.el --- Compose and send messages -*- lexical-binding: t -*-

;; Copyright (C) 2011-2023 Dirk-Jan C. Binnema

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

;; Implements mu4e-compose-mode, which is a `message-mode' derivative. This is a
;; *fairly* thin wrapper around the gnus functions for message composition,
;; integrated with mu4e. Still, quite a bit of code to make it work nicely in
;; the mu4e context.


;; Code
(require 'message)
(require 'nnheader) ;; for make-full-mail-header

(require 'mu4e-obsolete)
(require 'mu4e-server)
(require 'mu4e-message)
(require 'mu4e-context)
(require 'mu4e-folders)


;;; User configuration for compose-mode
(defgroup mu4e-compose nil
  "Customization for composing/sending messages."
  :group 'mu4e)

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

(defcustom mu4e-compose-switch nil
  "Where to display the new message?
A symbol:
- nil       : default (new buffer)
- window    : compose in new window
- frame or t: compose in new frame

For backward compatibility with `mu4e-compose-in-new-frame', t is
treated as =\\'frame."
  :type 'symbol
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
y              (const :tag "Sign forwarded messages" sign-forwarded-messages)
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
variable `mu4e-compose-parent-message' is the message replied to
/ being forwarded / edited, and `mu4e-compose-type' contains the
type of message to be composed.

Note that there is no draft message yet when this hook runs, it
is meant for influencing the how mu4e constructs the draft
message. If you want to do something with the draft messages
after it has been constructed, `mu4e-compose-mode-hook' would be
the place to do that."
  :type 'hook
  :group 'mu4e-compose)


;;; Runtime variables; useful for user-hooks etc.
(defvar-local mu4e-compose-parent-message nil
  "The parent message plist.
This is the message being replied to, forwarded or edited; used
in `mu4e-compose-pre-hook'. For new (non-reply, forward etc.)
messages, it is nil.")

(defvar-local mu4e-compose-type nil
  "The compose-type for the current message.")


;;; Filenames
(defun mu4e--message-basename()
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
  "Construct a Fcc: path, based on PARENT and `mu4e-sent-messages-behavior'.

PARENT is either nil or the original message (being replied
to/forwarded etc.), and is used to determine the sent folder,
together with `mu4e-sent-messages-behavior'. BASE-NAME is the
base filename without any Maildir decoration.

Returns the path for the sent message, either in the sent or
trash folder, or nil if the message should be removed after
sending."
  (when-let ((sent-dir
          (pcase mu4e-sent-messages-behavior
            ('delete nil)
            ('trash (mu4e-get-trash-folder parent))
            ('sent (mu4e-get-sent-folder parent))
            ((pred functionp) (funcall mu4e-sent-messages-behavior))
            (_ (mu4e-error "Error in `mu4e-sent-messages-behavior'")))))
    (mu4e-join-paths
     (mu4e-root-maildir) sent-dir "cur"
     (format "%s%s2,S" base-name mu4e-maildir-info-delimiter))))


;;; address completion

;; inspired by org-contacts.el and
;; https://github.com/nordlow/elisp/blob/master/mine/completion-styles-cycle.el

(defun mu4e--compose-complete-handler (str pred action)
  "Complete address STR with predication PRED for ACTION."
  (cond
   ((eq action nil)
    (try-completion str mu4e--contacts-set pred))
   ((eq action t)
    (all-completions str mu4e--contacts-set pred))
   ((eq action 'metadata)
    ;; our contacts are already sorted - just need to tell the completion
    ;; machinery not to try to undo that...
    '(metadata
      (display-sort-function . identity)
      (cycle-sort-function   . identity)))))

(defconst mu4e--header-separator ;; XX properties down work... why not?
  (propertize "--text follows this line--" 'read-only t 'intangible t)
  "Line used to separate headers from text in messages being composed.")

(defun mu4e--compose-complete-contact (&optional start)
  "Complete the text at START with a contact.
Ie. either \"name <email>\" or \"email\")."
  (interactive)
  (let ((mail-abbrev-mode-regexp
         "^\\(To\\|B?Cc\\|Reply-To\\|From\\|Sender\\):")
        (mail-header-separator mu4e--header-separator)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t))))
    ;; try to complete only when we're in the headers area, looking at an
    ;; address field.
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point)))))
        (list start end 'mu4e--compose-complete-handler)))))

(defun mu4e--compose-setup-completion ()
  "Set up auto-completion of addresses."
  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'completion-cycle-threshold) 7)
  (add-to-list (make-local-variable 'completion-styles) 'substring)
  (add-hook 'completion-at-point-functions
            'mu4e--compose-complete-contact nil t))


(defun mu4e--fcc-handler (msgpath)
  "Handle Fcc: for MSGPATH.

This ensures that a copy of a sent messages ends up in the
appropriate sent-messages folder.

If MSGPATH is nil, do nothing."
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

(defun mu4e--delimit-headers (&optional undelimit)
  "Delimit or undelimit (with UNDELIMIT) headers."
  (let ((mail-header-separator mu4e--header-separator)
        (inhibit-read-only t))
    (save-excursion
      (mail-sendmail-undelimit-header) ;; clear first
      (unless undelimit (mail-sendmail-delimit-header)))))

(defun mu4e--compose-before-save ()
  "Function called just before the draft buffer is saved."
  ;; This does 3 things:
  ;;  - set the Message-Id if not already
  ;;  - set the Date if not already
  ;;  - (temporarily) remove the mail-header separator
  (setq mu4e--compose-undo buffer-undo-list)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (unless (message-fetch-field "Message-ID")
        (message-generate-headers '(Message-ID)))
      (message-generate-headers '(Date)))
    (mu4e--delimit-headers 'undelimit))) ;; remove separator

(defvar mu4e--compose-buffer-max-name-length 48)
(defun mu4e--compose-set-friendly-buffer-name ()
  "Use some friendly name for this composition buffer."
  (let* ((subj (message-field-value "subject"))
         (subj (if (or (not subj) (string-match "^[:blank:]*$" subj))
                   "No subject"  subj)))
    (rename-buffer (generate-new-buffer-name
                    (format "\"%s\""
                            (truncate-string-to-width subj
                             mu4e--compose-buffer-max-name-length
                             0 nil t)))
                   (buffer-name))))

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
  (let ((buf (find-file-noselect path)))
    (when buf
      (with-current-buffer buf
        (message-narrow-to-headers-or-head)
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
                  ;; the last will be the first
                  (setq forwarded-from (car refs))))))
          ;; remove the <> and update the flags on the server-side.
          (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
            (mu4e--server-move (match-string 1 in-reply-to) nil "+R-N"))
          (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
            (mu4e--server-move (match-string 1 forwarded-from) nil "+P-N")))))))

(defun mu4e--compose-after-save()
  "Function called immediately after the draft buffer is saved."
  ;; This does 3 things:
  ;; - restore the mail-header-separator (see mu4e--compose-before-save)
  ;; - update the buffer name (based on the message subject
  ;; - tell the mu server about the updated draft message
  (mu4e--delimit-headers)
  (mu4e--compose-set-friendly-buffer-name)
  ;; tell the server
  (mu4e--server-add (buffer-file-name))
  ;; restore history.
  (set-buffer-modified-p nil)
  (setq buffer-undo-list mu4e--compose-undo))

(defun mu4e-sent-handler (docid path)
  "Handler called with DOCID and PATH for the just-sent message.
For Forwarded ('Passed') and Replied messages, try to set the
appropriate flag at the message forwarded or replied-to."
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

(defun mu4e--compose-before-send ()
  "Function called just before sending a message."
  ;; Remove References: if In-Reply-To: is missing.
  ;; This allows the user to effectively start a new message-thread by
  ;; removing the In-Reply-To header.
  (when (eq mu4e-compose-type 'reply)
    (unless (message-fetch-field "In-Reply-To")
      (message-remove-header "References")))
  (when use-hard-newlines
    (mu4e--send-harden-newlines))
  ;; for safety, always save the draft before sending
  (set-buffer-modified-p t)
  (save-buffer))

(defun mu4e--compose-after-send ()
  "Function called just after sending a message."
  (setq mu4e-sent-func #'mu4e-sent-handler)
  (mu4e--server-sent (buffer-file-name)))

;; (defun mu4e-message-kill-buffer ()
;;   "Wrapper around `message-kill-buffer'.
;; It attempts to restore some mu4e window layout after killing the
;; compose-buffer."
;;   (interactive)
;;   (let ((view (save-selected-window (mu4e-get-view-buffer)))
;;         (hdrs (mu4e-get-headers-buffer)))
;;     (message-kill-buffer)
;;     ;; try to go back to some mu window if it is live; otherwise do nothing.
;;     (if (buffer-live-p view)
;;         (switch-to-buffer view)
;;       (when (and (buffer-live-p hdrs))
;;         (switch-to-buffer hdrs)))))

(defalias 'mu4e-message-kill-buffer 'message-kill-buffer)

;;; Crypto
(defun mu4e--compose-setup-crypto (parent compose-type)
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

 ;;; mu4e-compose-mode
(defun mu4e--compose-remap-faces ()
  "Remap `message-mode' faces to mu4e ones.

Our parent `message-mode' uses font-locking for the compose
buffers; lets remap its faces so it uses the ones for mu4e."
  ;; normal headers
  (face-remap-add-relative 'message-header-name  'mu4e-header-field-face)
  (face-remap-add-relative 'message-header-other 'mu4e-header-value-face)
  ;; special headers
  (face-remap-add-relative 'message-header-from 'mu4e-contact-face)
  (face-remap-add-relative 'message-header-to   'mu4e-contact-face)
  (face-remap-add-relative 'message-header-cc   'mu4e-contact-face)
  (face-remap-add-relative 'message-header-bcc  'mu4e-contact-face)
  (face-remap-add-relative 'message-header-subject
                           'mu4e-special-header-value-face))

(defvar mu4e-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map message-mode-map)
    (define-key map (kbd "C-S-u")               #'mu4e-update-mail-and-index)
    (define-key map (kbd "C-c C-u")             #'mu4e-update-mail-and-index)
    (define-key map [remap message-kill-buffer] #'mu4e-message-kill-buffer)
    ;; remove some unsupported commands... [remap ..]  does not work here
    ;; XXX remove from menu, too.
    (define-key map (kbd "C-c C-f C-n")   nil) ;; message-goto-newsgroups
    (define-key map (kbd "C-c C-n")       nil) ;; message-insert-newsgroups
    (define-key map (kbd "C-c C-j")       nil) ;; gnus-delay-article
    map)
  "The keymap for mu4e-compose buffers.")

(defun mu4e--compose-unsupported (&rest _args)
  "Advise wrapper for Gnus unsupported functions in mu4e."
  (when (eq major-mode 'mu4e-compose-mode)
    (mu4e-warn "Not available in mu4e")))

(defun mu4e--neutralize-undesirables ()
  "Beware Gnus commands that do not work with mu4e."
  ;; the Field menu contains many items that don't apply.
  (advice-add 'gnus-delay-article
              :before #'mu4e--compose-unsupported) ;; # XXX does not work?!
  (advice-add 'message-goto-newsgroups :before #'mu4e--compose-unsupported)
  (advice-add 'message-insert-newsgroups :before #'mu4e--compose-unsupported))

(define-derived-mode mu4e-compose-mode message-mode "mu4e:compose"
  "Major mode for the mu4e message composition, derived from `message-mode'.
\\{mu4e-compose-mode-map}."
  (progn
    (use-local-map mu4e-compose-mode-map)
    (mu4e-context-minor-mode)
    (mu4e--neutralize-undesirables)
    (mu4e--compose-remap-faces)
    (setq-local nobreak-char-display nil)
    ;; set this to allow mu4e to work when gnus-agent is unplugged in gnus
    (set (make-local-variable 'message-send-mail-real-function) nil)
    ;; Set to nil to enable `electric-quote-local-mode' to work:
    (set (make-local-variable 'comment-use-syntax) nil)
    ;; offer completion for e-mail addresses
    (when mu4e-compose-complete-addresses
      (mu4e--compose-setup-completion))
    ;; format-flowed
    (if mu4e-compose-format-flowed
        (progn
          (turn-off-auto-fill)
          (setq truncate-lines nil
                word-wrap t
                mml-enable-flowed t
                use-hard-newlines t)
          (visual-line-mode t))
      (setq mml-enable-flowed nil))))

(declare-function mu4e-view-message-text "mu4e-view")

(defun mu4e-message-cite-nothing ()
  "Function for `message-cite-function' that cites _nothing_."
  (save-excursion
    (message-cite-original-without-signature)
    (delete-region (point-min) (point-max))))

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
    (unless (message-fetch-field "Message-Id")
      (goto-char (point-min))
      (insert (format "Message-Id: <%s>\n" (plist-get msg :message-id))))
    (mu4e--delimit-headers 'undelimit)
    (ignore-errors (run-hooks 'gnus-article-decode-hook))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mu4e--compose-cite (msg)
  "Return a cited version of the ORIG message (a string).
This function uses `message-cite-function', and its settings apply."
  (with-temp-buffer
    (insert (mu4e-view-message-text msg))
    (goto-char (point-min))
    (push-mark (point-max))
    (let ((message-signature-separator "^-- *$")
          (message-signature-insert-empty-line t))
      (funcall message-cite-function))
    (pop-mark)
    (goto-char (point-min))
    (buffer-string)))

(defvar mu4e-user-agent-string
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)
  "The User-Agent string for mu4e, or nil.")

(defun mu4e--compose-switch-function ()
  "Function to switch & display composition buffer.
Based on the value of `mu4e-compose-switch'."
  (pcase mu4e-compose-switch
    ('nil           #'switch-to-buffer)
    ('window        #'switch-to-buffer-other-window)
    ((or 'frame 't) #'switch-to-buffer-other-frame)
    ;; t for backward compatibility with mu4e-compose-in-new-frame
    (_             (mu4e-error "Invalid mu4e-compose-switch"))))

(defun mu4e--fake-pop-to-buffer (name &optional _switch)
  "A fake `message-pop-to-buffer' which creates NAME.
This is a little glue to use `message-reply', `message-forward'
etc. We cannot use the normal `message-pop-to-buffer' since we're
not ready yet to show the buffer in mu4e."
  (set-buffer (get-buffer-create name))
  (erase-buffer)
  (current-buffer))

(defun mu4e--headers (compose-type)
  "Determine headers needed for message."
  (seq-filter #'identity ;; ensure needed headers are generated.
       `(From Subject Date Message-ID
        ,(when (memq compose-type '(reply forward)) 'References)
        ,(when (eq compose-type 'reply) 'In-Reply-To)
        ,(when message-newsreader 'User-Agent)
        ,(when message-user-organization 'Organization))))

(defun mu4e--compose-setup-buffer (compose-type compose-func parent)
  "Set up a buffer for message composition before mu4e-compose-mode.

COMPOSE-FUNC is a function / lambda to create the specific type
of message; it should return (but not show) the created buffer.

PARENT is the \"parent\" message; nil
 for a \\='new message, set for all others (the message replied to /
 forwarded / ...)."
  (with-temp-buffer
    ;; call the call message function; turn off the gnus crypto stuff;
    ;; we handle that ourselves below
    (let* ((message-this-is-mail t)
           (message-generate-headers-first nil)
           (message-newsreader mu4e-user-agent-string)
           (message-mail-user-agent nil))
      ;; we handle it ourselves.
      (setq-local gnus-message-replysign nil
                  gnus-message-replyencrypt nil
                  gnus-message-replysignencrypted nil)
      (goto-char (point-min))
      ;; annoyingly, various message- functions call `message-pop-to-buffer`
      ;; (showing the message. But we're not ready for that yet. So
      ;; temporarily override that.
      (advice-add 'message-pop-to-buffer
                  :override #'mu4e--fake-pop-to-buffer)
      (funcall compose-func parent)
      ;; add some more headers, if needed.
      (message-generate-headers (mu4e--headers compose-type))
      (advice-remove 'message-pop-to-buffer #'mu4e--fake-pop-to-buffer)
      (current-buffer)))) ;; returns new buffer (this is not the tmp buf)


(defvar mu4e-compose-hidden-headers
  (append message-hidden-headers '("^User-agent:" "^Fcc:"))
  "Message headers to hide when composing.
This is mu4e's version of `message-hidden-headers'.")

(defun mu4e--message-is-yours-p (func &rest args)
  "Mu4e advice for `message-is-yours'.
Is this address yours?"
  (if (mu4e-running-p)
      (let ((sender (message-fetch-field "from"))
            (from (message-fetch-field "sender")))
        (or (and sender (mu4e-personal-or-alternative-address-p
                         (car (mail-header-parse-address sender))))
            (and from (mu4e-personal-or-alternative-address-p
                       (car (mail-header-parse-address from))))))
    (apply func args)))

(defun mu4e--compose-setup-post (compose-type &optional parent)
  "Prepare the new message buffer."
  (mu4e-compose-mode)
  ;; remember some variables, e.g for user hooks.
  (setq-local
   mu4e-compose-parent-message parent
   mu4e-compose-type compose-type)

  (mu4e--compose-setup-crypto parent compose-type)
  ;; set the attachment dir to something more reasonable than the draft
  ;; directory.
  (setq default-directory (mu4e-determine-attachment-dir))

  (add-hook 'before-save-hook  #'mu4e--compose-before-save nil t)
  (add-hook 'after-save-hook   #'mu4e--compose-after-save nil t)
  (add-hook 'message-send-hook #'mu4e--compose-before-send nil t)
  (add-hook 'message-sent-hook #'mu4e--compose-after-send nil t)

  (when-let ((fcc-path (mu4e--fcc-path (mu4e--message-basename) parent)))
    (message-add-header (concat "Fcc: " fcc-path "\n")))
  (setq-local message-fcc-handler-function #'mu4e--fcc-handler)

  (mu4e--compose-set-friendly-buffer-name)
  (let ((message-hidden-headers mu4e-compose-hidden-headers))
    (message-hide-headers))

  ;; jump to some reasonable place.
  (if (not (message-field-value "To"))
      (message-goto-to)
    (if (not (message-field-value "Subject"))
        (message-goto-subject)
      (message-goto-body)))
  ;; buffer is not user-modified yet
  (set-buffer-modified-p nil)
  (undo-boundary))

(defun mu4e--compose-setup (compose-type compose-func &optional switch)
  "Set up a new buffer for mu4e message composition.

COMPOSE-TYPE is a symbol for message-kind; one of \\='(new reply forward edit)
PARENT is the \"parent\" message; nil for a \\='new message, set for
all others (the message replied to / forwarded / ...).

COMPOSE-FUNC is a function / lambda to create the specific type
of message.

Optionally, SWITCH determines how to find a buffer for the message
(see SWITCH-FUNCTION in `compose-mail')."
  (cl-assert (member compose-type '(reply forward edit new)))
  (unless (mu4e-running-p) (mu4e 'background)) ;; start if needed
  (let* ((parent
          (when (member compose-type '(reply forward edit))
            (mu4e-message-at-point)))
         (mu4e-compose-parent-message parent)
         (mu4e-compose-type compose-type)
         (oldframe (selected-frame))
         (buf))
    (advice-add 'message-is-yours-p :around #'mu4e--message-is-yours-p)
    (run-hooks 'mu4e-compose-pre-hook) ;; run the pre-hook. Still useful?
    (mu4e--context-autoswitch parent mu4e-compose-context-policy)
    (with-current-buffer
        (mu4e--compose-setup-buffer compose-type compose-func parent)
      (funcall (or switch (mu4e--compose-switch-function)) (current-buffer))
      (unless (eq compose-type 'edit)
        (set-visited-file-name ;; make it a draft file
         (mu4e--draft-message-path (mu4e--message-basename) parent)))
      (mu4e--compose-setup-post compose-type parent)
      (setq buf (current-buffer))
      (display-buffer buf)
      (let* ((msgframe (selected-frame))
             (actions (list
                       (lambda () ;; kill frame when it was created for this
                         (unless (eq oldframe msgframe)
                           (delete-frame msgframe))))))
        ;; handle closing of frames.
        (setq-local ;;message-kill-actions actions
         message-return-actions actions
         message-send-actions actions
         message-kill-actions actions)))))


;;;###autoload
(defun mu4e-compose-new (&optional to subject other-headers continue
                                   switch-function yank-action send-actions
                                   return-action &rest _)
  "This is mu4e's implementation of `compose-mail'. TO, SUBJECT,
OTHER-HEADERS, CONTINUE, SWITCH-FUNCTION, YANK-ACTION
SEND-ACTIONS RETURN-ACTION are as described in `compose-mail',
and to the extend that they do not conflict with mu4e inner
workings."
  (interactive)
  (mu4e--compose-setup
   'new (lambda (_parent)
          (message-mail to subject other-headers continue nil
                        yank-action send-actions return-action))
   switch-function))

;;;###autoload
(defalias 'mu4e-compose-mail #'mu4e-compose-new)

;;;###autoload
(defun mu4e-compose-reply (&optional wide)
  "Reply to the message at point.
If WIDE is non-nil, make it a \"wide\" reply (a.k.a.
\"reply-to-all\")."
  (interactive)
  (mu4e--compose-setup
   'reply
   (lambda (parent)
     (insert (mu4e--decoded-message parent 'headers-only))
     (message-reply nil wide)
     (message-goto-body)
     (insert (mu4e--compose-cite parent)))))

;;;###autoload
(defun mu4e-compose-wide-reply ()
  "Wide-reply to the message at point.
A.k.a., \"reply-to-all\"."
  (interactive) (mu4e-compose-reply 'wide))

;;;###autoload
(defun mu4e-compose-supersede ()
  "Supersede message at point.

That is, send the message again, with all the same recipients;
this can be useful to follow-up on a sent message. The message
must be from current user, as determined through
`mu4e-personal-or-alternative-address-p'."
  (interactive)
  (mu4e--compose-setup
   'reply ;; it's a special kind of reply.
   (lambda (parent)
     (insert (mu4e--decoded-message parent))
     (set-buffer-modified-p nil)
     (message-supersede))))

;;;###autoload
(defun mu4e-compose-forward ()
  "Forward the message at point."
  (interactive)
  (mu4e--compose-setup
   'forward
   (lambda (parent)
     (let ((message-make-forward-subject-function
            #'message-forward-subject-fwd))
       (insert (mu4e--decoded-message parent))
       (mu4e--delimit-headers)
       ;; message-forward expects message-reply-headers to be set up; here we
       ;; only need message-id & references, rest is for completeness.
       (setq-local message-reply-headers
             (make-full-mail-header
              0
              (or (message-fetch-field "subject") "none")
              (or (message-fetch-field "from") "nobody")
              (message-fetch-field "date")
              (message-fetch-field "message-id" t)
              (message-fetch-field "references")
              0 0 ""))
       (mu4e--delimit-headers 'undelimit)
       (set-buffer-modified-p nil)
       (message-forward)))))

;;;###autoload
(defun mu4e-compose-edit()
  "Edit an existing draft message."
  (interactive)
  (let* ((msg (mu4e-message-at-point)))
    (unless  (member 'draft (mu4e-message-field msg :flags))
      (mu4e-warn "Cannot edit non-draft messages"))
    (mu4e--compose-setup
     'edit
     (lambda (parent)
       (find-file (plist-get parent :path))
       (mu4e--delimit-headers)))))

;;;###autoload
(defun mu4e-compose-resend (address)
  "Re-send the message at point.
The message is resent as-is, without any editing. "
  (interactive
   (list (completing-read
          "Resend message to address: " mu4e--contacts-set)))
  (unless (mu4e-running-p) (mu4e 'background))
  (let ((path (plist-get (mu4e-message-at-point) :path)))
    (with-temp-buffer
      (insert-file-contents path)
      (message-resend address))))

;;; Compose Mail

(declare-function mu4e "mu4e")

;;;###autoload
(define-mail-user-agent 'mu4e-user-agent
  #'mu4e-compose-mail
  #'message-send-and-exit
  #'mu4e-message-kill-buffer
  'message-send-hook)

;; Without this, `mail-user-agent' cannot be set to `mu4e-user-agent'
;; through customize, as the custom type expects a function.  Not
;; sure whether this function is actually ever used; if it is then
;; returning the symbol is probably the correct thing to do, as other
;; such functions suggest.
(defun mu4e-user-agent ()
  "Return the `mu4e-user-agent' symbol."
  'mu4e-user-agent)

;;; minor mode for use in other modes.
(defvar mu4e-compose-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "R" #'mu4e-compose-reply)
    (define-key map "W" #'mu4e-compose-wide-reply)
    (define-key map "F" #'mu4e-compose-forward)
    (define-key map "E" #'mu4e-compose-edit)
    (define-key map "C" #'mu4e-compose-new)
    map)
  "Keymap for compose minor-mode.")

(define-minor-mode mu4e-compose-minor-mode
  "Mode for searching for messages."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap mu4e-compose-minor-mode-map)

(defvar mu4e--compose-menu-items
  '("--"
    ["Compose new" mu4e-compose-new
     :help "Compose new message"]
    ["Reply" mu4e-compose-reply
     :help "Reply to message"]
    ["Reply to all" mu4e-compose-wide-reply
     :help "Reply to all-recipients"]
    ["Forward" mu4e-compose-forward
     :help "Forward message"]
    ["Resend" mu4e-compose-resend
     :help "Re-send message"])
  "Easy menu items for message composition.")
 ;;;
(provide 'mu4e-compose)
;;; mu4e-compose.el ends here
