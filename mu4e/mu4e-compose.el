;; -*-mode: emacs-lisp; tab-width: 8; indent-tabs-mode: t -*-
;; mu4e-compose.el -- part of mu4e, the mu mail user agent for emacs
;;
;; Copyright (C) 2011-2016 Dirk-Jan C. Binnema

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

;; In this file, various functions to compose/send messages, piggybacking on
;; gnus' message mode


;; Magic / Rupe Goldberg

;; 1) When we reply/forward a message, we get it from the backend, ie:
;; we send to the backend (mu4e-compose):
;;     compose type:reply docid:30935
;; backend responds with:
;;      (:compose reply :original ( .... <original message> ))


;; 2) When we compose a message, message and headers are separated by
;; `mail-header-separator', ie. '--text follows this line--. We use
;; before-save-hook and after-save-hook to remove/re-add this special line, so
;; it stays in the buffer, but never hits the disk.
;; see:
;;     mu4e~compose-insert-mail-header-separator
;;     mu4e~compose-remove-mail-header-separator
;;
;; (maybe we can get away with remove it only just before sending? what does
;; gnus do?)

;; 3) When sending a message, we want to do a few things:
;;   a) move the message from drafts to the sent folder (maybe; depends on
;;      `mu4e-sent-messages-behavior')
;;   b) if it's a reply, mark the replied-to message as "R", i.e. replied
;;      if it's a forward, mark the forwarded message as "P", i.e. passed (forwarded)
;;   c) kill all buffers looking at the sent message

;;  a) is dealt with by message-mode, but we need to tell it where to move the
;;     sent message. We do this by adding an Fcc: header with the target folder,
;;     see `mu4e~compose-setup-fcc-maybe'. Since message-mode does not natively
;;     understand maildirs, we also need to tell it what to do, so we also set
;;     `message-fcc-handler-function' there. Finally, we add the the message in
;;     the sent-folder to the database.
;;
;;   b) this is handled in `mu4e~compose-set-parent-flag'
;;
;;   c) this is handled in our handler for the `sent'-message from the backend
;;   (`mu4e-sent-handler')
;;

;;; Code:

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'message)
(require 'mail-parse)
(require 'smtpmail)
(require 'rfc2368)

(require 'mu4e-utils)
(require 'mu4e-vars)
(require 'mu4e-proc)
(require 'mu4e-actions)
(require 'mu4e-message)
(require 'mu4e-draft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing / Sending messages
(defgroup mu4e-compose nil
  "Customizations for composing/sending messages."
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
which takes no arguments, and which should return on of the mentioned symbols,
for example:

  (setq mu4e-sent-messages-behavior (lambda ()
        (if (string= (message-sendmail-envelope-from) \"foo@example.com\")
                   'delete 'sent)))

The various `message-' functions from `message-mode' are available
for querying the message information."
  :type '(choice (const :tag "move message to mu4e-sent-folder" sent)
	   (const :tag "move message to mu4e-trash-folder" trash)
	   (const :tag "delete message" delete))
  :safe 'symbolp
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
	   (const :tag "Always ask what context to use" 'always-ask)
	   (const :tag "Ask if none of the contexts match" 'ask)
	   (const :tag "Ask when there's no context yet" 'ask-if-none)
	   (const :tag "Pick the first context if none match" 'pick-first)
	   (const :tag "Don't change the context when none match" nil)
	   :safe 'symbolp
	   :group 'mu4e-compose))


(defcustom mu4e-compose-crypto-reply-policy 'sign-and-encrypt
  "Policy for signing/encrypting replies to encrypted messages.
We have the following choices:

- `sign': sign the reply
- `sign-and-encrypt': sign and encrypt the repy
- `encrypt': encrypt the reply, but don't sign it.
-  anything else: do nothing."
  :type '(choice
	   (const :tag "Sign the reply" 'sign)
	   (const :tag "Sign and encrypt the reply" 'sign-and-encrypt)
	   (const :tag "Encrypt the reply" 'encrypt)
	   (const :tag "Don't do anything" nil)
	   :safe 'symbolp
	   :group 'mu4e-compose))

(defcustom mu4e-compose-format-flowed nil
  "Whether to compose messages to be sent as format=flowed (or
   with long lines if `use-hard-newlines' is set to nil).  The
   variable `fill-flowed-encode-column' lets you customize the
   width beyond which format=flowed lines are wrapped."
  :type 'boolean
  :safe 'booleanp
  :group 'mu4e-compose)

(defcustom mu4e-compose-pre-hook nil
  "Hook run just *before* message composition starts.
If the compose-type is either 'reply' or 'forward', the variable
`mu4e-compose-parent-message' points to the message replied to /
being forwarded / edited.

Note that there is no draft message yet when this hook runs, it
is meant for influencing the how mu4e constructs the draft
message. If you want to do something with the draft messages after
it has been constructed, `mu4e-compose-mode-hook' would be the
place to do that."
  :type 'hook
  :group 'mu4e-compose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e-compose-attach-captured-message ()
  "Insert the last captured message file as an attachment.
Messages are captured with `mu4e-action-capture-message'."
  (interactive)
  (unless mu4e-captured-message
    (mu4e-warn "No message has been captured"))
  (let ((path (plist-get mu4e-captured-message :path)))
    (unless (file-exists-p path)
      (mu4e-warn "Captured message file not found"))
    (mml-attach-file
      path
      "message/rfc822"
      (or (plist-get mu4e-captured-message :subject) "No subject")
      "attachment")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 'fcc' refers to saving a copy of a sent message to a certain folder. that's
;; what these 'Sent mail' folders are for!
;;
;; We let message mode take care of this by adding a field

;;   Fcc: <full-path-to-message-in-target-folder>

;; in the "message-send-hook" (ie., just before sending).  message mode will
;; then take care of the saving when the message is actually sent.
;;
;; note, where and if you make this copy depends on the value of
;; `mu4e-sent-messages-behavior'.

(defun mu4e~compose-setup-fcc-maybe ()
  "Maybe setup Fcc, based on `mu4e-sent-messages-behavior'.
If needed, set the Fcc header, and register the handler function."
  (let* ((sent-behavior
	   ;; Note; we cannot simply use functionp here, since at least
	   ;; delete is a function, too...
	   (if (member mu4e-sent-messages-behavior '(delete trash sent))
	     mu4e-sent-messages-behavior
	     (if (functionp mu4e-sent-messages-behavior)
	       (funcall mu4e-sent-messages-behavior)
	       mu4e-sent-messages-behavior)))
	  (mdir
	    (case sent-behavior
	      (delete nil)
	      (trash (mu4e-get-trash-folder mu4e-compose-parent-message))
	      (sent (mu4e-get-sent-folder mu4e-compose-parent-message))
	      (otherwise
		(mu4e-error "unsupported value '%S' `mu4e-sent-messages-behavior'."
		  mu4e-sent-messages-behavior))))
	  (fccfile (and mdir
		     (concat mu4e-maildir mdir "/cur/"
		       (mu4e~draft-message-filename-construct "S")))))
    ;; if there's an fcc header, add it to the file
    (when fccfile
      (message-add-header (concat "Fcc: " fccfile "\n"))
      ;; sadly, we cannot define as 'buffer-local'...  this will screw up gnus
      ;; etc. if you run it after mu4e so, (hack hack) we reset it to the old
      ;; handler after we've done our thing.
      (setq message-fcc-handler-function
	(lexical-let ((maildir mdir) (old-handler message-fcc-handler-function))
	  (lambda (file)
	    (setq message-fcc-handler-function old-handler) ;; reset the fcc handler
	    (write-file file)		       ;; writing maildirs files is easy
	    (mu4e~proc-add file (or maildir "/")))))))) ;; update the database

(defvar mu4e-compose-hidden-headers
  `("^References:" "^Face:" "^X-Face:"
     "^X-Draft-From:" "^User-agent:")
  "Hidden headers when composing.")

(defun mu4e~compose-hide-headers ()
  "Hide the headers as per `mu4e-compose-hidden-headers'."
  (let ((message-hidden-headers mu4e-compose-hidden-headers))
    (message-hide-headers)))

(defconst mu4e~compose-address-fields-regexp
  "^\\(To\\|B?Cc\\|Reply-To\\|From\\):")

(defun mu4e~compose-register-message-save-hooks ()
  "Just before saving, we remove the mail-header-separator; just
after saving we restore it; thus, the separator should never
appear on disk."
  (add-hook 'before-save-hook
    'mu4e~draft-remove-mail-header-separator nil t)
  (add-hook 'after-save-hook
    (lambda ()
      (mu4e~compose-set-friendly-buffer-name)
      (mu4e~draft-insert-mail-header-separator)
      ;; hide some headers again
      (mu4e~compose-hide-headers)
      (set-buffer-modified-p nil)
      (mu4e-message "Saved (%d lines)" (count-lines (point-min) (point-max)))
      ;; update the file on disk -- ie., without the separator
      (mu4e~proc-add (buffer-file-name) mu4e~draft-drafts-folder)) nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; address completion; inspired by org-contacts.el and
;; https://github.com/nordlow/elisp/blob/master/mine/completion-styles-cycle.el
(defun mu4e~compose-complete-handler (str pred action)
  (cond
    ((eq action nil)
      (try-completion str mu4e~contacts pred))
    ((eq action t)
      (all-completions str mu4e~contacts pred))
    ((eq action 'metadata)
      ;; our contacts are already sorted - just need to tell the
      ;; completion machinery not to try to undo that...
      '(metadata
	 (display-sort-function . mu4e~sort-contacts-for-completion)
	 (cycle-sort-function   . mu4e~sort-contacts-for-completion)))))

(defun mu4e~compose-complete-contact (&optional start)
  "Complete the text at START with a contact.
Ie. either 'name <email>' or 'email')."
  (interactive)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
	 (eoh ;; end-of-headers
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward-regexp mail-header-separator nil t))))
    ;; try to complete only when we're in the headers area,
    ;; looking  at an address field.
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
	      (start
		(or start
		  (save-excursion
		    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		    (goto-char (match-end 0))
		    (point)))))
	(list start end 'mu4e~compose-complete-handler)))))

(defun mu4e~compose-setup-completion ()
  "Set up auto-completion of addresses."
  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'completion-cycle-threshold) 7)
  (add-to-list (make-local-variable 'completion-styles) 'substring)
  (add-hook 'completion-at-point-functions
    'mu4e~compose-complete-contact nil t))

(defun mu4e~remove-refs-maybe ()
  "Remove the References: header if the In-Reply-To header is
missing. This allows the user to effectively start a new
message-thread by removing the In-Reply-To header."
  (unless (message-fetch-field "in-reply-to")
    (message-remove-header "References")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-compose-mode-map nil
  "Keymap for \"*mu4e-compose*\" buffers.")
(unless mu4e-compose-mode-map
  (setq mu4e-compose-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-S-u")   'mu4e-update-mail-and-index)
      (define-key map (kbd "C-c C-u") 'mu4e-update-mail-and-index)
      (define-key map (kbd "C-c C-k") 'mu4e-message-kill-buffer)
      (define-key map (kbd "M-q")     'mu4e-fill-paragraph)
      map)))

(defun mu4e-fill-paragraph (&optional region)
  "If `use-hard-newlines', takes a multi-line paragraph and makes
it into a single line of text.  Assume paragraphs are separated
by blank lines.  If `use-hard-newlines' is not enabled, this
simply executes `fill-paragraph'."
  ;; Inspired by https://www.emacswiki.org/emacs/UnfillParagraph
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (if mu4e-compose-format-flowed
    (let ((fill-column (point-max))
	   (use-hard-newlines nil)); rfill "across" hard newlines
      (fill-paragraph nil region))
    (fill-paragraph nil region)))

(defun mu4e-toggle-use-hard-newlines ()
  (interactive)
  (setq use-hard-newlines (not use-hard-newlines))
  (if use-hard-newlines
    (turn-off-auto-fill)
    (turn-on-auto-fill)))

(defun mu4e~compose-remap-faces ()
  "Our parent `message-mode' uses font-locking for the compose
buffers; lets remap its faces so it uses the ones for mu4e."
  ;; normal headers
  (face-remap-add-relative 'message-header-name
    '((:inherit mu4e-header-key-face)))
  (face-remap-add-relative 'message-header-other
    '((:inherit mu4e-header-value-face)))
  ;; special headers
  (face-remap-add-relative 'message-header-from
    '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-to
    '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-cc
    '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-bcc
    '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-subject
    '((:inherit mu4e-special-header-value-face)))
  ;; citation
  (face-remap-add-relative 'message-cited-text
    '((:inherit mu4e-cited-1-face))))


(defvar mu4e-compose-mode-abbrev-table nil)
(define-derived-mode mu4e-compose-mode message-mode "mu4e:compose"
  "Major mode for the mu4e message composition, derived from `message-mode'.
\\{message-mode-map}."
  (progn
    (use-local-map mu4e-compose-mode-map)
    (set (make-local-variable 'global-mode-string) '(:eval (mu4e-context-label)))
    (set (make-local-variable 'message-signature) mu4e-compose-signature)
    ;; set this to allow mu4e to work when gnus-agent is unplugged in gnus
    (set (make-local-variable 'message-send-mail-real-function) nil)
    (make-local-variable 'message-default-charset)
    ;; message-mode has font-locking, but uses its own faces. Let's
    ;; use the mu4e-specific ones instead
    (mu4e~compose-remap-faces)
    ;; if the default charset is not set, use UTF-8
    (unless message-default-charset
      (setq message-default-charset 'utf-8))
    ;; make sure mu4e is started in the background (ie. we don't want to error
    ;; out when sending the message; better to do it now if there's a problem)
    (mu4e~start) ;; start mu4e in background, if needed
    (mu4e~compose-register-message-save-hooks)
    ;; set the default directory to the user's home dir; this is probably more
    ;; useful e.g. when finding an attachment file the directory the current
    ;; mail files lives in...
    (setq default-directory (expand-file-name "~/"))
    ;; offer completion for e-mail addresses
    (when mu4e-compose-complete-addresses
      (mu4e~compose-setup-completion))

    (when mu4e-compose-format-flowed
      (turn-off-auto-fill)
      (setq truncate-lines nil
	word-wrap t
	use-hard-newlines t)
      ;; Set the marks in the fringes before activating visual-line-mode
      (set (make-local-variable 'visual-line-fringe-indicators)
	'(left-curly-arrow right-curly-arrow))
      (visual-line-mode t))

    (when (lookup-key message-mode-map [menu-bar text])
      (define-key-after
	(lookup-key message-mode-map [menu-bar text])
	[mu4e-hard-newlines]
	'(menu-item "Format=flowed" mu4e-toggle-use-hard-newlines
	   :button (:toggle . use-hard-newlines)
	   :help "Toggle format=flowed"
	   :visible (eq major-mode 'mu4e-compose-mode)
	   :enable mu4e-compose-format-flowed)
	'sep))

    (when (lookup-key mml-mode-map [menu-bar Attachments])
      (define-key-after
	(lookup-key mml-mode-map [menu-bar Attachments])
	[mu4e-compose-attach-captured-message]
	'(menu-item "Attach captured message"
	   mu4e-compose-attach-captured-message
	   :help "Attach message captured in Headers View (with 'a c')"
	   :visible (eq major-mode 'mu4e-compose-mode))
	(quote Attach\ External...)))

    ;; setup the fcc-stuff, if needed
    (add-hook 'message-send-hook
      (lambda () ;; mu4e~compose-save-before-sending
	;; when in-reply-to was removed, remove references as well.
	(when (eq mu4e~compose-type 'reply)
	  (mu4e~remove-refs-maybe))
	(when use-hard-newlines
	  (mu4e-send-harden-newlines))
	;; for safety, always save the draft before sending
	(set-buffer-modified-p t)
	(save-buffer)
	(mu4e~compose-setup-fcc-maybe)
	(widen)) nil t)
    ;; when the message has been sent.
    (add-hook 'message-sent-hook
      (lambda () ;;  mu4e~compose-mark-after-sending
	(setq mu4e-sent-func 'mu4e-sent-handler)
	(mu4e~proc-sent (buffer-file-name) mu4e~draft-drafts-folder)) nil t))
  ;; mark these two hooks as permanent-local, so they'll survive mode-changes
  ;;  (put 'mu4e~compose-save-before-sending 'permanent-local-hook t)
  (put 'mu4e~compose-mark-after-sending 'permanent-local-hook t))

(defun mu4e-send-harden-newlines ()
  "Set the hard property to all newlines."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(defconst mu4e~compose-buffer-max-name-length 30
  "Maximum length of the mu4e-send-buffer-name.")

(defvar mu4e~compose-type nil
  "Compose-type for this buffer.")

(defun mu4e~compose-set-friendly-buffer-name (&optional compose-type)
  "Set some user-friendly buffer name based on the compose type."
  (let* ((subj (message-field-value "subject"))
	  (subj (unless (and subj (string-match "^[:blank:]*$" subj)) subj))
	  (str (or subj
		 (case compose-type
		   (reply       "*reply*")
		   (forward     "*forward*")
		   (otherwise   "*draft*")))))
    (rename-buffer (generate-new-buffer-name
		     (truncate-string-to-width str
		       mu4e~compose-buffer-max-name-length
		       nil nil t)))))


(defun mu4e~compose-crypto-reply (parent compose-type)
  "When composing a reply to an encrypted message, we can
automatically encrypt that reply."
  (message "%S %S" parent compose-type)
  (when (and  (eq compose-type 'reply)
	  (and parent (member 'encrypted (mu4e-message-field parent :flags))))
	(case mu4e-compose-crypto-reply-policy
	  (sign (mml-secure-message-sign))
	  (encrypt (mml-secure-message-encrypt))
	  (sign-and-encrypt (mml-secure-message-sign-encrypt))
	  (message "Do nothing"))))



(defun* mu4e~compose-handler (compose-type &optional original-msg includes)
  "Create a new draft message, or open an existing one.

COMPOSE-TYPE determines the kind of message to compose and is a
symbol, either `reply', `forward', `edit', `resend' `new'. `edit'
is for editing existing (draft) messages. When COMPOSE-TYPE is
`reply' or `forward', MSG should be a message plist.  If
COMPOSE-TYPE is `new', ORIGINAL-MSG should be nil.

Optionally (when forwarding, replying) ORIGINAL-MSG is the original
message we will forward / reply to.

Optionally (when forwarding) INCLUDES contains a list of
   (:file-name <filename> :mime-type <mime-type> :disposition <disposition>)
for the attachements to include; file-name refers to
a file which our backend has conveniently saved for us (as a
tempfile)."

  ;; Run the hooks defined for `mu4e-compose-pre-hook'. If compose-type is
  ;; `reply', `forward' or `edit', `mu4e-compose-parent-message' points to the
  ;; message being forwarded or replied to, otherwise it is nil.
  (set (make-local-variable 'mu4e-compose-parent-message) original-msg)
  (put 'mu4e-compose-parent-message 'permanent-local t)
  ;; maybe switch the context
  (mu4e~context-autoswitch mu4e-compose-parent-message
    mu4e-compose-context-policy)
  (run-hooks 'mu4e-compose-pre-hook)

  ;; this opens (or re-opens) a messages with all the basic headers set.
  (let ((winconf (current-window-configuration)))
    (condition-case nil
      (mu4e-draft-open compose-type original-msg)
      (quit (set-window-configuration winconf)
	(mu4e-message "Operation aborted")
	(return-from mu4e~compose-handler))))
  ;; insert mail-header-separator, which is needed by message mode to separate
  ;; headers and body. will be removed before saving to disk
  (mu4e~draft-insert-mail-header-separator)
  ;; maybe encrypt/sign replies
  (mu4e~compose-crypto-reply original-msg compose-type)
  ;; include files -- e.g. when forwarding a message with attachments,
  ;; we take those from the original.
  (save-excursion
    (goto-char (point-max)) ;; put attachments at the end
    (dolist (att includes)
      (mml-attach-file
	(plist-get att :file-name) (plist-get att :mime-type))))
  ;; buffer is not user-modified yet
  (mu4e~compose-set-friendly-buffer-name compose-type)
  (set-buffer-modified-p nil)
  ;; now jump to some useful positions, and start writing that mail!

  (if (member compose-type '(new forward))
    (message-goto-to)
    (message-goto-body))
  ;; bind to `mu4e-compose-parent-message' of compose buffer
  (set (make-local-variable 'mu4e-compose-parent-message) original-msg)
  (put 'mu4e-compose-parent-message 'permanent-local t)
  ;; remember the compose-type
  (set (make-local-variable 'mu4e~compose-type) compose-type)
  (put 'mu4e~compose-type 'permanent-local t)

  ;; hide some headers
  (mu4e~compose-hide-headers)
  ;; switch on the mode
  (mu4e-compose-mode)
  (when mu4e-compose-in-new-frame
    ;; make sure to close the frame when we're done with the message these are
    ;; all buffer-local;
    (push 'delete-frame message-exit-actions)
    (push 'delete-frame message-postpone-actions)))

(defun mu4e-sent-handler (docid path)
  "Handler function, called with DOCID and PATH for the just-sent
message. For Forwarded ('Passed') and Replied messages, try to set
the appropriate flag at the message forwarded or replied-to."
  (mu4e~compose-set-parent-flag path)
  (when (file-exists-p path) ;; maybe the draft was not saved at all
    (mu4e~proc-remove docid))
  ;; kill any remaining buffers for the draft file, or they will hang around...
  ;; this seems a bit hamfisted...
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
	    (string= (buffer-file-name buf) path))
      (if message-kill-buffer-on-exit
	(kill-buffer buf))))
  ;; now, try to go back to some previous buffer, in the order
  ;; view->headers->main
  (if (buffer-live-p mu4e~view-buffer)
    (switch-to-buffer mu4e~view-buffer)
    (if (buffer-live-p mu4e~headers-buffer)
      (switch-to-buffer mu4e~headers-buffer)
      ;; if all else fails, back to the main view
      (when (fboundp 'mu4e) (mu4e))))
  (mu4e-message "Message sent"))

(defun mu4e-message-kill-buffer ()
  "Wrapper around `message-kill-buffer'.
It restores mu4e window layout after killing the compose-buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (message-kill-buffer)
    ;; Compose buffer killed
    (when (not (equal current-buffer (current-buffer)))
      ;; Restore mu4e
      (if mu4e-compose-in-new-frame
	(delete-frame)
	(if (buffer-live-p mu4e~view-buffer)
	  (switch-to-buffer mu4e~view-buffer)
	  (if (buffer-live-p mu4e~headers-buffer)
	    (switch-to-buffer mu4e~headers-buffer)
	    ;; if all else fails, back to the main view
	    (when (fboundp 'mu4e) (mu4e))))))))

(defun mu4e~compose-set-parent-flag (path)
  "Set the 'replied' \"R\" flag on messages we replied to, and the
'passed' \"F\" flag on message we have forwarded.

If a message has an 'in-reply-to' header, it is considered a reply
to the message with the corresponding message id. If it does not
have an 'in-reply-to' header, but does have a 'references' header,
it is considered to be a forward message for the message
corresponding with the /last/ message-id in the references header.

Now, if the message has been determined to be either a forwarded
message or a reply, we instruct the server to update that message
with resp. the 'P' (passed) flag for a forwarded message, or the
'R' flag for a replied message. The original messages are also
marked as Seen.

Function assumes that it's executed in the context of the message
buffer."
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
		  (setq forwarded-from (first refs))))))
	  ;; remove the <>
	  (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
	    (mu4e~proc-move (match-string 1 in-reply-to) nil "+R-N"))
	  (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
	    (mu4e~proc-move (match-string 1 forwarded-from) nil "+P-N")))))))

(defun mu4e-compose (compose-type)
  "Start composing a message of COMPOSE-TYPE, where COMPOSE-TYPE
is a symbol, one of `reply', `forward', `edit', `resend'
`new'. All but `new' take the message at point as input. Symbol
`edit' is only allowed for draft messages."
  (let ((msg (mu4e-message-at-point 'noerror)))
    ;; some sanity checks
    (unless (or msg (eq compose-type 'new))
      (mu4e-warn "No message at point"))
    (unless (member compose-type '(reply forward edit resend new))
      (mu4e-error "Invalid compose type '%S'" compose-type))
    (when (and (eq compose-type 'edit)
	    (not (member 'draft (mu4e-message-field msg :flags))))
      (mu4e-warn "Editing is only allowed for draft messages"))

    ;; 'new is special, since it takes no existing message as arg; therefore, we
    ;; don't need to involve the backend, and call the handler *directly*
    (if (eq compose-type 'new)
      (mu4e~compose-handler 'new)
      ;; otherwise, we need the doc-id
      (let* ((docid (mu4e-message-field msg :docid))
	      ;; decrypt (or not), based on `mu4e-decryption-policy'.
	      (decrypt
		(and (member 'encrypted (mu4e-message-field msg :flags))
		  (if (eq mu4e-decryption-policy 'ask)
		    (yes-or-no-p (mu4e-format "Decrypt message?"))
		    mu4e-decryption-policy))))
	;; if there's a visible view window, select that before starting composing
	;; a new message, so that one will be replaced by the compose window. The
	;; 10-or-so line headers buffer is not a good place to write it...
	(let ((viewwin (get-buffer-window mu4e~view-buffer)))
	  (when (window-live-p viewwin)
	    (select-window viewwin)))
	;; talk to the backend
	(mu4e~proc-compose compose-type decrypt docid)))))

(defun mu4e-compose-reply ()
  "Compose a reply for the message at point in the headers buffer."
  (interactive)
  (mu4e-compose 'reply))

(defun mu4e-compose-forward ()
  "Forward the message at point in the headers buffer."
  (interactive)
  (mu4e-compose 'forward))

(defun mu4e-compose-edit ()
  "Edit the draft message at point in the headers buffer.
This is only possible if the message at point is, in fact, a
draft message."
  (interactive)
  (mu4e-compose 'edit))

(defun mu4e-compose-resend ()
  "Resend the message at point in the headers buffer."
  (interactive)
  (mu4e-compose 'resend))

(defun mu4e-compose-new ()
  "Start writing a new message."
  (interactive)
  (mu4e-compose 'new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e-compose-func and mu4e-send-func are wrappers so we can set ourselves
;; as default emacs mailer (define-mail-user-agent etc.)

;;;###autoload
(defun mu4e~compose-mail (&optional to subject other-headers continue
			   switch-function yank-action send-actions return-action)
  "This is mu4e's implementation of `compose-mail'."

  ;; create a new draft message 'resetting' (as below) is not actually needed in
  ;; this case, but let's prepare for the re-edit case as well
  (mu4e~compose-handler 'new)

  (when (message-goto-to) ;; reset to-address, if needed
    (message-delete-line))
  (message-add-header (concat "To: " to "\n"))

  (when (message-goto-subject) ;; reset subject, if needed
    (message-delete-line))
  (message-add-header (concat "Subject: " subject "\n"))

  ;; add any other headers specified
  (when other-headers
    (message-add-header other-headers))

  ;; yank message
  (if (bufferp yank-action)
    (list 'insert-buffer yank-action)
    yank-action)

  ;; try to put the user at some reasonable spot...
  (if (not to)
    (message-goto-to)
    (if (not subject)
      (message-goto-subject)
      (message-goto-body))))

;; happily, we can re-use most things from message mode
;;;###autoload
(define-mail-user-agent 'mu4e-user-agent
  'mu4e~compose-mail
  'message-send-and-exit
  'message-kill-buffer
  'message-send-hook)
;; Without this `mail-user-agent' cannot be set to `mu4e-user-agent'
;; through customize, as the custom type expects a function.  Not
;; sure whether this function is actually ever used; if it is then
;; returning the symbol is probably the correct thing to do, as other
;; such functions suggest.
(defun mu4e-user-agent ()
  'mu4e-user-agent)

(defun mu4e~compose-browse-url-mail (url &optional ignored)
  "Adapter for `browse-url-mailto-function."
  (let* ((headers (rfc2368-parse-mailto-url url))
	  (to (cdr (assoc "To" headers)))
	  (subject (cdr (assoc "Subject" headers)))
	  (body (cdr (assoc "Body" headers))))
    (mu4e~compose-mail to subject)
    (if body
      (progn
	(message-goto-body)
	(insert body)
	(if (not to)
	  (message-goto-to)
	  (if (not subject)
	    (message-goto-subject)
	    (message-goto-body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-compose-goto-top ()
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to the
beginning of the buffer."
  (interactive)
  (let ((old-position (point)))
    (message-goto-body)
    (when (equal (point) old-position)
      (goto-char (point-min)))))

(define-key mu4e-compose-mode-map
  (vector 'remap 'beginning-of-buffer) 'mu4e-compose-goto-top)

(defun mu4e-compose-goto-bottom ()
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already there, go to the
end of the buffer."
  (interactive)
  (let ((old-position (point))
	 (message-position (save-excursion (message-goto-body) (point))))
    (goto-char (point-max))
    (when (re-search-backward message-signature-separator message-position t)
      (forward-line -1))
    (when (equal (point) old-position)
      (goto-char (point-max)))))

(define-key mu4e-compose-mode-map
  (vector 'remap 'end-of-buffer) 'mu4e-compose-goto-bottom)

(provide 'mu4e-compose)

;; Load mu4e completely even when this file was loaded through
;; autoload.
(require 'mu4e)
