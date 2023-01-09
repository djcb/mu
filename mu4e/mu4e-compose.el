;;; mu4e-compose.el -- part of mu4e -*- lexical-binding: t -*-

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
;;      if it's a forward, mark the forwarded message as "P", i.e.
;;      passed (forwarded)
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

;;; Code:

(require 'cl-lib)
(require 'message)
(require 'mail-parse)
(require 'smtpmail)

(require 'mu4e-server)
(require 'mu4e-actions)
(require 'mu4e-message)
(require 'mu4e-draft)
(require 'mu4e-context)
(require 'mu4e-window)

;;; Configuration
;; see mu4e-drafts.el

;;; Attachments
(defun mu4e-compose-attach-message (msg)
  "Insert message MSG as an attachment."
  (let ((path (plist-get msg :path)))
    (unless (file-exists-p path)
      (mu4e-warn "Message file not found"))
    (mml-attach-file
     path
     "message/rfc822"
     (or (plist-get msg :subject) "No subject")
     "attachment")))

(defun mu4e-compose-attach-captured-message ()
  "Insert the last captured message file as an attachment.
Messages are captured with `mu4e-action-capture-message'."
  (interactive)
  (unless mu4e-captured-message
    (mu4e-warn "No message has been captured"))
  (mu4e-compose-attach-message mu4e-captured-message))

;;; Misc

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
          (pcase sent-behavior
            ('delete nil)
            ('trash (mu4e-get-trash-folder mu4e-compose-parent-message))
            ('sent (mu4e-get-sent-folder mu4e-compose-parent-message))
            (_ (mu4e-error
                "Unsupported value %S for `mu4e-sent-messages-behavior'"
                mu4e-sent-messages-behavior))))
         (fccfile (and mdir
                       (concat (mu4e-root-maildir) mdir "/cur/"
                               (mu4e~draft-message-filename-construct "S")))))
    ;; if there's an fcc header, add it to the file
    (when fccfile
      (message-add-header (concat "Fcc: " fccfile "\n"))
      ;; sadly, we cannot define as 'buffer-local'...  this will screw up gnus
      ;; etc. if you run it after mu4e so, (hack hack) we reset it to the old
      ;; handler after we've done our thing.
      (setq message-fcc-handler-function
            (let ((maildir mdir)
                  (old-handler message-fcc-handler-function))
              (lambda (file)
                (setq message-fcc-handler-function old-handler)
                ;; reset the fcc handler
                (let ((mdir-path (concat (mu4e-root-maildir) maildir)))
                  ;; Create the full maildir structure for the sent folder if it
                  ;; doesn't exist. `mu4e--server-mkdir` runs asynchronously but
                  ;; no matter whether it runs before or after `write-file`, the
                  ;; sent maildir ends up in the correct state.
                  (unless (file-exists-p mdir-path)
                    (mu4e--server-mkdir mdir-path)))
                (write-file file) ;; writing maildirs files is easy
                (mu4e--server-add file))))))) ;; update the database

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
  "Just before saving, we remove the `mail-header-separator'.
Just after saving we restore it; thus, the separator should never
appear on disk. Also update the Date and ensure we have a
Message-ID."
  (add-hook 'before-save-hook
            #'mu4e~compose-before-save-hook-fn
            nil t)
  (add-hook 'after-save-hook
            #'mu4e~compose-after-save-hook-fn
            nil t))

(defvar-local mu4e~compose-undo nil
  "Remember the undo-state.")

(defun mu4e~compose-before-save-hook-fn ()
  "Add the message-id if necessary and update the date."
  (setq mu4e~compose-undo buffer-undo-list)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (unless (message-fetch-field "Message-ID")
        (message-generate-headers '(Message-ID)))
      (message-generate-headers '(Date)))
    (save-match-data
      (mu4e~draft-remove-mail-header-separator))))

(defun mu4e~compose-after-save-hook-fn ()
  (save-match-data
    (mu4e~compose-set-friendly-buffer-name)
    (mu4e~draft-insert-mail-header-separator)
    ;; hide some headers again
    (widen)
    (mu4e~compose-hide-headers)
    (set-buffer-modified-p nil)
    (mu4e-message "Saved (%d lines)" (count-lines (point-min) (point-max)))
    ;; update the file on disk -- ie., without the separator
    (mu4e--server-add (buffer-file-name)))
  (setq buffer-undo-list mu4e~compose-undo))


;;; address completion

;; inspired by org-contacts.el and
;; https://github.com/nordlow/elisp/blob/master/mine/completion-styles-cycle.el

(defun mu4e~compose-complete-handler (str pred action)
  "Complete address STR with predication PRED for ACTION."
  (cond
   ((eq action nil)
    (try-completion str mu4e--contacts-set pred))
   ((eq action t)
    (all-completions str mu4e--contacts-set pred))
   ((eq action 'metadata)
    ;; our contacts are already sorted - just need to tell the
    ;; completion machinery not to try to undo that...
    '(metadata
      (display-sort-function . identity)
      (cycle-sort-function   . identity)))))

(defun mu4e~compose-complete-contact (&optional start)
  "Complete the text at START with a contact.
Ie. either \"name <email>\" or \"email\")."
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
  "Remove References: if In-Reply-To: is missing.
This allows the user to effectively start a new message-thread by
removing the In-Reply-To header."
  (unless (message-fetch-field "in-reply-to")
    (message-remove-header "References")))

;;; Compose Mode

(defvar mu4e-compose-mode-map nil
  "Keymap for \"*mu4e-compose*\" buffers.")
(unless mu4e-compose-mode-map
  (setq mu4e-compose-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-S-u")   'mu4e-update-mail-and-index)
          (define-key map (kbd "C-c C-u") 'mu4e-update-mail-and-index)
          (define-key map (kbd "C-c C-k") 'mu4e-message-kill-buffer)
          (define-key map (kbd "C-c ;")   'mu4e-compose-context-switch)
          (define-key map (kbd "M-q")     'mu4e-fill-paragraph)
          map)))

(defun mu4e-fill-paragraph (&optional region)
  "Re-layout either the whole message or REGION.
If variable `use-hard-newlines', takes a multi-line paragraph and
makes it into a single line of text. Assume paragraphs are
separated by blank lines. If variable `use-hard-newlines' is not
set, this simply executes `fill-paragraph'."
  ;; Inspired by https://www.emacswiki.org/emacs/UnfillParagraph
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (ignore-errors
    (if mu4e-compose-format-flowed
        (let ((fill-column (point-max))
              (use-hard-newlines nil)); rfill "across" hard newlines
          (when (use-region-p)
            (delete-trailing-whitespace (region-beginning) (region-end)))
          (fill-paragraph nil region))
      (when (use-region-p)
        (delete-trailing-whitespace (region-beginning) (region-end)))
      (fill-paragraph nil region))))

(defun mu4e-toggle-use-hard-newlines ()
  (interactive)
  (setq use-hard-newlines (not use-hard-newlines))
  (if use-hard-newlines
      (turn-off-auto-fill)
    (turn-on-auto-fill)))

(defun mu4e~compose-remap-faces ()
  "Remap `message-mode' faces to mu4e ones.
Our parent `message-mode' uses font-locking for the compose
buffers; lets remap its faces so it uses the ones for mu4e."
  ;; normal headers
  (face-remap-add-relative 'message-header-name 'mu4e-header-field-face)
  (face-remap-add-relative 'message-header-other 'mu4e-header-value-face)
  ;; special headers
  (face-remap-add-relative 'message-header-from 'mu4e-contact-face)
  (face-remap-add-relative 'message-header-to 'mu4e-contact-face)
  (face-remap-add-relative 'message-header-cc 'mu4e-contact-face)
  (face-remap-add-relative 'message-header-bcc 'mu4e-contact-face)
  (face-remap-add-relative 'message-header-subject 'mu4e-special-header-value-face))

(define-derived-mode mu4e-compose-mode message-mode "mu4e:compose"
  "Major mode for the mu4e message composition, derived from `message-mode'.
\\{message-mode-map}."
  (progn
    (use-local-map mu4e-compose-mode-map)
    (mu4e-context-minor-mode)

    (set (make-local-variable 'message-signature) mu4e-compose-signature)
    ;; set this to allow mu4e to work when gnus-agent is unplugged in gnus
    (set (make-local-variable 'message-send-mail-real-function) nil)
    ;; Set to nil to enable `electric-quote-local-mode' to work:
    (make-local-variable 'comment-use-syntax)
    (setq comment-use-syntax nil)
    ;; message-mode has font-locking, but uses its own faces. Let's
    ;; use the mu4e-specific ones instead
    (mu4e~compose-remap-faces)
    (mu4e~compose-register-message-save-hooks)
    ;; offer completion for e-mail addresses
    (when mu4e-compose-complete-addresses
      (unless mu4e--contacts-set
        ;; work-around for https://github.com/djcb/mu/issues/1016
        (mu4e--request-contacts-maybe))
      (mu4e~compose-setup-completion))
    (if mu4e-compose-format-flowed
        (progn
          (turn-off-auto-fill)
          (setq truncate-lines nil
                word-wrap t
                mml-enable-flowed t
                use-hard-newlines t)
          (visual-line-mode t))
      (setq mml-enable-flowed nil))

    ;; set the attachment dir to something more reasonable than the draft
    ;; directory.
    (setq default-directory (mu4e~get-attachment-dir))

    (let ((keymap (lookup-key message-mode-map [menu-bar text])))
      (when keymap
        (define-key-after
          keymap
          [mu4e-hard-newlines]
          '(menu-item "Format=flowed" mu4e-toggle-use-hard-newlines
                      :button (:toggle . use-hard-newlines)
                      :help "Toggle format=flowed"
                      :visible (eq major-mode 'mu4e-compose-mode)
                      :enable mu4e-compose-format-flowed)
          'sep)

        (define-key-after
          keymap
          [mu4e-electric-quote-mode]
          '(menu-item "Electric quote" electric-quote-local-mode
                      :button (:toggle . electric-quote-mode)
                      :help "Toggle Electric quote mode"
                      :visible (and (eq major-mode 'mu4e-compose-mode)
                                    (functionp 'electric-quote-local-mode)))
          'mu4e-hard-newlines)))

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
              #'mu4e~setup-fcc-message-sent-hook-fn
               nil t)
    ;; when the message has been sent.
    (add-hook 'message-sent-hook
              #'mu4e~set-sent-handler-message-sent-hook-fn
              nil t))
  ;; mark these two hooks as permanent-local, so they'll survive mode-changes
  ;;  (put 'mu4e~compose-save-before-sending 'permanent-local-hook t)
  (put 'mu4e~compose-mark-after-sending 'permanent-local-hook t))

(defun mu4e~setup-fcc-message-sent-hook-fn ()
  ;; mu4e~compose-save-before-sending
  ;; when in-reply-to was removed, remove references as well.
  (when (eq mu4e-compose-type 'reply)
    (mu4e~remove-refs-maybe))
  (when use-hard-newlines
    (mu4e-send-harden-newlines))
  ;; for safety, always save the draft before sending
  (set-buffer-modified-p t)
  (save-buffer)
  (mu4e~compose-setup-fcc-maybe)
  (widen))

(defun mu4e~set-sent-handler-message-sent-hook-fn ()
  ;;  mu4e~compose-mark-after-sending
  (setq mu4e-sent-func 'mu4e-sent-handler)
  (mu4e--server-sent (buffer-file-name)))

(defun mu4e-send-harden-newlines ()
  "Set the hard property to all newlines."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(defconst mu4e~compose-buffer-max-name-length 30
  "Maximum length of the mu4e-send-buffer-name.")

(defun mu4e~compose-set-friendly-buffer-name (&optional compose-type)
  "Set some user-friendly buffer name based on the COMPOSE-TYPE."
  (let* ((subj (message-field-value "subject"))
         (subj (unless (and subj (string-match "^[:blank:]*$" subj)) subj))
         (str (or subj
                  (pcase compose-type
                    ('reply       "*mu4e-reply*")
                    ('forward     "*mu4e-forward*")
                    (_             "*mu4e-draft*")))))
    (rename-buffer (generate-new-buffer-name
                    (truncate-string-to-width
                     str mu4e~compose-buffer-max-name-length)
                    (buffer-name)))))

(defun mu4e-compose-crypto-message (parent compose-type)
  "Possibly encrypt or sign a message based on PARENT and COMPOSE-TYPE.
See `mu4e-compose-crypto-policy' for more details."
  (let* ((encrypted-p
          (and parent (memq 'encrypted (mu4e-message-field parent :flags))))
         (encrypt
          (or (memq 'encrypt-all-messages mu4e-compose-crypto-policy)
              ;; new messages
              (and (memq 'encrypt-new-messages mu4e-compose-crypto-policy)
                   (eq compose-type 'new))
              ;; forwarded messages
              (and (eq compose-type 'forward)
                   (memq 'encrypt-forwarded-messages mu4e-compose-crypto-policy))
              ;; edited messages
              (and (eq compose-type 'edit)
                   (memq 'encrypt-edited-messages mu4e-compose-crypto-policy))
              ;; all replies
              (and (eq compose-type 'reply)
                   (memq 'encrypt-all-replies mu4e-compose-crypto-policy))
              ;; plain replies
              (and (eq compose-type 'reply) (not encrypted-p)
                   (memq 'encrypt-plain-replies mu4e-compose-crypto-policy))
              ;; encrypted replies
              (and (eq compose-type 'reply) encrypted-p
                   (memq 'encrypt-encrypted-replies
                         mu4e-compose-crypto-policy))))
         (sign
          (or (memq 'sign-all-messages mu4e-compose-crypto-policy)
              ;; new messages
              (and (eq compose-type 'new)
                   (memq 'sign-new-messages mu4e-compose-crypto-policy))
              ;; forwarded messages
              (and (eq compose-type 'forward)
                   (memq 'sign-forwarded-messages mu4e-compose-crypto-policy))
              ;; edited messages
              (and (eq compose-type 'edit)
                   (memq 'sign-edited-messages mu4e-compose-crypto-policy))
              ;; all replies
              (and (eq compose-type 'reply)
                   (memq 'sign-all-replies mu4e-compose-crypto-policy))
              ;; plain replies
              (and (eq compose-type 'reply) (not encrypted-p)
                   (memq 'sign-plain-replies mu4e-compose-crypto-policy))
              ;; encrypted replies
              (and (eq compose-type 'reply) encrypted-p
                   (memq 'sign-encrypted-replies mu4e-compose-crypto-policy)))))
    (cond ((and sign encrypt)
           (mml-secure-message-sign-encrypt))
          (sign (mml-secure-message-sign))
          (encrypt (mml-secure-message-encrypt)))))

(cl-defun mu4e~compose-handler (compose-type &optional original-msg includes)
  "Create a new draft message, or open an existing one.

COMPOSE-TYPE determines the kind of message to compose and is a
symbol, either `reply', `forward', `edit', `resend' `new'. `edit'
is for editing existing (draft) messages. When COMPOSE-TYPE is
`reply' or `forward', MSG should be a message plist.  If
COMPOSE-TYPE is `new', ORIGINAL-MSG should be nil.

Optionally (when forwarding, replying) ORIGINAL-MSG is the original
message we will forward / reply to.

Optionally (when inline forwarding) INCLUDES contains a list of
   (:file-name <filename> :mime-type <mime-type>
    :description <description> :disposition <disposition>)
or
   (:buffer-name <filename> :mime-type <mime-type>
    :description <description> :disposition <disposition>)
for the attachments to include; file-name refers to
a file which our backend has conveniently saved for us (as a
tempfile).  The properties :mime-type, :description and :disposition
are optional."

  ;; Run the hooks defined for `mu4e-compose-pre-hook'. If compose-type is
  ;; `reply', `forward' or `edit', `mu4e-compose-parent-message' points to the
  ;; message being forwarded or replied to, otherwise it is nil.
  (set (make-local-variable 'mu4e-compose-parent-message) original-msg)
  (put 'mu4e-compose-parent-message 'permanent-local t)
  ;; remember the compose-type
  (set (make-local-variable 'mu4e-compose-type) compose-type)
  (put 'mu4e-compose-type 'permanent-local t)
  ;; maybe switch the context
  (mu4e--context-autoswitch mu4e-compose-parent-message
                            mu4e-compose-context-policy)
  (run-hooks 'mu4e-compose-pre-hook)
  ;; this opens (or re-opens) a message with all the basic headers set.
  (let ((draft-buffer))
    (let ((winconf (current-window-configuration)))
      (condition-case nil
          (setq draft-buffer (mu4e-draft-open compose-type original-msg))
        (quit (set-window-configuration winconf)
              (mu4e-message "Operation aborted")
              (cl-return-from mu4e~compose-handler))))
    (set-buffer draft-buffer)
    ;; insert mail-header-separator, which is needed by message mode to separate
    ;; headers and body. will be removed before saving to disk
    (mu4e~draft-insert-mail-header-separator)

    ;; maybe encrypt/sign replies
    (mu4e-compose-crypto-message original-msg compose-type)

    ;; include files -- e.g. when inline forwarding a message with
    ;; attachments, we take those from the original.
    (save-excursion
      (goto-char (point-max)) ;; put attachments at the end

      (if (and (eq compose-type 'forward) mu4e-compose-forward-as-attachment)
          (mu4e-compose-attach-message original-msg)
        (dolist (att includes)
          (let ((file-name (plist-get att :file-name))
                (mime (plist-get att :mime-type))
                (description (plist-get att :description))
                (disposition (plist-get att :disposition)))
            (if file-name
                (mml-attach-file file-name mime description disposition)
              (mml-attach-buffer (plist-get att :buffer-name)
                                 mime description disposition))))))

    (mu4e~compose-set-friendly-buffer-name compose-type)

    ;; bind to `mu4e-compose-parent-message' of compose buffer
    (set (make-local-variable 'mu4e-compose-parent-message) original-msg)
    (put 'mu4e-compose-parent-message 'permanent-local t)
    ;; set mu4e-compose-type once more for this buffer,
    (set (make-local-variable 'mu4e-compose-type) compose-type)
    (put 'mu4e-compose-type 'permanent-local t)

    ;; hide some headers
    (mu4e~compose-hide-headers)
    ;; switch on the mode
    (mu4e-compose-mode)

    ;; now jump to some useful positions, and start writing that mail!
    (if (member compose-type '(new forward))
        (message-goto-to)
      ;; otherwise, it depends...
      (pcase message-cite-reply-position
        ((or 'above 'traditional) (message-goto-body))
        (_ (when (message-goto-signature) (forward-line -2)))))

    ;; don't allow undoing anything before this.
    (setq buffer-undo-list nil)

    (when mu4e-compose-in-new-frame
      ;; make sure to close the frame when we're done with the message these are
      ;; all buffer-local;
      (push 'delete-frame message-exit-actions)
      (push 'delete-frame message-postpone-actions))

    ;; buffer is not user-modified yet
    (set-buffer-modified-p nil)
    (mu4e-display-buffer draft-buffer t)))

(defun mu4e~switch-back-to-mu4e-buffer ()
  "Try to go back to some previous buffer, in the order view->headers->main."
  (if (buffer-live-p (mu4e-get-view-buffer))
      (mu4e-display-buffer (mu4e-get-view-buffer) t)
    (if (buffer-live-p (mu4e-get-headers-buffer))
        (mu4e-display-buffer (mu4e-get-headers-buffer) t)
      ;; if all else fails, back to the main view
      (when (fboundp 'mu4e) (mu4e)))))

(defun mu4e-compose-context-switch (&optional force name)
  "Change the context for the current draft message.

With NAME, switch to the context with NAME, and with FORCE non-nil,
switch even if the switch is to the same context.

Like `mu4e-context-switch' but with some changes after switching:
1. Update the From and Organization headers as per the new context
2. Update the message-signature as per the new context.

Unlike some earlier version of this function, does _not_ update
the draft folder for the messages, as that would require changing
the file under our feet, which is a bit fragile."
  (interactive "P")

  (unless (derived-mode-p 'mu4e-compose-mode)
    (mu4e-error "Only available in mu4e compose buffers"))

  (let ((old-context (mu4e-context-current)))
    (unless (and name (not force) (eq old-context name))
      (unless (and (not force)
                   (eq old-context (mu4e-context-switch nil name)))
        (save-excursion
          ;; Change From / Organization if needed.
          (message-replace-header "Organization"
                                  (or (message-make-organization) "")
                                  '("Subject")) ;; keep in same place
          (message-replace-header "From"
                                  (or (mu4e~draft-from-construct) ""))
          ;; Update signature.
          (when (message-goto-signature) ;; delete old signature.
            (if message-signature-insert-empty-line
                (forward-line -2) (forward-line -1))
            (delete-region (point) (point-max)))
          (if (and mu4e-compose-signature-auto-include mu4e-compose-signature)
              (let ((message-signature mu4e-compose-signature))
                (save-excursion (message-insert-signature)))))))))

(defun mu4e-sent-handler (docid path)
  "Handler called with DOCID and PATH for the just-sent message.
For Forwarded ('Passed') and Replied messages, try to set the
appropriate flag at the message forwarded or replied-to."
  (mu4e~compose-set-parent-flag path)
  (when (file-exists-p path) ;; maybe the draft was not saved at all
    (mu4e--server-remove docid))
  ;; kill any remaining buffers for the draft file, or they will hang around...
  ;; this seems a bit hamfisted...
  (when message-kill-buffer-on-exit
    (dolist (buf (buffer-list))
      (and (buffer-file-name buf)
           (string= (buffer-file-name buf) path)
           (kill-buffer buf))))
  (mu4e~switch-back-to-mu4e-buffer)
  (mu4e-message "Message sent"))

(defun mu4e-message-kill-buffer ()
  "Wrapper around `message-kill-buffer'.
It restores mu4e window layout after killing the compose-buffer."
  (interactive)
  (let ((current-buffer (current-buffer))
        (win (selected-window)))
    (message-kill-buffer)
    (when (window-live-p win)
      (delete-window win))
    ;; Compose buffer killed
    (when (not (equal current-buffer (current-buffer)))
      ;; Restore mu4e
      (if mu4e-compose-in-new-frame
          (delete-frame)
        (mu4e~switch-back-to-mu4e-buffer)))))

(defun mu4e~compose-set-parent-flag (path)
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
          ;; remove the <>
          (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
            (mu4e--server-move (match-string 1 in-reply-to) nil "+R-N"))
          (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
            (mu4e--server-move (match-string 1 forwarded-from) nil "+P-N")))))))

(defun mu4e-compose (compose-type)
  "Start composing a message of COMPOSE-TYPE.
COMPOSE-TYPE is a symbol, one of `reply', `forward', `edit',
`resend' `new'. All but `new' take the message at point as input.
Symbol `edit' is only allowed for draft messages."
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
        ;; if there's a visible view window, select that before starting
        ;; composing a new message, so that one will be replaced by the compose
        ;; window. The 10-or-so line headers buffer is not a good place to write
        ;; it...
        ;; (mu4e-display-buffer (mu4e-get-view-buffer))
        ;; talk to the backend
        (mu4e--server-compose compose-type decrypt docid)))))

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


;;; Compose Mail
;; mu4e-compose-func and mu4e-send-func are wrappers so we can set ourselves
;; as default emacs mailer (define-mail-user-agent etc.)

(declare-function mu4e "mu4e")

;;;###autoload
(defun mu4e~compose-mail (&optional to subject other-headers _continue
                                    _switch-action yank-action _send-actions _return-action)
  "This is mu4e's implementation of `compose-mail'.
Quoting its docstring:

Start composing a mail message to send. This uses the user's
chosen mail composition package as selected with the variable
`mail-user-agent'. The optional arguments TO and SUBJECT specify
recipients and the initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, if non-nil, says to continue editing a message already
being composed.  Interactively, CONTINUE is the prefix argument.

YANK-ACTION, if non-nil, is an action to perform, if and when
necessary, to insert the raw text of the message being replied
to. It has the form (FUNCTION . ARGS). The user agent will apply
FUNCTION to ARGS, to insert the raw text of the original message.
\(The user agent will also run `mail-citation-hook', *after* the
original text has been inserted in this way.)

SEND-ACTIONS is a list of actions to call when the message is sent.
Each action has the form (FUNCTION . ARGS).

RETURN-ACTION, if non-nil, is an action for returning to the
caller.  It has the form (FUNCTION . ARGS).  The function is
called after the mail has been sent or put aside, and the mail
buffer buried."

  (unless (mu4e-running-p)
    (mu4e))

  ;; create a new draft message 'resetting' (as below) is not actually needed in
  ;; this case, but let's prepare for the re-edit case as well
  (mu4e~compose-handler 'new nil nil)

  (when (message-goto-to) ;; reset to-address, if needed
    (message-delete-line))
  (message-add-header (concat "To: " to "\n"))

  (when (message-goto-subject) ;; reset subject, if needed
    (message-delete-line))
  (message-add-header (concat "Subject: " subject "\n"))

  ;; add any other headers specified
  (seq-each (lambda(hdr)
              (let ((field (capitalize(car hdr))) (value (cdr hdr)))
                ;; fix in-reply without <>
                (when (and (string= field "In-Reply-To")
                           (string-match-p "\\`[^ @]+@[^ @]+\\'" value)
                           (not (string-match-p "\\`<.*>\\'" value)))
                  (setq value (concat "<" value ">")))
                (message-add-header (concat (capitalize field) ": " value "\n"))))
            other-headers)

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
  "Return the `mu4e-user-agent' symbol."
  'mu4e-user-agent)

;;; Go to bottom / top

(defun mu4e-compose-goto-top (&optional arg)
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to the
beginning of the buffer.

Push mark at previous position, unless either a \\[universal-argument] prefix
is supplied, or Transient Mark mode is enabled and the mark is active."
  (interactive "P")
  (or arg
      (region-active-p)
      (push-mark))
  (let ((old-position (point)))
    (message-goto-body)
    (when (equal (point) old-position)
      (goto-char (point-min)))))

(define-key mu4e-compose-mode-map
  (vector 'remap 'beginning-of-buffer) 'mu4e-compose-goto-top)

(defun mu4e-compose-goto-bottom (&optional arg)
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already there, go to the
end of the buffer.

Push mark at previous position, unless either a \\[universal-argument] prefix
is supplied, or Transient Mark mode is enabled and the mark is active."
  (interactive "P")
  (or arg
      (region-active-p)
      (push-mark))
  (let ((old-position (point))
        (message-position (save-excursion (message-goto-body) (point))))
    (goto-char (point-max))
    (when (re-search-backward message-signature-separator message-position t)
      (forward-line -1))
    (when (equal (point) old-position)
      (goto-char (point-max)))))

(define-key mu4e-compose-mode-map
  (vector 'remap 'end-of-buffer) 'mu4e-compose-goto-bottom)

(defvar mu4e--compose-menu-items
  '("--"
    ["Compose new" mu4e-compose-new
     :help "Compose new message"]
    ["Reply" mu4e-compose-reply
     :help "Reply to message"]
    ["Forward" mu4e-compose-forward
     :help "Forward message"])
  "Easy menu items for search.")

;;; _
(provide 'mu4e-compose)
;;; mu4e-compose.el ends here
