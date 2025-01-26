;;; mu4e-compose.el --- Compose and send messages -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 Dirk-Jan C. Binnema

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

;; Implements mu4e-compose-mode, which is a `message-mode' derivative. There's
;; quite a bit of trickery involved to make the message-mode functions work in
;; this context; see mu4e-draft for details.


;;; Code:
(require 'message)
(require 'sendmail)
(require 'gnus-msg)
(require 'nnheader) ;; for make-full-mail-header

(require 'mu4e-obsolete)
(require 'mu4e-server)
(require 'mu4e-message)
(require 'mu4e-context)
(require 'mu4e-folders)

(require 'mu4e-draft)


;;; User configuration for compose-mode
(defgroup mu4e-compose nil
  "Customization for composing/sending messages."
  :group 'mu4e)

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

(defcustom mu4e-compose-post-hook
  (list
   ;; kill compose frames
   #'mu4e-compose-post-kill-frame
   ;; attempt to restore the old configuration.
   #'mu4e-compose-post-restore-window-configuration)
  "Hook run *after* message composition is over.

This is hook is run when closing the composition buffer, either
by sending, postponing, exiting or killing it.

This multiplexes the `message-mode' hooks `message-send-actions',
`message-postpone-actions', `message-exit-actions' and
`message-kill-actions', and the hook is run with a variable
`mu4e-message-post-action' set correspondingly to a symbol,
`send', `postpone', `exit' or `kill'."
  :type 'hook
  :group 'mu4e-compose)



(defvar mu4e-captured-message)
(defun mu4e-compose-attach-captured-message ()
  "Insert the last captured message file as an attachment.

Messages are expect to have been captured earlier with
`mu4e-action-capture-message'. Note: this is unrelated to
`org-mode' capturing."
  (interactive)
  (if-let* ((msg mu4e-captured-message)
         (path (plist-get msg :path))
         (path (and (file-exists-p path) path))
         (descr (or (plist-get msg :subject) ""))
         (descr
          (if (and (stringp descr) (not (string-empty-p descr)))
              descr "No subject")))
    (mml-attach-file path "message/rfc822" descr "attachment")
    (mu4e-warn "No valid message has been captured")))

;; Go to bottom / top

(defun mu4e-compose-goto-top (&optional arg)
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to
the beginning of the buffer.

Push mark at previous position, unless either a
\\[universal-argument] prefix ARG is supplied, or Transient Mark mode
is enabled and the mark is active."
  (interactive "P")
  (or arg
      (region-active-p)
      (push-mark))
  (let ((old-position (point)))
    (message-goto-body)
    (when (equal (point) old-position)
      (goto-char (point-min)))))

(defun mu4e-compose-goto-bottom (&optional arg)
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already
there, go to the end of the buffer.

Push mark at previous position, unless either a
\\[universal-argument] prefix ARG is supplied, or Transient Mark mode
is enabled and the mark is active."
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

(defun mu4e-compose-context-switch (&optional force name)
  "Change the context for the current draft message.

With NAME, switch to the context with NAME, and with FORCE non-nil,
switch even if the switch is to the same context.

Like `mu4e-context-switch' but with some changes after switching:
1. Update the From and Organization headers as per the new context
2. Update the `message-signature' as per the new context.

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
                                  (or (message-make-from) ""))
          ;; Update signature.
          (when (message-goto-signature) ;; delete old signature.
            (if message-signature-insert-empty-line
                (forward-line -2) (forward-line -1))
            (delete-region (point) (point-max)))
          (when message-signature
              (save-excursion (message-insert-signature))))))))


;;; Address completion

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

(defun mu4e-complete-contact ()
  "Attempt to complete the text at point with a contact.
I.e., either \"name <email>\" or \"email\". Return nil if not found.

This function can be used for `completion-at-point-functions', to
complete addresses. This can be used from outside mu4e, but mu4e
must be active (running) for this to work."
  (let* ((end (point))
         (start (save-excursion
                  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                  (goto-char (match-end 0))
                  (point))))
    (list start end #'mu4e--compose-complete-handler)))

(defun mu4e--compose-complete-contact-field ()
  "Attempt to complete a contact when in a contact field.

This is like `mu4e-compose-complete-contact', but limited to the
contact fields."
  (let ((mail-abbrev-mode-regexp
         "^\\(To\\|B?Cc\\|Reply-To\\|From\\|Sender\\):")
        (mail-header-separator mu4e--header-separator))
    (when (mail-abbrev-in-expansion-header-p)
      (mu4e-complete-contact))))

(defun mu4e--compose-setup-completion ()
  "Maybe enable auto-completion of addresses.
Do this when `mu4e-compose-complete-addresses' is non-nil.

When enabled, this attempts to put mu4e's completions at the
start of the buffer-local `completion-at-point-functions'. Other
completion functions still apply."
  (when mu4e-compose-complete-addresses
    (set (make-local-variable 'completion-ignore-case) t)
    (set (make-local-variable 'completion-cycle-threshold) 7)
    (add-to-list (make-local-variable 'completion-styles) 'substring)
    (add-hook 'completion-at-point-functions
              #'mu4e--compose-complete-contact-field -10 t)))

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
    (define-key map (kbd "C-S-u")    #'mu4e-update-mail-and-index)
    (define-key map (kbd "C-c C-u")  #'mu4e-update-mail-and-index)
    (define-key map (kbd "C-c ;")    #'mu4e-compose-context-switch)

    ;; emacs 29
    ;;(keymap-set map "<remap> <beginning-of-buffer>" #'mu4e-compose-goto-top)
    ;;(keymap-set map "<remap> <end-of-buffer>" #'mu4e-compose-goto-bottom)
    (define-key map (vector 'remap #'beginning-of-buffer)
                #'mu4e-compose-goto-top)
    (define-key map (vector 'remap #'end-of-buffer)
                #'mu4e-compose-goto-bottom)

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
    (mu4e--compose-setup-completion) ;; maybe offer address completion
    (if mu4e-compose-format-flowed   ;; format-flowed
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

(defun mu4e--compose-cite (msg)
  "Return a cited version of the ORIG message MSG (a string).
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

;;; Interactive functions

;;;###autoload
(defalias 'mu4e-compose-mail #'mu4e-compose-new)

;;;###autoload
(defun mu4e-compose-new (&optional to subject other-headers continue
                                   _switch-function yank-action send-actions
                                   return-action &rest _)
  "Mu4e's implementation of `compose-mail'.
TO, SUBJECT, OTHER-HEADERS, CONTINUE, YANK-ACTION SEND-ACTIONS
RETURN-ACTION are as described in `compose-mail', and to the
extend that they do not conflict with mu4e's inner workings.
SWITCH-FUNCTION is ignored."
  (interactive)
  (mu4e--draft
   'new
   (lambda () (mu4e--message-call
               #'message-mail to subject other-headers continue
               nil ;; switch-function -> we handle it ourselves.
               yank-action send-actions return-action))))

;;;###autoload
(defun mu4e-compose-reply-to (&optional to wide)
  "Reply to the message at point.
Optional TO can be the To: address for the message. If WIDE is
non-nil, make it a \"wide\" reply (a.k.a. \"reply-to-all\")."
  (interactive)
  (let ((parent (mu4e-message-at-point)))
    (mu4e--draft-with-parent
     'reply parent
     (lambda ()
       (with-current-buffer (mu4e--message-call #'message-reply to wide)
         (message-goto-body)
         (insert (mu4e--compose-cite parent))
         (current-buffer))))))

;;;###autoload
(defun mu4e-compose-reply (&optional wide)
  "Reply to the message at point.
If WIDE is non-nil, make it a \"wide\" reply (a.k.a.
\"reply-to-all\")."
  (interactive "P")
  (mu4e-compose-reply-to nil wide))

;;;###autoload
(defun mu4e-compose-wide-reply ()
  "Wide reply to the message at point.
I.e., \"reply-to-all\"."
  (interactive)
  (mu4e-compose-reply-to nil t))

;;;###autoload
(defun mu4e-compose-supersede ()
  "Supersede the message at point.

That is, send the message again, with all the same recipients;
this can be useful to follow-up on a sent message. The message
must originate from the current user, as determined through
`mu4e-personal-or-alternative-address-p'."
  (interactive)
  (let ((parent (mu4e-message-at-point)))
    (mu4e--draft-with-parent
     'reply ;; it's a special kind of reply.
     parent
     (lambda ()
       (with-current-buffer (mu4e--message-call #'message-supersede))))))

(defun mu4e-compose-forward ()
  "Forward the message at point.
To influence the way a message is forwarded, you can use the
variables ‘message-forward-as-mime’ and
‘message-forward-show-mml’."
  (interactive)
  (let ((parent (mu4e-message-at-point)))
    (mu4e--draft-with-parent
     'forward parent
     (lambda ()
       (setq
        message-reply-headers (make-full-mail-header
                               0
                               (or (message-field-value "Subject") "none")
                               (or (message-field-value "From") "nobody")
                               (message-field-value "Date")
                               (message-field-value "Message-Id" t)
                               (message-field-value "References")
                               0 0 ""))
       ;; a bit of a hack; mu4e--draft-with-parent will insert the decoded
       ;; version of the message, but that's not good enough for
       ;; message-forward, which needs the raw message instead; see #2662.
       (erase-buffer)
       (insert-file-contents-literally (mu4e-message-readable-path parent))
       (with-current-buffer (mu4e--message-call #'message-forward)
         (current-buffer))))))

;;;###autoload
(defun mu4e-compose-edit()
  "Edit an existing draft message."
  (interactive)
  (let* ((msg (mu4e-message-at-point)))
    (unless  (member 'draft (mu4e-message-field msg :flags))
      (mu4e-warn "Cannot edit non-draft messages"))
    (mu4e--draft
     'edit
     (lambda ()
       (with-current-buffer
           (find-file-noselect (mu4e-message-readable-path msg))
         (mu4e--delimit-headers)
         (current-buffer))))))

;;;###autoload
(defun mu4e-compose-resend (address)
  "Re-send the message at point to ADDRESS.
The message is resent as-is, without any editing. See
`message-resend' for details."
  (interactive
   (list (completing-read
          "Resend message to address: " mu4e--contacts-set)))
  (let ((msg (mu4e-message-at-point)))
    (with-temp-buffer
      (mu4e--prepare-draft msg)
      (insert-file-contents (mu4e-message-readable-path msg))
      (message-resend address))))

;;; Compose-mode

(declare-function mu4e "mu4e")

;;;###autoload
(define-mail-user-agent 'mu4e-user-agent
  #'mu4e-compose-mail
  #'message-send-and-exit
  #'message-kill-buffer
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
