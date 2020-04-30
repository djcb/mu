;;; mu4e-context.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Dirk-Jan C. Binnema

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

;; A mu4e 'context' is a set of variable-settings and functions, which can be
;; used e.g. to switch between accounts.

;;; Code:

(require 'cl-lib)
(require 'mu4e-utils)

(defvar mu4e-move-to-trash-patterns)
(defvar smtpmail-smtp-user)
(defvar mu4e-view-date-format)

(defvar mu4e-contexts nil "The list of `mu4e-context' objects
describing mu4e's contexts.")

(defvar mu4e-context-changed-hook nil
  "Hook run just *after* the context changed.")

(defvar mu4e~context-current nil
  "The current context; for internal use. Use
  `mu4e-context-switch' to change it.")

(defun mu4e-context-current (&optional output)
  "Get the currently active context, or nil if there is none.
When OUTPUT is non-nil, echo the name of the current context or
none."
  (interactive "p")
  (let ((ctx mu4e~context-current))
    (when output
      (mu4e-message "Current context: %s"
                    (if ctx (mu4e-context-name ctx) "<none>")))
    ctx))

(defun mu4e-context-label ()
  "Propertized string with the current context name, or \"\" if
  there is none."
  (if (mu4e-context-current)
      (concat "[" (propertize (mu4e~quote-for-modeline
                               (mu4e-context-name (mu4e-context-current)))
                              'face 'mu4e-context-face) "]") ""))

(cl-defstruct mu4e-context
  "A mu4e context object with the following members:
- `name': the name of the context, eg. \"Work\" or \"Private\".
- `enter-func': a parameterless function invoked when entering
  this context, or nil
- `leave-func':a parameterless function invoked when leaving this
  context, or nil
- `match-func': a function called when composing a new message,
  that takes a message plist for the message replied to or
  forwarded, and nil otherwise. Before composing a new message,
  `mu4e' switches to the first context for which `match-func'
  returns t.
- `vars': variables to set when entering context."
  name                      ;; name of the context, e.g. "work"
  (enter-func nil)          ;; function invoked when entering the context
  (leave-func nil)          ;; function invoked when leaving the context
  (match-func nil)          ;; function that takes a msg-proplist, and return t
  ;; if it matches, nil otherwise
  vars) ;; alist of variables.

(defvar mu4e-no-trash-providers '("gmail.com" "googlemail.com")
  "List of email providers that don't support the trash flag.")

(cl-defun make-mu4e-context-account (&key name
                                          enter-func
                                          leave-func
                                          match-func
                                          vars
                                          ;; We set sane defaults for the following variables.  They will be added to
                                          ;; the context vars.
                                          (user-mail-address user-mail-address)
                                          (smtpmail-smtp-user smtpmail-smtp-user)
                                          ;; Folders:
                                          maildir
                                          (drafts-folder "drafts")
                                          (sent-folder "sent")
                                          (trash-folder "trash")
                                          (refile-folder "archive")
                                          ;; Trash fix.
                                          no-trash-flag
                                          ;; Rule for matching the context.
                                          predicate)
  "Create a context with sane defaults (see `make-mu4e-context').
Also:
- Add the context to the `mu4e-contexts'.
- Update the bookmarks to ignore the trash folder if NO-TRASH-FLAG is non-nil.

Options beyond those of `make-mu4e-context':
- `user-mail-address': Defaults to the global value when the context is created.
- `smtpmail-smtp-user': Defaults to the global value if non-nil when the context
  is created, or the context `user-mail-address' otherwise.
- `maildir': Mailbox folder name in as stored in `mu4e-maildir' (just the name,
  there must be no '/').  Defaults to `name'.
- `drafts-folder': Context value of `mu4e-drafts-folder'.  Defaults to
  \"drafts\".
- `sent-folder': Context value of `mu4e-sent-folder'.  Defaults to \"sent\".
- `trash-folder': Context value of `mu4e-trash-folder'.  Defaults to \"trash\".
- `refile-folder': Context value of `mu4e-refile-folder'.  Defaults to
  \"refile\".
- `no-trash-flag': If non-nil, the maildir will be added to
  `mu4e-move-to-trash-patterns' so that trashing moves the message instead of flagging.
- `predicate': A function that takes a message and returns non-nil if it matches
  the context.  This is only used if `match-func' is not provided, in which case
  the context is always matched against the message folder.

Example of a mailbox where only the sent-folder differs from the
default folders (see `make-mu4e-context' and `mu4e-context'):

  (let ((gandi-smtp-vars '((smtpmail-smtp-server . \"mail.gandi.net\")
                           (smtpmail-stream-type . starttls)
                           (smtpmail-smtp-service . 587))))
    (make-mu4e-context-account
     :name \"personal\"
     :user-mail-address \"john@doe.xyz\"
     :sent-folder \"Sent\"
     :vars gandi-smtp-vars)
    (make-mu4e-context-account
     :name \"work\"
     :user-mail-address \"john@work.org\"
     :sent-folder \"Sent\"
     :predicate (lambda (msg)
                  (mu4e-message-contact-field-matches
                   msg '(:from :to) \"boss@work.org\"))
     :vars gandi-smtp-vars))"
  (cl-assert name)
  (setq maildir (concat "/" (or maildir name) "/")
        smtpmail-smtp-user (or smtpmail-smtp-user user-mail-address)
        no-trash-flag (or no-trash-flag
                          (string-match (regexp-opt mu4e-no-trash-providers)
                                        user-mail-address)))
  (when no-trash-flag
    ;; Exclude trash folder from all bookmarks.  This is useful for mailboxes
    ;; which don't use the "trash" flag like Gmail.
    (dolist (bookmark mu4e-bookmarks)
      ;; TODO: mu4e-bookmark-query does not work here, why?
      (setf (car bookmark) (format "NOT maildir:\"%s\" and %s"
                                   mu4e-trash-folder
                                   (car bookmark))))
    ;; If this is a Gmail context, we add the maildir to the pattern list so
    ;; that they can be properly trashed.
    (add-to-list 'mu4e-move-to-trash-patterns (concat "^" maildir)))
  ;; TODO: Seems that mu4e fails to start when no default folder is set.
  ;; The following setq is a workaround.
  (setq mu4e-drafts-folder (concat maildir drafts-folder)
        mu4e-sent-folder (concat maildir sent-folder)
        mu4e-trash-folder (concat maildir trash-folder)
        mu4e-refile-folder (concat maildir refile-folder))
  (let ((context (make-mu4e-context :name name
                                    :enter-func enter-func
                                    :leave-func leave-func
                                    :match-func match-func
                                    :vars vars)))
    (unless (mu4e-context-match-func context)
      (setf (mu4e-context-match-func context)
            `(lambda (msg)
               (when msg
                 (or
                  ,(when predicate
                     `(funcall ,predicate msg))
                  (string-prefix-p ,maildir (mu4e-message-field msg :maildir)))))))
    (setf (mu4e-context-vars context)
          (append `((user-mail-address . ,user-mail-address)
                    (smtpmail-smtp-user . ,smtpmail-smtp-user)
                    (mu4e-drafts-folder . ,mu4e-drafts-folder)
                    (mu4e-sent-folder . ,mu4e-sent-folder)
                    (mu4e-trash-folder . ,mu4e-trash-folder)
                    (mu4e-refile-folder . ,mu4e-refile-folder))
                  (mu4e-context-vars context)))
    ;; Required when using multiple addresses and if we don't want to
    ;; reply to ourselves.
    (add-to-list 'mu4e-contexts context)
    context))

(defun mu4e~context-ask-user (prompt)
  "Let user choose some context based on its name."
  (when mu4e-contexts
    (let* ((names (cl-map 'list (lambda (context)
                                  (cons (mu4e-context-name context) context))
                          mu4e-contexts))
           (context (mu4e-read-option prompt names)))
      (or context (mu4e-error "No such context")))))

(defun mu4e-context-switch (&optional force name)
  "Switch context to a context with NAME which is part of
`mu4e-contexts'; if NAME is nil, query user.

If the new context is the same and the current context, only
switch (run associated functions) when prefix argument FORCE is
non-nil."
  (interactive "P")
  (unless mu4e-contexts
    (mu4e-error "No contexts defined"))
  (let* ((names (cl-map 'list (lambda (context)
                                (cons (mu4e-context-name context) context))
                        mu4e-contexts))
         (context
          (if name
              (cdr-safe (assoc name names))
            (mu4e~context-ask-user "Switch to context: "))))
    (unless context (mu4e-error "No such context"))
    ;; if new context is same as old one one switch with FORCE is set.
    (when (or force (not (eq context (mu4e-context-current))))
      (when (and (mu4e-context-current)
                 (mu4e-context-leave-func mu4e~context-current))
        (funcall (mu4e-context-leave-func mu4e~context-current)))
      ;; enter the new context
      (when (mu4e-context-enter-func context)
        (funcall (mu4e-context-enter-func context)))
      (when (mu4e-context-vars context)
        (mapc (lambda (cell)
                (set (car cell) (cdr cell)))
              (mu4e-context-vars context)))
      (setq mu4e~context-current context)

      (run-hooks 'mu4e-context-changed-hook)
      (mu4e-message "Switched context to %s" (mu4e-context-name context))
      (force-mode-line-update))
    context))

(defun mu4e~context-autoswitch (&optional msg policy)
  "When contexts are defined but there is no context yet, switch
to the first whose :match-func return non-nil. If none of them
match, return the first. For MSG and POLICY, see `mu4e-context-determine'."
  (when mu4e-contexts
    (let ((context (mu4e-context-determine msg policy)))
      (when context (mu4e-context-switch
                     nil (mu4e-context-name context))))))

(defun mu4e-context-determine (msg &optional policy)
  "Return the first context with a match-func that returns t. MSG
points to the plist for the message replied to or forwarded, or
nil if there is no such MSG; similar to what
`mu4e-compose-pre-hook' does.

POLICY specifies how to do the determination. If POLICY is
'always-ask, we ask the user unconditionally.

In all other cases, if any context matches (using its match
function), this context is returned. If none of the contexts
match, POLICY determines what to do:

- pick-first: pick the first of the contexts available
- ask: ask the user
- ask-if-none: ask if there is no context yet
- otherwise, return nil. Effectively, this leaves the current context as it is."
  (when mu4e-contexts
    (if (eq policy 'always-ask)
        (mu4e~context-ask-user "Select context: ")
      (or ;; is there a matching one?
       (cl-find-if (lambda (context)
                     (when (mu4e-context-match-func context)
                       (funcall (mu4e-context-match-func context) msg)))
                   mu4e-contexts)
       ;; no context found yet; consult policy
       (cl-case policy
         (pick-first (car mu4e-contexts))
         (ask (mu4e~context-ask-user "Select context: "))
         (ask-if-none (or (mu4e-context-current)
                          (mu4e~context-ask-user "Select context: ")))
         (otherwise nil))))))

(defun mu4e-context-in-modeline ()
  "Display the mu4e-context (if any) in a (buffer-specific)
global-mode-line."
  (add-to-list
   (make-local-variable 'global-mode-string)
   '(:eval (mu4e-context-label))))

;;; _
(provide 'mu4e-context)
;;; mu4e-context.el ends here
