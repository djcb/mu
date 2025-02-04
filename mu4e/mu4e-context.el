;;; mu4e-context.el --- Switching between settings -*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 Dirk-Jan C. Binnema

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

;; A mu4e 'context' is a set of variable-settings and functions, which can be
;; used e.g. to switch between accounts.

;;; Code:

(require 'mu4e-helpers)
(require 'mu4e-modeline)
(require 'mu4e-query-items)


;;; Configuration
(defcustom mu4e-context-policy 'ask-if-none
  "The policy to determine the context when entering the mu4e main view.

If the value is `always-ask', ask the user unconditionally.

In all other cases, if any context matches (using its match
function), this context is used. Otherwise, if none of the
contexts match, we have the following choices:

- `pick-first': pick the first of the contexts available (ie. the default)
- `ask': ask the user `ask-if-none': ask if there is no context yet,
   otherwise leave it as it is
- nil: return nil; leaves the current context as is.

Also see `mu4e-compose-context-policy'."
  :type '(choice
          (const :tag "Always ask what context to use, even if one matches"
                 always-ask)
          (const :tag "Ask if none of the contexts match" ask)
          (const :tag "Ask when there's no context yet" ask-if-none)
          (const :tag "Pick the first context if none match" pick-first)
          (const :tag "Don't change the context when none match" nil))
  :group 'mu4e)


(defvar mu4e-contexts nil
  "The list of `mu4e-context' objects describing mu4e's contexts.")

(defvar mu4e-context-changed-hook nil
  "Hook run just *after* the context changed.")

(defface mu4e-context-face
  '((t :inherit mu4e-title-face :weight bold))
  "Face for displaying the context in the modeline."
  :group 'mu4e-faces)

(defvar mu4e--context-current nil
  "The current context.
Internal; use `mu4e-context-switch' to change it.")

(defun mu4e-context-current (&optional output)
  "Get the currently active context, or nil if there is none.
When OUTPUT is non-nil, echo the name of the current context or
none."
  (interactive "p")
  (let ((ctx mu4e--context-current))
    (when output
      (mu4e-message "Current context: %s"
                    (if ctx (mu4e-context-name ctx) "<none>")))
    ctx))

(cl-defstruct mu4e-context
  "A mu4e context object with the following members:
- `name': the name of the context, eg. \"Work\" or \"Private\".
   When using mu4e's default completion, it uses the first letter of
   the name for this, so you should ensure those are different for
   all contexts.
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

(defun mu4e--context-ask-user (prompt)
  "Let user choose some context based on its name with PROMPT."
  (when mu4e-contexts
    (let* ((names (seq-map (lambda (context)
                             (cons (mu4e-context-name context) context))
                          mu4e-contexts))
           (context (mu4e-read-option prompt names)))
      (or context (mu4e-error "No such context")))))

(defun mu4e-context-switch (&optional force name)
  "Switch to a context with NAME.
Context must be part of `mu4e-contexts'; if NAME is nil, query user.

If the new context is the same as the current context, only
switch (run associated functions) when prefix argument FORCE is
non-nil."
  (interactive "P")
  (unless mu4e-contexts
    (mu4e-error "No contexts defined"))
  (let* ((names (seq-map (lambda (context)
                           (cons (mu4e-context-name context) context))
                        mu4e-contexts))
         (old-context mu4e--context-current) ; i.e., context before switch
         (context
          (if name
              (cdr-safe (assoc name names))
            (mu4e--context-ask-user "Switch to context: "))))
    (unless context (mu4e-error "No such context"))
    ;; if new context is same as old one, only switch with FORCE
    (when (or force (not (eq context (mu4e-context-current))))
      (when (and (mu4e-context-current)
                 (mu4e-context-leave-func mu4e--context-current))
        (funcall (mu4e-context-leave-func mu4e--context-current)))
      ;; enter the new context
      (when (mu4e-context-enter-func context)
        (funcall (mu4e-context-enter-func context)))
      (when (mu4e-context-vars context)
        (mapc (lambda (cell)
                (set (car cell) (cdr cell)))
              (mu4e-context-vars context)))
      (setq mu4e--context-current context)
      (run-hooks 'mu4e-context-changed-hook)
      ;; refresh the cached query items if there was a context before; we have
      ;; have different bookmarks/maildirs now.
      (when old-context
           (mu4e--query-items-refresh 'reset-baseline))
      (mu4e-message "Switched context to %s"
                    (mu4e-context-name context)))
    context))

(defun mu4e--context-autoswitch (&optional msg policy)
  "Automatically switch to some context.

When contexts are defined but there is no context yet, switch to
the first whose :match-func return non-nil. If none of them
match, return the first. For MSG and POLICY, see
`mu4e-context-determine'."
  (when mu4e-contexts
    (let ((context (mu4e-context-determine msg policy)))
      (when context (mu4e-context-switch
                     nil (mu4e-context-name context))))))

(defun mu4e-context-determine (msg &optional policy)
  "Return first context for which match-func returns non-nil.

MSG points to the plist for the message replied to or forwarded,
or nil if there is no such MSG; similar to what
`mu4e-compose-pre-hook' does.

POLICY specifies how to do the determination. If POLICY is
`always-ask', we ask the user unconditionally.

In all other cases, if any context matches (using its match
function), this context is returned. If none of the contexts
match, POLICY determines what to do:

- `pick-first': pick the first of the contexts available
- `ask': ask the user
- `ask-if-none': ask if there is no context yet
- otherwise, return nil. Effectively, this leaves the current context
as it is."
  (when mu4e-contexts
    (if (eq policy 'always-ask)
        (mu4e--context-ask-user "Select context: ")
      (or ;; is there a matching one?
       (seq-find (lambda (context)
                   (when (mu4e-context-match-func context)
                     (funcall (mu4e-context-match-func context) msg)))
                 mu4e-contexts)
       ;; no context found yet; consult policy
       (pcase policy
         ('pick-first (car mu4e-contexts))
         ('ask (mu4e--context-ask-user "Select context: "))
         ('ask-if-none (or (mu4e-context-current)
                          (mu4e--context-ask-user "Select context: ")))
         (_ nil))))))

(defmacro with-mu4e-context-vars (context &rest body)
  "Evaluate BODY, with variables let-bound for CONTEXT (if any).
`funcall'."
  (declare (indent 2))
  `(let* ((vars (and ,context (mu4e-context-vars ,context))))
     (cl-progv ;; XXX: perhaps use eval's lexical environment instead of progv?
         (mapcar (lambda(cell) (car cell)) vars)
         (mapcar (lambda(cell) (cdr cell)) vars)
       (eval ,@body))))

(defun mu4e--context-modeline-item ()
  "Propertized string with the current context or nil."
  (when-let* ((ctx (mu4e-context-current))
              (name (and ctx (mu4e-context-name ctx))))
    (concat
     "<"
     (propertize
      name
      'face 'mu4e-context-face
      'help-echo (format  "mu4e context: %s" name))
     ">")))

(define-minor-mode mu4e-context-minor-mode
  "Mode for switching the mu4e context."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  (mu4e--modeline-register #'mu4e--context-modeline-item))

(defvar mu4e--context-menu-items
  '("--"
    ["Switch-context" mu4e-context-switch
     :help "Switch the mu4e context"])
  "Easy menu items for mu4e-context.")

;;;
(provide 'mu4e-context)
;;; mu4e-context.el ends here
