; mu4e-context.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2015-2016 Dirk-Jan C. Binnema

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

;; A mu4e 'context' is a a set of variable-settings and functions, which can be
;; used e.g. to switch between accounts.

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'mu4e-utils)

(defvar mu4e-contexts nil "The list of `mu4e-context' objects
describing mu4e's contexts.")

(defvar mu4e~context-current nil
  "The current context; for internal use. Use
  `mu4e-context-switch' to change it.")

(defun mu4e-context-current ()
  "Get the currently active context, or nil if there is none."
  mu4e~context-current)

(defun mu4e-context-label ()
  "Propertized string with the current context name, or \"\" if
  there is none."
  (if (mu4e-context-current)
    (concat "[" (propertize (mu4e~quote-for-modeline
			      (mu4e-context-name (mu4e-context-current)))
		  'face 'mu4e-context-face) "]") ""))

(defstruct mu4e-context
  "A mu4e context object with the following members:
- `name': the name of the context, eg. \"Work\" or \"Private\".'
- `enter-func': a parameterless function invoked when entering
  this context, or nil
- `leave-func':a parameterless fuction invoked when leaving this
  context, or nil
- `match-func': a function called when comnposing a new messages,
  and takes a message plist
for the message replied to or forwarded, and nil
otherwise. Before composing a new message, `mu4e' switches to the
first context for which `match-func' return t."
  name                      ;; name of the context, e.g. "work"
  (enter-func nil)          ;; function invoked when entering the context
  (leave-func nil)          ;; function invoked when leaving the context
  (match-func nil)          ;; function that takes a msg-proplist, and return t
  ;; if it matches, nil otherwise
  vars)                     ;; alist of variables.

(defun mu4e~context-ask-user (prompt)
  "Let user choose some context based on its name."
  (when mu4e-contexts
    (let* ((names (map 'list (lambda (context)
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
  (let* ((names (map 'list (lambda (context)
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
	(mapc #'(lambda (cell)
		  (set (car cell) (cdr cell)))
	  (mu4e-context-vars context)))
      (setq mu4e~context-current context)
      (mu4e~main-view-real nil nil)
      (mu4e-message "Switched context to %s" (mu4e-context-name context)))
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
	(find-if (lambda (context)
		     (when (mu4e-context-match-func context)
		       (funcall (mu4e-context-match-func context) msg)))
	  mu4e-contexts)
	;; no context found yet; consult policy
	(case policy
	  (pick-first (car mu4e-contexts))
	  (ask (mu4e~context-ask-user "Select context: "))
	  (ask-if-none (or (mu4e-context-current)
			 (mu4e~context-ask-user "Select context: ")))
	  (otherwise nil))))))

(provide 'mu4e-context)
 
