; mu4e-context.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2015 Dirk-Jan C. Binnema

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

;; A mu4e 'context' is a a set of variable-settings and hooks, which can be used e.g. to switch
;; between accounts.

(require 'cl)
(require 'mu4e-utils)

(defvar mu4e-contexts nil "The list of context objects.")

(defvar mu4e~context-current nil
  "The current context. Use `mu4e-context-switch' to change it.")

(defun mu4e-context-current ()
  "Get the currently active context, or nil if there is none."
  mu4e~context-current)

(defun mu4e-context-label ()
  "Propertized string with the current context name, or \"\" if
  there is none."
  (if (mu4e-context-current)
    (concat "[" (propertize (mu4e-context-name (mu4e-context-current))
		  'face 'mu4e-title-face) "]") ""))

(defstruct mu4e-context
  "A mu4e context object with the following members:
- `name': the name of the context, eg. \"Work\" or \"Private\".'
- `enter-func': a parameterless function invoked when entering this context, or nil
- `leave-func':a parameterless fuction invoked when leaving this context, or nil
- `match-func': a function called when comnposing a new messages, and takes a message plist
for the message replied to or forwarded, and nil otherwise. Before composing a new message,
`mu4e' switches to the first context for which `match-func' return t."
  name                      ;; name of the context, e.g. "work"
  (enter-func nil)          ;; function invoked when entering the context
  (leave-func nil)          ;; function invoked when leaving the context
  (match-func nil)          ;; function that takes a msg-proplist, and return t
  ;; if it matches, nil otherwise
  vars)                     ;; alist of variables.

(defun mu4e-context-switch (&optional name)
  "Switch context to a context with NAME which is part of
`mu4e-contexts'; if NAME is nil, query user."
  (interactive)
  (unless mu4e-contexts
    (mu4e-error "No contexts defined"))
  (let* ((names (map 'list (lambda (context)
			     (cons (mu4e-context-name context) context))
		  mu4e-contexts))
	  (context
	    (if name (cdr-safe  (assoc name names))
	      (mu4e-read-option "Switch to context: " names))))
    (unless context (mu4e-error "No such context"))
   
    ;; leave the current context
    (when (and mu4e~context-current (mu4e-context-leave-func mu4e~context-current))
      (funcall (mu4e-context-leave-func mu4e~context-current)))
    ;; enter the new context
    (when (mu4e-context-enter-func context)
      (funcall (mu4e-context-enter-func context)))
    (when (mu4e-context-vars context)
      (mapc #'(lambda (cell)
		(set (car cell) (cdr cell)))
	(mu4e-context-vars context)))
    (mu4e-clear-caches)
    (setq mu4e~context-current context)
    (mu4e-message "Switched context to %s" (mu4e-context-name context))
    context))

(defun mu4e-context-determine (msg)
  "Return the first context with a match-func that returns t. MSG
  points to the plist for the message replied to or forwarded, or
  nil if there is no such MSG; similar to what
  `mu4e-compose-pre-hook' does. If no context matches, return
  nil."
  (when mu4e-contexts
    (find-if (lambda (context)
	       (and (mu4e-context-match-func context)
		 (funcall (mu4e-context-match-func context) msg)))
      mu4e-contexts)))

(provide 'mu4e-context)
 
