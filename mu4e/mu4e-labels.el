;;; mu4e-labels.el --- Dealing with labels -*- lexical-binding: t -*-

;; Copyright (C) 2025 Dirk-Jan C. Binnema

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

;; In this file, add some helpers for dealing with message labels

;;; Code:
(require 'mu4e-message)
(require 'mu4e-server)
(require 'mu4e-helpers)

(defconst mu4e-label-regex
  "[^\"'+/\\`[:cntrl:][:blank:]-][^\"'/\\`[:cntrl:][:blank:]]*"
  ;;Emacs 30:
  ;;(rx-let ((taboo (any cntrl blank "\"'\\/`$"))
  ;;          (taboo1-extra (any "-+")))
  ;;   (rx (seq
  ;;        ;; First character: base forbidden + extra chars
  ;;        (not (or taboo taboo1-extra))
  ;;        ;; Rest: just base forbidden chars
  ;;        (zero-or-more (not taboo)))))
  "Unanchored regular expression matching a valid label.

Any character is allowed that is not a control-character, a
blank, or ASCII single/double quotes,backtick or
forward/backward slash; additionally, the first character cannot
be a \"+\" or \"-\", \"$\" either.")

(defun mu4e-label-validate (str)
  "Validate label STR.

If STR is a valid label, return STR. Otherwise, raise an warning.
This function attempts to be a bit more informative than simply
checking a regular expression.

See `mu4e-label-regex' for the definition of the valid format."
  (when (string-empty-p str) ;; i. must not be empty
    (mu4e-warn "invalid: empty string"))
  (let ((first (aref str 0)))
    ;; ii. must not start with + or -
    (when (or (char-equal first ?+) (char-equal first ?-))
      (mu4e-warn "invalid: starts with '%c'" first))
    ;; iii. check all characters a  valid:
    (seq-do (lambda (kar)
             (unless (string-match-p mu4e-label-regex
                                     (string ?_ kar))
               (mu4e-warn "invalid: character '%c'" kar)))
            str))
  str)

(defun mu4e-label-parse-expr (expr)
  "Parse a single delta expression EXPR.

If EXPR is non-empty, raises an error if EXPR is not a valid
delta. Otherwise, returns EXPR with extra whitespace removed.

If STR is empty, return nil."
  (let ((op (aref expr 0))
        (label (substring expr 1)))
    (unless (or (char-equal op ?+) (char-equal op ?-))
      (mu4e-warn "invalid: delta-expression must start with '+' or '-'"))
    (concat (char-to-string op)
            (mu4e-label-validate label))))

(defun mu4e-label-parse-exprs (exprs)
  "Parse a string EXPRS with deltas to a string.

If EXPRS is non-empty, raises an error if EXPRS is invalid.
Otherwise, return EXPRS with extra whitespace removed.

If EXPRS is empty, return nil."
  (mapconcat #'mu4e-label-parse-expr (split-string exprs) " "))

(defvar mu4e-labels-list nil "Cached list of labels.")

(defun mu4e--labels-completion-at-point ()
  "Provide completion when entering a label delta expressions."
  (cond
   ((not (looking-back "[^ \t]*" nil))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (or (car bounds) (point))
            (or (cdr bounds) (point)))))
   ((looking-back
     (rx (any "+" "-")
         (group
          (opt (regex mu4e-label-regex)))) nil)
    (list (match-beginning 1)
          (match-end 1)
          mu4e-labels-list))))

(defvar mu4e-minibuffer-label-expr-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "The keymap for reading label delta expression.")

(defun mu4e-labels-delta-read ()
  "Ask for a labels delta expression."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local completion-at-point-functions
                    #'mu4e--labels-completion-at-point)
        (use-local-map mu4e-minibuffer-label-expr-map))
    (mu4e-label-parse-exprs
     (read-string "Label delta expression: "))))

(defun mu4e--labels-update-server (docid expr)
  "Tell the server to update message with DOCID with EXPR.
EXPR is a label delta-expression, such as \"+foo -bar\".

Update the label cache while doing so."
  ;; update the cache
  (let ((expr (mu4e-label-parse-exprs expr))
        (labels ;; the list of labels without +/- prefix
         (seq-map (lambda (pmlabel)
                (substring pmlabel 1))
                  (split-string expr " "))))
    ;; don't care about dups etc.; the list
    ;; will be replaced by a fresh server-side one, after
    ;; update / restart
    (setq mu4e-labels-list
          (append labels mu4e-labels-list))
    ;; update the server
    (mu4e--server-label docid expr)))

(defun mu4e--labels-clear-server (docid)
  "Clear all labels from message with DOCID."
  ;; update the server
  ;; '-*' is not a valid label, but special-cased
  ;; on the server-side
  (mu4e--server-label docid "-*"))


(provide 'mu4e-labels)
;;; mu4e-labels.el ends here
