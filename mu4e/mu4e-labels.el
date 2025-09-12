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
  "[^\"',+/\\`[:cntrl:][:blank:]-][^\"'$,/\\`[:cntrl:][:blank:]]+"
  "Unanchored regular expression matching a valid label.

Any character is allowed that is not a control-character, a
blank, or a number of special characters. Additionally, the first
character cannot be + or - either.")
;; sadly, the 'rx' macro is not expressive enough, pre-emacs30

(defun mu4e-label-validate (str)
  "Validate label STR.

If STR is a valid label, return STR. Otherwise, raise an warning.
This function attempts to be a bit more informative than simply
checking a regular expression.

See `mu4e-label-regex' for the definition of the valid format."
  (when (string-empty-p str) ;; i. must not be empty
    (mu4e-warn "an empty string is a not a valid label"))
  (let ((first (aref str 0))
        ;; anchored
        (valid-rx (rx bos (regex mu4e-label-regex) eos)))
    ;; ii. must not start with + or -
    (when (or (char-equal first ?+) (char-equal first ?-))
      (mu4e-warn "labels cannot starts with '%c'" first))
    ;; iii. match the regexp
    (unless (string-match-p valid-rx str)
      (mu4e-warn "not a valid label: %S" str)))
  str)

(defun mu4e-label-parse-delta-exprs (delta-exprs)
  "Parse a string a DELTA-EXPRS.

If empty, return nil. Otherwise, raise an error if it is invalid.
Otherwise, return a list with the invidual elements."
  (seq-map (lambda (delta-expr)
             (when (string-empty-p delta-expr)
               (mu4e-warn "delta-expression cannot be empty"))
             (let ((op (aref delta-expr 0))
                   (label (substring delta-expr 1)))
               (unless (or (char-equal op ?+) (char-equal op ?-))
                 (mu4e-warn (concat  "labels in delta-expression"
                                     " must start with +/- ('%s')")
                            delta-expr))
               (concat (char-to-string op) (mu4e-label-validate label))))
           (split-string delta-exprs "[,[:space:]]+" t)))

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
  "Ask for a label delta +/  expression.

I.e., a sequence of 1 or more space-separated labels, each
prefixed with \"+\" for addding the label, or \"-\" for removing
it."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local completion-at-point-functions
                    #'mu4e--labels-completion-at-point)
        (use-local-map mu4e-minibuffer-label-expr-map))
    (string-join
     (mu4e-label-parse-delta-exprs
      (read-string "Label delta (+/-) expression: "))
     " ")))

(defun mu4e--labels-update-server (docid expr)
  "Tell the server to update message with DOCID with EXPR.
EXPR is a label delta-expression, such as \"+foo -bar\".

Update the label cache while doing so."
  ;; update the cache
  (let ((deltas (mu4e-label-parse-delta-exprs expr)))
    ;; update cache
    (seq-do (lambda (delta-label)
              (cl-pushnew (substring delta-label 1) mu4e-labels-list))
            deltas)
    ;; maybe pass as list?
    (mu4e--server-label docid (string-join deltas " "))))

(defun mu4e--labels-clear-server (docid)
  "Clear all labels from message with DOCID."
  ;; update the server
  ;; '-*' is not a valid label, but special-cased
  ;; on the server-side
  (mu4e--server-label docid "-*"))


(provide 'mu4e-labels)
;;; mu4e-labels.el ends here
