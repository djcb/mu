;;; mu4e-search.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Dirk-Jan C. Binnema

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

;; Search-related functions and a minor-mode.

;;; Code:

(require 'seq)
(require 'mu4e-helpers)
(require 'mu4e-message)
(require 'mu4e-bookmarks)
(require 'mu4e-contacts)
(require 'mu4e-lists)
(require 'mu4e-mark)


;;; Configuration
(defgroup mu4e-search nil
  "Search-related settings."
  :group 'mu4e)

(define-obsolete-variable-alias 'mu4e-headers-results-limit
  'mu4e-search-results-limit "1.7.0")
(defcustom mu4e-search-results-limit 500
  "Maximum number of results to show.
This affects performance, especially when
`mu4e-summary-include-related' is non-nil.
Set to -1 for no limits."
  :type '(choice (const :tag "Unlimited" -1)
		 (integer :tag "Limit"))
  :group 'mu4e-search)

(define-obsolete-variable-alias 'mu4e-headers-full-search
  'mu4e-search-full "1.7.0")
(defvar mu4e-search-full nil
  "Whether to search for all results.
If this is nil, search for up to `mu4e-search-results-limit')")


(define-obsolete-variable-alias 'mu4e-headers-show-threads
  'mu4e-search-threads "1.7.0")
(defvar mu4e-search-threads t
  "Whether to calculate threads for the search results.")

(defcustom mu4e-query-rewrite-function 'identity
  "Function to rewrite a query.

It takes a search expression string, and returns a possibly
  changed search expression string.

This function is applied on the search expression just before
searching, and allows users to modify the query.

For instance, we could change and of workmail into
\"maildir:/long-path-to-work-related-emails\", by setting the function

(setq mu4e-query-rewrite-function
  (lambda(expr)
     (replace-regexp-in-string \"workmail\"
                   \"maildir:/long-path-to-work-related-emails\" expr)))

It is good to remember that the replacement does not understand
anything about the query, it just does text replacement."
  :type 'function
  :group 'mu4e-search)

(define-obsolete-variable-alias
  'mu4e-headers-search-bookmark-hook
  'mu4e-search-bookmark-hook "1.7.0")
(defcustom mu4e-search-bookmark-hook nil
  "Hook run just after invoking a bookmarked search.

This function receives the query as its parameter, before any
rewriting as per `mu4e-query-rewrite-function' has taken place.

The reason to use this instead of `mu4e-headers-search-hook' is
if you only want to execute a hook when a search is entered via a
bookmark, e.g. if you'd like to treat the bookmarks as a custom
folder and change the options for the search."
  :type 'hook
  :group 'mu4e-search)

(define-obsolete-variable-alias 'mu4e-headers-search-hook
  'mu4e-search-hook "1.7.0")
(defcustom mu4e-search-hook nil
  "Hook run just before executing a new search operation.
This function receives the query as its parameter, before any
rewriting as per `mu4e-query-rewrite-function' has taken place

This is a more general hook facility than the
`mu4e-search-bookmark-hook'. It gets called on every
executed search, not just those that are invoked via bookmarks,
but also manually invoked searches."
  :type 'hook
  :group 'mu4e-search)

;; Internals

;;; History
(defvar mu4e--search-query-past nil
  "Stack of queries before the present one.")
(defvar mu4e--search-query-future nil
  "Stack of queries after the present one.")
(defvar mu4e--search-query-stack-size 20
  "Maximum size for the query stacks.")
(defvar mu4e--search-last-query nil
  "The present (most recent) query.")



;;; Interactive functions
(declare-function mu4e--search-execute "mu4e-headers")

(defvar mu4e--search-view-target nil
  "Whether to automatically view (open) the target message.")
(defvar mu4e--search-msgid-target nil
  "Message-id to jump to after the search has finished.")


(defun mu4e-search (&optional expr prompt edit ignore-history msgid show)
  "Search for query EXPR.

Switch to the output buffer for the results. This is an
interactive function which ask user for EXPR. PROMPT, if non-nil,
is the prompt used by this function (default is \"Search for:\").
If EDIT is non-nil, instead of executing the query for EXPR, let
the user edit the query before executing it.

If IGNORE-HISTORY is true, do *not* update the query history
stack. If MSGID is non-nil, attempt to move point to the first
message with that message-id after searching. If SHOW is non-nil,
show the message with MSGID."
  (interactive)
  (let* ((prompt (mu4e-format (or prompt "Search for: ")))
	 (expr
	  (if (or (null expr) edit)
	      (mu4e-read-query prompt expr)
	    expr)))
    (mu4e-mark-handle-when-leaving)
    (mu4e--search-execute expr ignore-history)
    (setq mu4e--search-msgid-target msgid
    	  mu4e--search-view-target show)))

(define-obsolete-function-alias 'mu4e-headers-search 'mu4e-search "1.7.0")

(defun mu4e-search-edit ()
  "Edit the last search expression."
  (interactive)
  (mu4e-search mu4e--search-last-query nil t))

(define-obsolete-function-alias 'mu4e-headers-search-edit
  'mu4e-search-edit "1.7.0")

(defun mu4e-search-bookmark (&optional expr edit)
  "Search using some bookmarked query EXPR.
If EDIT is non-nil, let the user edit the bookmark before starting
the search."
  (interactive)
  (let ((expr
	 (or expr
	     (mu4e-ask-bookmark (if edit "Select bookmark: " "Bookmark: ")))))
    (run-hook-with-args 'mu4e-search-bookmark-hook expr)
    (mu4e-search expr (when edit "Edit bookmark: ") edit)))

(define-obsolete-function-alias 'mu4e-headers-search-bookmark
  'mu4e-search-bookmark "1.7.0")

(defun mu4e-search-bookmark-edit ()
  "Edit an existing bookmark before executing it."
  (interactive)
  (mu4e-search-bookmark nil t))

(define-obsolete-function-alias 'mu4e-headers-search-bookmark-edit
  'mu4e-search-bookmark-edit "1.7.0")

(defun mu4e-search-narrow(&optional filter)
  "Narrow the last search.
Do so by appending search expression FILTER to the last search
expression. Note that you can go back to the previous
query (effectively, \"widen\" it), with `mu4e-search-prev'."
  (interactive
   (let ((filter
	  (read-string (mu4e-format "Narrow down to: ")
		       nil 'mu4e~headers-search-hist nil t)))
     (list filter)))
  (unless mu4e--search-last-query
    (mu4e-warn "There's nothing to filter"))
  (mu4e-search (format "(%s) AND (%s)" mu4e--search-last-query filter)))

(define-obsolete-function-alias 'mu4e-headers-search-narrow
  'mu4e-search-narrow "1.7.0")

;; (defun mu4e-headers-change-sorting (&optional field dir)
;;   "Change the sorting/threading parameters.
;; FIELD is the field to sort by; DIR is a symbol: either 'ascending,
;; 'descending, 't (meaning: if FIELD is the same as the current
;; sortfield, change the sort-order) or nil (ask the user)."
;;   (interactive)
;;   (let* ((field
;;           (or field
;;               (mu4e-read-option "Sortfield: " mu4e~headers-sort-field-choices)))
;;          ;; note: 'sortable' is either a boolean (meaning: if non-nil, this is
;;          ;; sortable field), _or_ another field (meaning: sort by this other field).
;;          (sortable (plist-get (cdr (assoc field mu4e-header-info)) :sortable))
;;          ;; error check
;;          (sortable
;;           (if sortable
;;               sortable
;;             (mu4e-error "Not a sortable field")))
;;          (sortfield (if (booleanp sortable) field sortable))
;;          (dir
;;           (cl-case dir
;;             ((ascending descending) dir)
;;             ;; change the sort order if field = curfield
;;             (t
;;              (if (eq sortfield mu4e-headers-sort-field)
;;                  (if (eq mu4e-headers-sort-direction 'ascending)
;;                      'descending 'ascending)
;;                'descending))
;;             (mu4e-read-option "Direction: "
;;                               '(("ascending" . 'ascending) ("descending" . 'descending))))))
;;     (setq
;;      mu4e-headers-sort-field sortfield
;;      mu4e-headers-sort-direction dir)
;;     (mu4e-message "Sorting by %s (%s)"
;;                   (symbol-name sortfield)
;;                   (symbol-name mu4e-headers-sort-direction))
;;     (mu4e-headers-rerun-search)))

;; (defun mu4e~headers-toggle (name togglevar dont-refresh)
;;   "Toggle variable TOGGLEVAR for feature NAME. Unless DONT-REFRESH is non-nil,
;; re-run the last search."
;;   (set togglevar (not (symbol-value togglevar)))
;;   (mu4e-message "%s turned %s%s"
;;                 name
;;                 (if (symbol-value togglevar) "on" "off")
;;                 (if dont-refresh
;;                     " (press 'g' to refresh)" ""))
;;   (unless dont-refresh
;;     (mu4e-headers-rerun-search)))

;; (defun mu4e-headers-toggle-threading (&optional dont-refresh)
;;   "Toggle `mu4e-headers-show-threads'. With prefix-argument, do
;; _not_ refresh the last search with the new setting for threading."
;;   (interactive "P")
;;   (mu4e~headers-toggle "Threading" 'mu4e-headers-show-threads dont-refresh))

;; (defun mu4e-headers-toggle-full-search (&optional dont-refresh)
;;   "Toggle `mu4e-headers-full-search'. With prefix-argument, do
;; _not_ refresh the last search with the new setting for threading."
;;   (interactive "P")
;;   (mu4e~headers-toggle "Full-search"
;;                        'mu4e-headers-full-search dont-refresh))

;; (defun mu4e-headers-toggle-include-related (&optional dont-refresh)
;;   "Toggle `mu4e-headers-include-related'. With prefix-argument, do
;; _not_ refresh the last search with the new setting for threading."
;;   (interactive "P")
;;   (mu4e~headers-toggle "Include-related"
;;                        'mu4e-headers-include-related dont-refresh))

;; (defun mu4e-headers-toggle-skip-duplicates (&optional dont-refresh)
;;   "Toggle `mu4e-headers-skip-duplicates'. With prefix-argument, do
;; _not_ refresh the last search with the new setting for threading."
;;   (interactive "P")
;;   (mu4e~headers-toggle "Skip-duplicates"
;;                        'mu4e-headers-skip-duplicates dont-refresh))



(defun mu4e--search-push-query (query where)
  "Push QUERY to one of the query stacks.
WHERE is a symbol telling us where to push; it's a symbol, either
`future' or `past'. Also removes duplicates and truncates to
limit the stack size."
  (let ((stack
	 (pcase where
	   ('past   mu4e--search-query-past)
	   ('future mu4e--search-query-future))))
    ;; only add if not the same item
    (unless (and stack (string= (car stack) query))
      (push query stack)
      ;; limit the stack to `mu4e--search-query-stack-size' elements
      (when (> (length stack) mu4e--search-query-stack-size)
	(setq stack (cl-subseq stack 0 mu4e--search-query-stack-size)))
      ;; remove all duplicates of the new element
      (seq-remove (lambda (elm) (string= elm (car stack))) (cdr stack))
      ;; update the stacks
      (pcase where
	('past   (setq mu4e--search-query-past   stack))
	('future (setq mu4e--search-query-future stack))))))

(defun mu4e--search-pop-query (whence)
  "Pop a query from the stack.
WHENCE is a symbol telling us where to get it from, either `future'
or `past'."
  (pcase whence
    ('past
     (unless mu4e--search-query-past
       (mu4e-warn "No more previous queries"))
     (pop mu4e--search-query-past))
    ('future
     (unless mu4e--search-query-future
       (mu4e-warn "No more next queries"))
     (pop mu4e--search-query-future))))

(defun mu4e-search-rerun ()
  "Re-run the search for the last search expression."
  (interactive)
  ;; if possible, try to return to the same message
  (let* ((msg (mu4e-message-at-point t))
	 (msgid (and msg (mu4e-message-field msg :message-id))))
    (mu4e-search mu4e--search-last-query nil nil t msgid)))

(define-obsolete-function-alias 'mu4e-headers-rerun-search
  'mu4e-search-rerun "1.7.0")

(defun mu4e--search-query-navigate (whence)
  "Execute the previous query from the query stacks.
WHENCE determines where the query is taken from and is a symbol,
either `future' or `past'."
  (let ((query (mu4e--search-pop-query whence))
	(where (if (eq whence 'future) 'past 'future)))
    (when query
      (mu4e--search-push-query mu4e--search-last-query where)
      (mu4e-search query nil nil t))))

(defun mu4e-search-next ()
  "Execute the next query from the query stack."
  (interactive)
  (mu4e--search-query-navigate 'future))

(define-obsolete-function-alias 'mu4e-headers-query-next
  'mu4e-search-next "1.7.0")

(defun mu4e-search-prev ()
  "Execute the previous query from the query stacks."
  (interactive)
  (mu4e--search-query-navigate 'past))

(define-obsolete-function-alias 'mu4e-headers-query-prev
  'mu4e-search-prev "1.7.0")

;; forget the past so we don't repeat it :/
(defun mu4e-search-forget ()
  "Forget the search history."
  (interactive)
  (setq mu4e--search-query-past nil
	mu4e--search-query-future nil)
  (mu4e-message "Query history cleared"))

(define-obsolete-function-alias 'mu4e-headers-forget-queries
  'mu4e-search-forget "1.7.0")

(defun mu4e-last-query ()
  "Get the most recent query or nil if there is none."
  mu4e--search-last-query)

;;; Completion for queries

(defvar mu4e--search-hist nil "History list of searches.")
(defvar mu4e-minibuffer-search-query-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "The keymap for reading a search query.")

(defun mu4e-search-read-query (prompt &optional initial-input)
  "Read a query with completion using PROMPT and INITIAL-INPUT."
  (minibuffer-with-setup-hook
      (lambda ()
	(setq-local completion-at-point-functions
		    #'mu4e--search-query-completion-at-point)
	(use-local-map mu4e-minibuffer-search-query-map))
    (read-string prompt initial-input 'mu4e--search-hist)))

(define-obsolete-function-alias 'mu4e-read-query
  'mu4e-search-read-query "1.7.0")

(defconst mu4e--search-query-keywords
  '("and" "or" "not"
    "from:" "to:" "cc:" "bcc:" "contact:" "recip:" "date:" "subject:" "body:"
    "list:" "maildir:" "flag:" "mime:" "file:" "prio:" "tag:" "msgid:"
    "size:" "embed:"))

(defun mu4e--search-completion-contacts-action (match _status)
  "Delete contact alias from contact autocompletion, leaving just email address.
Implements the `completion-extra-properties' :exit-function' which
requires a function with arguments string MATCH and completion
status, STATUS."
  (let ((contact-email (replace-regexp-in-string "^.*<\\|>$" "" match)))
    (delete-char (- (length match)))
    (insert contact-email)))

(defun mu4e--search-query-completion-at-point ()
  "Provide completion when entering search expressions."
  (cond
   ((not (looking-back "[:\"][^ \t]*" nil))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (or (car bounds) (point))
	    (or (cdr bounds) (point))
	    mu4e--search-query-keywords)))
   ((looking-back "flag:\\(\\w*\\)" nil)
    (list (match-beginning 1)
	  (match-end 1)
	  '("attach" "draft" "flagged" "list" "new" "passed" "replied"
	    "seen" "trashed" "unread" "encrypted" "signed" "personal")))
   ((looking-back "maildir:\\([a-zA-Z0-9/.]*\\)" nil)
    (list (match-beginning 1)
          (match-end 1)
          (mu4e-get-maildirs)))
   ((looking-back "prio:\\(\\w*\\)" nil)
    (list (match-beginning 1)
	  (match-end 1)
	  (list "high" "normal" "low")))
   ((looking-back "mime:\\([a-zA-Z0-9/-]*\\)" nil)
    (list (match-beginning 1)
	  (match-end 1)
          (mailcap-mime-types)))
   ((looking-back "\\(from\\|to\\|cc\\|bcc\\|contact\\|recip\\):\\([a-zA-Z0-9/.@]*\\)" nil)
    (list (match-beginning 2)
          (match-end 2)
          mu4e--contacts-set
          :exit-function
          #'mu4e--search-completion-contacts-action))
   ((looking-back "list:\\([a-zA-Z0-9/.@]*\\)" nil)
    (list (match-beginning 1)
          (match-end 1)
          mu4e--lists-hash))))

(define-minor-mode mu4e-search-minor-mode
  "Mode for searching for messages."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'mu4e-search)
    (define-key map "S" 'mu4e-search-edit)
    (define-key map "/" 'mu4e-search-narrow)
    ;;(define-key map "j" 'mu4e~headers-jump-to-maildir)
    (define-key map (kbd "<M-left>")  'mu4e-search-prev)
    (define-key map (kbd "\\")        'mu4e-search-prev)
    (define-key map (kbd "<M-right>") 'mu4e-search-next)

    (define-key map "b" 'mu4e-search-bookmark)
    (define-key map "B" 'mu4e-search-bookmark-edit)
    map))

(provide 'mu4e-search)
;;; mu4e-search.el ends here
