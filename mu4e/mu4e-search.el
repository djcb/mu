;;; mu4e-search.el --- Search-related functions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Dirk-Jan C. Binnema

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
(require 'mu4e-query-items)


;;; Configuration
(defgroup mu4e-search nil
  "Search-related settings."
  :group 'mu4e)

(defcustom mu4e-search-results-limit 500
  "Maximum number of results to show.
This affects performance, especially when
`mu4e-summary-include-related' is non-nil.
Set to -1 for no limits."
  :type '(choice (const :tag "Unlimited" -1)
                 (integer :tag "Limit"))
  :group 'mu4e-search)

(defcustom mu4e-search-full nil
  "Whether to search for all results.
If this is nil, search for up to `mu4e-search-results-limit')"
  :type 'boolean
  :group 'mu4e-search)

(defcustom mu4e-search-threads t
  "Whether to calculate threads for the search results."
  :type 'boolean
  :group 'mu4e-search)

(defcustom mu4e-search-include-related t
  "Whether to include \"related\" messages in queries.
With this option set to non-nil, not just return the matches for
a searches, but also messages that are related (through their
references) to these messages. This can be useful e.g. to include
sent messages into message threads."
  :type 'boolean
  :group 'mu4e-search)

(defcustom mu4e-search-skip-duplicates t
  "Whether to skip duplicate messages.
With this option set to non-nil, show only one of duplicate
messages. This is useful when you have multiple copies of the same
message, which is a common occurrence for example when using Gmail
and offlineimap."
  :type 'boolean
  :group 'mu4e-search)

(defvar mu4e-search-hide-predicate nil
  "Predicate function to hide matching headers.
Either nil or a function taking one message plist parameter and
which which return non-nil for messages that should be hidden from
the search results. Also see `mu4e-search-hide-enabled'.

Example that hides all trashed messages:

  (setq mu4e-search-hide-predicate
     (lambda (msg)
       (member \\='trashed (mu4e-message-field msg :flags)))).")

(defvar mu4e-search-hide-enabled t
  "Whether `mu4e-search-hide-predicate' should be active.
This can be used to toggle use of the predicate through
 `mu4e-search-toggle-property'.")


(defcustom mu4e-search-sort-field :date
  "Field to sort the headers by. A symbol:
one of: `:date', `:subject', `:size', `:prio', `:from', `:to.',
`:list'.

Note that when threading is enabled (through
`mu4e-search-threads'), the headers are exclusively sorted
chronologically (`:date') by the newest message in the thread."
  :type '(radio (const :date)
                (const :subject)
                (const :size)
                (const :prio)
                (const :from)
                (const :to)
                (const :list))
  :group 'mu4e-search)

(defcustom mu4e-search-sort-direction 'descending
  "Direction to sort by.
A symbol either `descending' (sorting Z->A) or
`ascending' (sorting A->Z)."
  :type '(radio (const ascending)
                (const descending))
  :group 'mu4e-search)

;; mu4e-query-rewrite-function lives in mu4e-query-items.el
;; to avoid circular deps.

(defcustom mu4e-search-bookmark-hook nil
  "Hook run just after invoking a bookmarked search.

This function receives the query as its parameter, before any
rewriting as per `mu4e-query-rewrite-function' has taken place.

The reason to use this instead of `mu4e-search-hook' is
if you only want to execute a hook when a search is entered via a
bookmark, e.g. if you'd like to treat the bookmarks as a custom
folder and change the options for the search."
  :type 'hook
  :group 'mu4e-search)

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
(defvar mu4e--search-query-future nil  "Stack of queries after the present one.")
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

(defun mu4e--search-maybe-reset-baseline (query)
  "Reset the baseline if QUERY matches the favorite.
Note that the query must match the favorite _exactly_,
equivalence is not enough."
  (when-let* ((fav (mu4e--bookmark-query (mu4e-bookmark-favorite))))
    (when (and fav (string= fav query))
        (mu4e--query-items-refresh 'reset-baseline))))

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
show the message with MSGID.

Attempts to reset the query baseline if EXPR is an exact match
with the favorite bookmark's query."
  (interactive)
  (let* ((prompt (mu4e-format (or prompt "Search for: ")))
         (expr
          (if (or (null expr) edit)
              (mu4e-search-read-query prompt expr)
            expr)))
    (mu4e-mark-handle-when-leaving)
    (mu4e--search-execute expr ignore-history)
    (setq mu4e--search-msgid-target msgid
          mu4e--search-view-target show)
    (mu4e--search-maybe-reset-baseline expr)
    (mu4e--modeline-update)))

(defun mu4e-search-edit ()
  "Edit the last search expression."
  (interactive)
  (mu4e-search mu4e--search-last-query nil t))

(defun mu4e-search-bookmark (&optional expr edit)
  "Search using some bookmarked query EXPR.
If EDIT is non-nil, let the user edit the bookmark before starting
the search."
  (interactive)
  (let* ((expr
         (or expr
             (mu4e-ask-bookmark
              (if edit "Select bookmark: " "Bookmark: "))))
         (expr (if (functionp expr) (funcall expr) expr)))
    (run-hook-with-args 'mu4e-search-bookmark-hook expr)
    (mu4e-search expr (when edit "Edit query: ") edit)))

(defun mu4e-search-bookmark-edit ()
  "Edit an existing bookmark before executing it."
  (interactive)
  (mu4e-search-bookmark nil t))

(defun mu4e-search-maildir (maildir &optional edit)
  "Search the messages in MAILDIR.
The user is prompted to ask what maildir. If prefix-argument EDIT
is given, offer to edit the search query before executing it."
  (interactive
   (let ((maildir (mu4e-ask-maildir "Jump to maildir: ")))
     (list maildir current-prefix-arg)))
  (when maildir
    (let* ((query (format "maildir:\"%s\"" maildir))
           (query (if edit
                      (mu4e-search-read-query "Refine query: " query) query)))
      (mu4e-mark-handle-when-leaving)
      (mu4e-search query))))

(defun mu4e-search-narrow (&optional filter)
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

(defun mu4e-search-prev ()
  "Execute the previous query from the query stacks."
  (interactive)
  (mu4e--search-query-navigate 'past))

;; forget the past so we don't repeat it :/
(defun mu4e-search-forget ()
  "Forget the search history."
  (interactive)
  (setq mu4e--search-query-past nil
        mu4e--search-query-future nil)
  (mu4e-message "Query history cleared"))

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

(defconst mu4e--search-query-keywords
  '(;; logical
    "and"
    "or"
    "not"
    ;; fields
    "bcc:"
    "body:"
    "cc:"
    "changed:"
    "date:"
    "embed:"
    "file:"
    "flag:"
    "from:"
    "lang:"
    "maildir:"
    "list:"
    "msgid"
    "mime:"
    "path:"
    "prio:"
    "ref"
    "size:"
    "subject:"
    "tags:"
    "thread:"
    "to:"
    ;; combin fields
    "recip:"
    "contact:"
    "related:")
  "Mu4e query-keywords for completion.")

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
            "seen" "trashed" "unread" "encrypted" "signed" "personal"
            "calendar")))
   ((looking-back "maildir:\\([a-zA-Z0-9/.]*\\)" nil)
    (list (match-beginning 1)
          (match-end 1)
          (mapcar (lambda (dir)
                    ;; Quote maildirs with whitespace in their name, e.g.,
                    ;; maildir:"Foobar/Junk Mail".
                    (if (string-match-p "[[:space:]]" dir)
                        (concat "\"" dir "\"")
                      dir))
                  (mu4e-get-maildirs))))
   ((looking-back "prio:\\(\\w*\\)" nil)
    (list (match-beginning 1)
          (match-end 1)
          (list "high" "normal" "low")))
   ((looking-back "mime:\\([a-zA-Z0-9/-]*\\)" nil)
    (list (match-beginning 1)
          (match-end 1)
          (when (fboundp 'mailcap-mime-types) (mailcap-mime-types))))
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

;;; Interactive functions
(defun mu4e-search-change-sorting (&optional field dir)
  "Change the sorting/threading parameters.
FIELD is the field to sort by; DIR is a symbol: either
`ascending', `descending', t (meaning: if FIELD is the same as
the current sortfield, change the sort-order) or nil (ask the
user).

When threads are enabled (`mu4e-search-threads'), you can only sort
by the `:date' field."
  (interactive)
  (let* ((choices ;; with threads enabled, you can only sort by *date*
          (if mu4e-search-threads
              '(("date"    . :date))
            '(("date"    . :date)
              ("from"    . :from)
              ("list"    . :list)
              ("maildir" . :maildir)
              ("prio"    . :prio)
              ("zsize"   . :size)
              ("subject" . :subject)
              ("to"      . :to))))
         (field
          (or field
              (mu4e-read-option "Sortfield: " choices)))
         ;; note: 'sortable' is either a boolean (meaning: if non-nil, this is
         ;; sortable field), _or_ another field (meaning: sort by this other
         ;; field).
         (sortable (plist-get (cdr (assoc field mu4e-header-info)) :sortable))
         ;; error check
         (sortable
          (if sortable
              sortable
            (mu4e-error "Not a sortable field")))
         (sortfield (if (booleanp sortable) field sortable))
         (dir
          (cl-case dir
            ((ascending descending) dir)
            ;; change the sort order if field = curfield
            (t
             (if (eq sortfield mu4e-search-sort-field)
                 (if (eq mu4e-search-sort-direction 'ascending)
                     'descending 'ascending)
               'descending)))))
    (setq
     mu4e-search-sort-field sortfield
     mu4e-search-sort-direction dir)
    (mu4e-message "Sorting by %s (%s)"
                  (symbol-name sortfield)
                  (symbol-name mu4e-search-sort-direction))
    (mu4e-search-rerun)))

(defun mu4e-search-toggle-property (&optional dont-refresh)
  "Toggle some aspect of search.
When prefix-argument DONT-REFRESH is non-nil, do not refresh the
last search with the new setting."
  (interactive "P")
  (let* ((toggles '(("fFull-search"      . mu4e-search-full)
                    ("rInclude-related"  . mu4e-headers-include-related)
                    ("tShow threads"     . mu4e-search-threads)
                    ("uSkip duplicates"  . mu4e-search-skip-duplicates)
                    ("pHide-predicate"   . mu4e-search-hide-enabled)))
         (toggles (seq-map
                   (lambda (cell)
                     (cons
                      (concat (car cell)
                              (format" (%s)"
                                     (if (symbol-value (cdr cell)) "on" "off")))
                      (cdr cell)))
                   toggles))
         (choice (mu4e-read-option "Toggle property " toggles)))
    (when choice
      (set choice (not (symbol-value choice)))
      (mu4e-message "Set `%s' to %s" (symbol-name choice) (symbol-value choice))
      (mu4e--modeline-update)
      (unless dont-refresh
        (mu4e-search-rerun)))))

(defvar mu4e-search-threaded-label        '("T" . "Ⓣ")
  "Non-fancy and fancy labels to indicate threaded search in the mode-line.")
(defvar mu4e-search-full-label            '("F" . "Ⓕ")
  "Non-fancy and fancy labels to indicate full search in the mode-line.")
(defvar mu4e-search-related-label         '("R" . "Ⓡ")
  "Non-fancy and fancy labels to indicate related search in the mode-line.")
(defvar mu4e-search-skip-duplicates-label '("U" . "Ⓤ") ;; 'U' for 'unique'
  "Non-fancy and fancy labels for include-related search in the mode-line.")
(defvar mu4e-search-hide-label            '("H" . "Ⓗ")
  "Non-fancy and fancy labels to indicate header-hiding.")

(defun mu4e--search-modeline-item ()
  "Get mu4e-search modeline item."
  (let* ((label (lambda (label-cons)
                  (if mu4e-use-fancy-chars
                      (cdr label-cons) (car label-cons))))
         (props
          `((,mu4e-search-full ,mu4e-search-full-label
             "Full search")
            (,mu4e-search-include-related
             ,mu4e-search-related-label
             "Include related messages")
            (,mu4e-search-threads
             ,mu4e-search-threaded-label
             "Show message threads")
            (,mu4e-search-skip-duplicates
             ,mu4e-search-skip-duplicates-label
             "Skip duplicate messages")
            (,mu4e-search-hide-enabled
             ,mu4e-search-hide-label
             "Enable message hide predicate")))
         ;; can we fin find a bookmark corresponding
         ;; with this query?
         (bookmark
          (and mu4e-modeline-prefer-bookmark-name
               (seq-find (lambda (item)
                           (string=
                            mu4e--search-last-query
                            (or (plist-get item :effective-query)
                                (plist-get item :query))))
                         (mu4e-query-items 'bookmarks)))))
    (concat
     (propertize
      (mapconcat
       (lambda (cell)
         (when (nth 0 cell) (funcall label (nth 1 cell))))
       props "")
      'help-echo (concat "mu4e search properties legend\n\n"
                         (mapconcat
                          (lambda (cell)
                            (format "%s %s (%s)"
                                    (funcall label (nth 1 cell))
                                    (nth 2 cell)
                                    (if (nth 0 cell) "yes" : "no")))
                          props "\n")))
     " ["
     (propertize
      (if bookmark ;; show the bookmark name instead of the query?
          (plist-get bookmark :name)
          mu4e--search-last-query)
      'face 'mu4e-title-face
      'help-echo (format "mu4e query:\n\t%s" mu4e--search-last-query))
     "]")))

(defun mu4e-search-query (&optional edit)
  "Select a search query through `completing-read'.

If prefix-argument EDIT is non-nil, allow for editing the chosen
query before submitting it."
  (interactive "P")
  (let* ((candidates (seq-map (lambda (item)
                                (cons (plist-get item :name) item))
                              (mu4e-query-items)))
         (longest-name
          (seq-max (seq-map (lambda (c) (length (car c))) candidates)))
         (longest-query
          (seq-max (seq-map (lambda (c) (length (plist-get (cdr c) :query)))
                            candidates)))

         (annotation-func
          (lambda (candidate)
            (let* ((item (cdr-safe (assoc candidate candidates)))
                   (name (propertize (or  (plist-get item :name) "")
                                     'face 'mu4e-header-key-face))
                   (query (propertize (or (plist-get item :query) "")
                                      'face 'mu4e-header-value-face)))
              (concat
               "  "
               (make-string (- longest-name (length name)) ?\s)
               query
               (make-string (- longest-query (length query)) ?\s)
               "  "
               (mu4e--query-item-display-counts item)))))
         (completion-extra-properties
          `(:annotation-function ,annotation-func))
         (chosen (completing-read "Query: " candidates))
         (query (or (plist-get (cdr-safe (assoc chosen candidates)) :query)
                    (mu4e-warn "No query for %s" chosen))))
    (mu4e-search-bookmark query edit)))

(defvar mu4e-search-minor-mode-map
    (let ((map (make-sparse-keymap)))
    (define-key map "s" #'mu4e-search)
    (define-key map "S" #'mu4e-search-edit)
    (define-key map "/" #'mu4e-search-narrow)

    (define-key map (kbd "<M-left>")  #'mu4e-search-prev)
    (define-key map (kbd "\\")        #'mu4e-search-prev)
    (define-key map (kbd "<M-right>") #'mu4e-search-next)

    (define-key map "O" #'mu4e-search-change-sorting)
    (define-key map "P" #'mu4e-search-toggle-property)

    (define-key map "b" #'mu4e-search-bookmark)
    (define-key map "B" #'mu4e-search-bookmark-edit)

    (define-key map "c" #'mu4e-search-query)

    (define-key map "j" #'mu4e-search-maildir)
    map)
    "Keymap for mu4e-search-minor-mode.")

(define-minor-mode mu4e-search-minor-mode
  "Mode for searching for messages."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap mu4e-search-minor-mode-map)

(defvar mu4e--search-menu-items
  '("--"
    ["Search" mu4e-search
     :help "Search using expression"]
    ["Search bookmark" mu4e-search-bookmark
     :help "Show messages matching some bookmark query"]
    ["Search maildir" mu4e-search-maildir
     :help "Show messages in some maildir"]
    ["Choose query" mu4e-search-query
     :help "Show messages for some query"]
    ["Previous query" mu4e-search-prev
     :help "Run previous query"]
    ["Next query" mu4e-search-next
     :help "Run next query"]
    ["Narrow search" mu4e-search-narrow
     :help "Narrow the search query"]
    ["Search properties" mu4e-search-toggle-property
     :help "Toggle some search properties"])
  "Easy menu items for search.")

(provide 'mu4e-search)
;;; mu4e-search.el ends here
