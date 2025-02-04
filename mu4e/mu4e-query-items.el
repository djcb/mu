;;; mu4e-query-items.el --- Manage query results -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Dirk-Jan C. Binnema

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
;;
;; Managing the last query results / baseline, which we use to get the
;; unread-counts, i.e., query items. `mu4e-query-items` delivers these items,
;; aggregated from various sources.


;;; Code:

;;; Last & baseline query results for bookmarks.
(require 'cl-lib)
(require 'mu4e-helpers)
(require 'mu4e-server)

(defcustom mu4e-query-rewrite-function 'identity
  "Function to rewrite a query.

It takes a search expression string, and returns a possibly
  changed search expression string.

This function is applied on the search expression just before
searching, and allows users to modify the query.

For instance, we could change any instance of \"workmail\" into
\"maildir:/long-path-to-work-related-emails\", by setting the function

\\=(setq mu4e-query-rewrite-function
  (lambda(expr)
     (replace-regexp-in-string \"workmail\"
                   \"maildir:/long-path-to-work-related-emails\" expr)))

It is good to remember that the replacement does not understand
anything about the query, it just does text replacement.

A word of caution: the function should be deterministic and
always return the same result for a given query (at least within
some \"context\" (see `mu4e-context'). If not, you may get
incorrect results for the various unread counts."
  :type 'function
  :group 'mu4e-search)

(defcustom mu4e-hide-short-counts nil
  "Hide the short count of unread messages.
As used in `mu4e-ask-bookmark' and `mu4e-ask-folder'."
  :type 'boolean
  :group 'mu4e)

(defvar mu4e--query-items-baseline nil
  "Some previous version of the query-items.
This is used as the baseline to track updates by comparing it to
the latest query-items.")
(defvar mu4e--query-items-baseline-tstamp nil
  "Timestamp for when the query-items baseline was updated.")
(defvar mu4e--last-delta-unread 0 "Last notified number.")

(defun mu4e--bookmark-query (bm)
  "Get the query string for some bookmark BM."
  (when bm
    (let* ((query (or (plist-get bm :query)
                      (mu4e-warn "No query in %S" bm)))
           ;; queries being functions is deprecated, but for now we
           ;; still support it.
           (query (if (functionp query) (funcall query) query)))
      (unless (stringp query)
        (mu4e-warn "Could not get query string from %s" bm))
      ;; apparently, non-UTF8 queries exist, i.e.,
      ;; with maildir names.
      (decode-coding-string query 'utf-8 t))))

(defun mu4e--query-items-pick-favorite (items)
  "Pick the :favorite querty item.
If ITEMS does not yet have a favorite item, pick the first."
  (unless (seq-find
           (lambda (item) (plist-get item :favorite)) items)
    (plist-put (car items) :favorite t))
  items)

(defvar mu4e--bookmark-items-cached nil "Cached bookmarks query items.")
(defvar mu4e--maildir-items-cached nil "Cached maildirs query items.")

(declare-function mu4e-bookmarks "mu4e-bookmarks")
(declare-function mu4e-maildir-shortcuts "mu4e-folders")

(defun mu4e--query-item-display-counts (item)
  "Get the count display string for some query-data ITEM.
If the items has its `:hide-unread' at a non-nil value, return
an empty string."
  ;; purely for display, but we need it in the main menu, modeline
  ;; so let's keep it consistent.
  (cl-destructuring-bind (&key unread hide-unread delta-unread count
                               &allow-other-keys) item
    (if hide-unread
        ""
      (concat
       (propertize (number-to-string unread)
                   'face 'mu4e-header-key-face
                   'help-echo "Number of unread")
       (if (<= delta-unread 0) ""
         (propertize (format "(%+d)" delta-unread) 'face
                     'mu4e-unread-face))
       "/"
       (propertize (number-to-string count)
                   'help-echo "Total number")))))

(defun mu4e--query-item-display-short-counts (item)
  "Get the short count display string for some query-data ITEM.
This gets the delta if it is greater than zero. Otherwise, the
total unread count if is greater than zero. Otherwise, an empty
string.

If the items has its `:hide-unread' at a non-nil value, or if
`mu4e-hide-short-counts' is non-nil, returns an empty string."
  (cl-destructuring-bind (&key unread hide-unread delta-unread
                               &allow-other-keys) item
    (if (or hide-unread mu4e-hide-short-counts)
        ""
      (concat
       (if (> (or delta-unread 0) 0)
           (concat "(" (propertize (format "+%d" delta-unread) 'face 'mu4e-unread-face) ")")
         (if (> (or unread 0) 0)
             (concat "(" (propertize (format "%d" unread) 'face 'mu4e-header-key-face) ")")
           ""))))))

(defun mu4e--query-items-refresh (&optional reset-baseline)
  "Get the latest query data from the mu4e server.
With RESET-BASELINE, reset the baseline first."
  (when reset-baseline
    (setq mu4e--query-items-baseline nil
          mu4e--query-items-baseline-tstamp nil
          mu4e--bookmark-items-cached nil
          mu4e--maildir-items-cached nil
          mu4e--last-delta-unread 0))
  (mu4e--server-queries
   ;; note: we must apply the rewrite function here, since the query does not go
   ;; through mu4e-search.
   (mapcar (lambda (bm)
             (funcall mu4e-query-rewrite-function
                      (mu4e--bookmark-query bm)))
           (seq-filter (lambda (item)
                         (and (not (or (plist-get item :hide)
                                       (plist-get item :hide-unread)))))
                       (mu4e-query-items)))))

(defun mu4e--query-items-queries-handler (_sexp)
  "Handler for queries responses from the mu4e-server.
I.e. what we get in response to mu4e--query-items-refresh."
  ;; if we cleared the baseline (in mu4e--query-items-refresh)
  ;; set it to the latest now.
  (unless mu4e--query-items-baseline
    (setq mu4e--query-items-baseline (mu4e-server-query-items)
          mu4e--query-items-baseline-tstamp (current-time)))

  (setq mu4e--bookmark-items-cached nil
        mu4e--maildir-items-cached nil)
  (mu4e-query-items) ;; for side-effects
  ;; tell the world.
  (run-hooks 'mu4e-query-items-updated-hook))

;; this makes for O(n*m)... but with typically small(ish) n,m. Perhaps use a
;; hash for last-query-items and baseline-results?
(defun mu4e--query-find-item (query data)
  "Find the item in DATA for the given QUERY."
  (seq-find (lambda (item)
              (equal query (mu4e--bookmark-query item)))
            data))

(defun mu4e--make-query-items (data type)
  "Map the items in DATA to plists with aggregated query information.

DATA is either the bookmarks or maildirs (user-defined).

LAST-RESULTS-DATA contains unread/counts we received from the
server, while BASELINE-DATA contains the same but taken at some
earier time.

The TYPE denotes the category for the query item, a symbol
bookmark or maildir."
  (seq-map
   (lambda (item)
     (let* ((maildir (plist-get item :maildir))
            ;; for maildirs, construct the query
            (query (if (equal type 'maildirs)
                       (format "maildir:\"%s\"" maildir)
                     (plist-get item :query)))
            (query (if (functionp query) (funcall query) query))
            (name (plist-get item :name))
            ;; it is possible that the user has a rewrite function
            (effective-query (funcall mu4e-query-rewrite-function query))
            ;; maildir items may have an implicit name
            ;; which is the maildir value.
            (name (or name (and (equal type 'maildirs) maildir)))
            (last-results (mu4e-server-query-items))
            (baseline mu4e--query-items-baseline)
            ;; we use the _effective_ query to find the results,
            ;; since that's what the server will give to us.
            (baseline-item
             (mu4e--query-find-item effective-query baseline))
            (last-results-item
             (mu4e--query-find-item effective-query last-results))
            (count  (or (plist-get last-results-item :count) 0))
            (unread (or (plist-get last-results-item :unread) 0))
            (baseline-count  (or (plist-get baseline-item :count) count))
            (baseline-unread (or (plist-get baseline-item :unread) unread))
            (delta-unread (- unread baseline-unread))
            (value
             (list
              :name         name
              :query        query
              :key          (plist-get item :key)
              :count        count
              :unread       unread
              :delta-count  (- count baseline-count)
              :delta-unread delta-unread)))
       ;; remember the *effective* query too; we don't really need it, but
       ;; useful for debugging.
       (unless (string= query effective-query)
         (plist-put value :effective-query effective-query))
       ;;for matching maildir shortcuts
       (when maildir (plist-put value :maildir maildir))
       ;; nil props bring me discomfort
       (when (plist-get item :favorite)
         (plist-put value :favorite t))
       (when (plist-get item :hide)
         (plist-put value :hide t))
       (when (plist-get item :hide-unread)
         (plist-put value :hide-unread t))
       value))
   data))

(defun mu4e-query-items (&optional type)
  "Grab cached information about query items of some TYPE.

TYPE is a symbol; either `bookmarks' or `maildirs', or nil for
both, and returns a list of plists. The information is based on
the last (cached) information known by mu4e.

This combines:
- the latest queries data (i.e., `(mu4e-server-query-items)')
- baseline queries data (i.e. `mu4e-baseline') with the combined
  queries for `mu4e-bookmarks' and `mu4e-maildir-shortcuts' in
  bookmarks-compatible plists.

Currently, the plist contains the following fields:
- `:name'         - the name of the bookmark or query
- `:query'        - the associated (unprocessed) query
- `:count'        - number of matches for the query
- `:unread'       - number of unread messages for the query
- `:delta-count'  - change in count since baseline
- `:delta-unread' - change in unread count since baseline
- `:favorite'     - non-nil if this is the favorite query

There are some other fields for internal mu4e use, better not use
those externally.

For the various nuances with the unread count and baseline,
please refer to info node `(mu4e) Bookmarks and Maildirs'."
  (cond
   ((equal type 'bookmarks)
    (or mu4e--bookmark-items-cached
        (setq mu4e--bookmark-items-cached
              (mu4e--query-items-pick-favorite
               (mu4e--make-query-items (mu4e-bookmarks) 'bookmarks)))))
   ((equal type 'maildirs)
    (or mu4e--maildir-items-cached
        (setq mu4e--maildir-items-cached
              (mu4e--make-query-items (mu4e-maildir-shortcuts) 'maildirs))))
   ((not type)
    (append (mu4e-query-items 'bookmarks)
            (mu4e-query-items 'maildirs)))
   (t
    (mu4e-error "No such type %s" type))))

(provide 'mu4e-query-items)
;;; mu4e-query-items.el ends here
