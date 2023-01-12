;;; mu4e-bookmarks.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2023 Dirk-Jan C. Binnema

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

;;; Code:
(require 'mu4e-helpers)
(require 'mu4e-server)
(require 'mu4e-modeline)


;;; Configuration

(defgroup mu4e-bookmarks nil
  "Settings for bookmarks."
  :group 'mu4e)

(defcustom mu4e-bookmarks
  '(( :name  "Unread messages"
             :query "flag:unread AND NOT flag:trashed"
             :key ?u)
    ( :name "Today's messages"
            :query "date:today..now"
            :key ?t)
    ( :name "Last 7 days"
            :query "date:7d..now"
            :hide-unread t
            :key ?w)
    ( :name "Messages with images"
            :query "mime:image/*"
            :key ?p))
  "List of pre-defined queries that are shown on the main screen.

Each of the list elements is a plist with at least:
`:name'  - the name of the query
`:query' - the query expression string (not a function)
`:key'   - the shortcut key (single character)

Note that the :query parameter can be a function/lambda.

Optionally, you can add the following:

- `:favorite' - if t, monitor the results of this query, and make
it eligible for showing its status in the emacs modeline. At most
one bookmark should have this set to t (otherwise the _first_
bookmark is the implicit favorite)
- `:hide' - if t, the bookmark is hidden from the main-view and
speedbar.
- `:hide-unread' - do not show the counts of
unread/total number of matches for the query in the main-view.
This can be useful if a bookmark uses a very slow query.

`:hide-unread' is implied from `:hide'.

Note: for efficiency, queries used to determine the unread/all
counts do not discard duplicate or unreadable messages. Thus, the
numbers shown may differ from the number you get from a normal
query."
  :type '(repeat (plist))
  :group 'mu4e-bookmarks)


(defun mu4e-ask-bookmark (prompt)
  "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query."
  (unless (mu4e-bookmarks) (mu4e-error "No bookmarks defined"))
  (let* ((prompt (mu4e-format "%s" prompt))
         (bmarks
          (mapconcat
           (lambda (bm)
             (concat
              "[" (propertize (make-string 1 (plist-get bm :key))
                              'face 'mu4e-highlight-face)
              "]"
              (plist-get bm :name))) (mu4e-bookmarks) ", "))
         (kar (read-char (concat prompt bmarks))))
    (mu4e-get-bookmark-query kar)))


(defun mu4e--bookmark-query (bm)
  "Get query string for some bookmark."
  (when bm
    (let* ((query (or (plist-get bm :query)
                      (mu4e-warn "No query in %S" bm)))
           ;; queries being functions is deprecated.
           (query (if (functionp query) (funcall query) query)))
      ;; earlier, we allowed for the queries being fucntions
      (unless (stringp query)
        (mu4e-warn "Could not get query string from %s" bm))
      ;; apparently, non-UTF8 queries exist, i.e.,
      ;; with maild dir names.
      (decode-coding-string query 'utf-8 t))))


(defun mu4e-get-bookmark-query (kar)
  "Get the corresponding bookmarked query for shortcut KAR.
Raise an error if none is found."
  (let ((chosen-bm
         (or (seq-find
              (lambda (bm)
                (= kar (plist-get bm :key)))
              (mu4e-bookmarks))
             (mu4e-warn "Unknown shortcut '%c'" kar))))
    (mu4e--bookmark-query chosen-bm)))

(defun mu4e-bookmark-define (query name key)
  "Define a bookmark for QUERY with NAME and shortcut KEY.
Append it to `mu4e-bookmarks'. Replaces any existing bookmark
with KEY."
  (setq mu4e-bookmarks
        (seq-remove
         (lambda (bm)
           (= (plist-get bm :key) key))
         (mu4e-bookmarks)))
  (cl-pushnew `(:name  ,name
                :query ,query
                :key   ,key)
              mu4e-bookmarks :test 'equal))

(defun mu4e-bookmarks ()
  "Get `mu4e-bookmarks' in the (new) format.
Convert from the old format if needed."
  (seq-map (lambda (item)
             (if (and (listp item) (= (length item) 3))
               `(:name  ,(nth 1 item) :query ,(nth 0 item)
                        :key   ,(nth 2 item))
               item))
           mu4e-bookmarks))

(defun mu4e-favorite-bookmark ()
  "Find the favorite bookmark.
The favorit bookmark is the first one that has a non-nil
':favorite' property, or the first if there is none."
  (let ((bookmarks (mu4e-bookmarks)))
    (or (seq-find (lambda (bm) (plist-get bm :favorite))
                  (mu4e-bookmarks))
        (car-safe bookmarks))))

;;; Last & baseline query results for bookmarks.

(defvar mu4e--baseline nil
  "Some previous version of the query-results.
This is used as the baseline to track updates by comparing it to
the latest query-results.")
(defvar mu4e--baseline-tstamp nil
  "Timestamp for when the query-results baseline was updated.")

(defun mu4e--reset-baseline ()
  (setq mu4e--baseline (mu4e-server-query-results)
        mu4e--baseline-tstamp (current-time))
  (mu4e-last-query-results 'refresh)) ; for side-effects


(defvar mu4e--last-query-results-cached nil)
(defun mu4e-last-query-results(&optional refresh)
  "Get the results (counts) of the latest queries.

Either read form the cache or update them when oudated or FORCE
is non-nil.

The queries are the bookmark / maildir queries that are used to
populate the read/unread counts in the main view and modeline.
They are refreshed when calling `(mu4e)', i.e., when going to the
main view.

When available, the baseline results are added as well.

The results are a list of elements of the form
   (:query \"query string\"
      :count  <total number matching count>
      :unread <number of unread messages in count>
      [:favorite t]
      :baseline ( ;; baseline results
         :count  <total number matching count>
         :unread <number of unread messages in count>)) The
baseline part is optional (see `mu4e-reset-query-results') for
more details).

Uses a cached string unless it is nil or REFRESH is non-nil."
  (or (and (not refresh) mu4e--last-query-results-cached)
      (setq mu4e--last-query-results-cached
            (let* ((favorite (mu4e-favorite-bookmark))
                   (favorite-query
                    (and favorite (mu4e--bookmark-query favorite))))
              ;; walk over the remembered queries
              ;; and augment them with the baseline data and ':favorite' flag, if
              ;; any.
              (seq-map
               (lambda (qres)
                 ;; note: queries can be _functions_ too; use their
                 ;; string value.
                 (let* ((query (mu4e--bookmark-query qres))
                        (bres (seq-find ;; find the corresponding baseline entry
                               (lambda (bq)
                                 (string= query (mu4e--bookmark-query bq)))
                               mu4e--baseline)))
                   (when (string= query (or favorite-query ""))
                     (plist-put qres :favorite t))
                   (when bres
                     (plist-put qres :baseline
                                `(:count ,(plist-get bres :count)
                                         :unread ,(plist-get bres :unread))))
                   qres))
               (mu4e-server-query-results))))))

(defun mu4e-last-query-result (query)
  "Get the last result for some QUERY or nil if not found.
See `mu4e-last-query-results' for the format."
  (seq-find
   (lambda (elm) (string= query (mu4e--bookmark-query elm)))
   (mu4e-last-query-results)))

;; for Zero-Inbox afficionados
(defvar mu4e-modeline-all-clear                   '("C:" . "🌀")
  "No more messages at all for this query.")
(defvar mu4e-modeline-all-read                    '("R:" . "✅")
  "No unread messages left.")
(defvar mu4e-modeline-unread-items                '("U:" . "📫")
  "There are some unread items.")
(defvar mu4e-modeline-new-items                   '("N:" . "🔥")
  "There are some new items after the baseline.
I.e., very new messages.")

(declare-function mu4e-search-bookmark "mu4e-search")
(defun mu4e-jump-to-favorite ()
  "Jump to to the favorite bookmark, if any."
  (interactive)
  (when-let ((fav (mu4e--bookmark-query (mu4e-favorite-bookmark))))
    (mu4e-search-bookmark fav)))

(defun mu4e--bookmarks-modeline-item ()
  "Modeline item showing message counts for the favorite bookmark.

This uses the one special ':favorite' bookmark, and if there is
one, creates a propertized string for display in the modeline."
  (when-let ((fav ;; any results for the favorite bookmark item?
              (seq-find (lambda (bm) (plist-get bm :favorite))
                        (mu4e-last-query-results))))
    (let* ((unread (plist-get   fav :unread))
           (count  (plist-get   fav :count))
           (baseline (plist-get fav :baseline))
           (baseline-unread
            (or (when baseline (plist-get baseline :unread)) unread))
           (delta (- unread baseline-unread)))
      (propertize
       (format "%s%s%s/%s "
               (funcall (if mu4e-use-fancy-chars 'cdr 'car)
                        (cond
                         ((> delta 0)  mu4e-modeline-new-items)
                         ((> unread 0) mu4e-modeline-unread-items)
                         ((> count 0) mu4e-modeline-all-read)
                         (t mu4e-modeline-all-clear)))
               (propertize (number-to-string unread) 'face 'mu4e-header-key-face)
               (if (<= delta 0) ""
                 (propertize (format "(%+d)" delta)
                             'face 'mu4e-unread-face))
               (number-to-string count))
       'help-echo (format "mu4e query: '%s'" (mu4e--bookmark-query fav))
       'mouse-face 'mode-line-highlight
       'keymap '(mode-line keymap
                           (mouse-1   . mu4e-jump-to-favorite)
                           (mouse-2   . mu4e-jump-to-favorite)
                           (mouse-3   . mu4e-jump-to-favorite))))))



(provide 'mu4e-bookmarks)
;;; mu4e-bookmarks.el ends here
