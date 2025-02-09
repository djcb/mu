;;; mu4e-bookmarks.el --- Bookmarks handling -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Dirk-Jan C. Binnema

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
(require 'mu4e-modeline)
(require 'mu4e-folders)
(require 'mu4e-query-items)

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
`:query' - the query expression string or function
`:key'   - the shortcut key (single character)

Optionally, you can add the following:

- `:favorite' - if t, monitor the results of this query, and make
it eligible for showing its status in the modeline. At most
one bookmark should have this set to t (otherwise the _first_
bookmark is the implicit favorite). The query for the `:favorite'
item must be unique among `mu4e-bookmarks' and
`mu4e-maildir-shortcuts'.
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

(declare-function mu4e-query-items "mu4e-query-items")
(declare-function mu4e--query-item-display-short-counts "mu4e-query-items")

(defun mu4e-ask-bookmark (prompt)
  "Ask user for bookmark using PROMPT.
Return the corresponding query. The bookmark are as defined in
`mu4e-bookmarks'.

The names of the bookmarks are displayed in the minibuffer,
suffixed with the short version of the unread counts, as per
`mu4e--query-item-display-short-counts'."
  (unless (mu4e-bookmarks) (mu4e-error "No bookmarks defined"))
  (let* ((bmarks
          (seq-map
           (lambda (bm) ;; find query-item for bookmark
             (let* ((qitem (seq-find
                           (lambda (qitem)
                             (equal (plist-get bm :query) (plist-get qitem :query)))
                           (mu4e-query-items 'bookmarks)))
                    (unreads (mu4e--query-item-display-short-counts qitem)))
               (cons (format "%c%s%s"
                             (plist-get bm :key)
                             (plist-get bm :name)
                             unreads)
                     (plist-get bm :query))))
          (mu4e-filter-single-key (mu4e-bookmarks)))))
  (mu4e-read-option prompt bmarks)))

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

(defun mu4e-bookmark-favorite ()
  "Find the favorite bookmark."
  ;; note, use query-items, which will have picked a favorite
  ;; even if user did not provide one explictly
  (seq-find
   (lambda (item)
     (plist-get item :favorite))
   (mu4e-query-items 'bookmarks)))

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
  (when-let* ((fav (mu4e--bookmark-query (mu4e-bookmark-favorite))))
    (mu4e-search-bookmark fav)))

(defun mu4e--bookmarks-modeline-item ()
  "Modeline item showing message counts for the favorite bookmark.

This uses the one special ':favorite' bookmark, and if there is
one, creates a propertized string for display in the modeline."
  (when-let* ((fav ;; any results for the favorite bookmark item?
              (seq-find (lambda (bm) (plist-get bm :favorite))
                        (mu4e-query-items 'bookmarks))))
    (cl-destructuring-bind (&key unread count delta-unread
                                 &allow-other-keys) fav
      (propertize
       (format "%s%s "
               (funcall (if mu4e-use-fancy-chars 'cdr 'car)
                        (cond
                         ((> delta-unread 0)  mu4e-modeline-new-items)
                         ((> unread 0) mu4e-modeline-unread-items)
                         ((> count 0) mu4e-modeline-all-read)
                         (t mu4e-modeline-all-clear)))
               (mu4e--query-item-display-counts fav))
       'help-echo
       (format
        (concat
         "mu4e favorite bookmark '%s':\n"
         "\t%s\n\n"
         "number of matches: %d\n"
         "unread messages: %d\n"
         "changes since baseline: %+d\n")
        (plist-get fav :name)
        (mu4e--bookmark-query fav)
        count unread  delta-unread)
       'mouse-face 'mode-line-highlight
       'keymap '(mode-line keymap
                           (mouse-1 . mu4e-jump-to-favorite)
                           (mouse-2 . mu4e-jump-to-favorite)
                           (mouse-3 . mu4e-jump-to-favorite))))))
(provide 'mu4e-bookmarks)
;;; mu4e-bookmarks.el ends here
