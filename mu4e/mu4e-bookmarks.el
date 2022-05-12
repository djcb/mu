;;; mu4e-bookmarks.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2022 Dirk-Jan C. Binnema

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
`:query' - the query expression or function
`:key'   - the shortcut key.

Note that the :query parameter can be a function/lambda.

Optionally, you can add the following: `:hide' - if t, the
bookmark is hidden from the main-view and speedbar.
`:hide-unread' - do not show the counts of unread/total number of
matches for the query in the main-view. This can be useful if a
bookmark uses a very slow query.

`:hide-unread' is implied from `:hide'. Furthermore, it is
implied when `:query' is a function.

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

(defun mu4e-get-bookmark-query (kar)
  "Get the corresponding bookmarked query for shortcut KAR.
Raise an error if none is found."
  (let* ((chosen-bm
          (or (seq-find
               (lambda (bm)
                 (= kar (plist-get bm :key)))
               (mu4e-bookmarks))
              (mu4e-warn "Unknown shortcut '%c'" kar)))
         (expr (plist-get chosen-bm :query))
         (expr (if (not (functionp expr)) expr
                 (funcall expr)))
         (query (eval expr)))
    (if (stringp query)
        query
      (mu4e-warn "Expression must evaluate to query string ('%S')" expr))))


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
               item)) mu4e-bookmarks))

(provide 'mu4e-bookmarks)
;;; mu4e-bookmarks.el ends here
