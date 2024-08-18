;;; mu4e-notification.el --- Mail notifications -*- lexical-binding: t-*-

;; Copyright (C) 2023-2024 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;;; Generic support for showing new-mail notifications.

;;; Code:

(require 'mu4e-query-items)
(require 'mu4e-bookmarks)

;; for Emacs' built-in desktop notifications to work, we need
;; DBus
(when (featurep 'dbus)
  (require 'notifications))

;;; Options

(defcustom mu4e-notification-filter #'mu4e--default-notification-filter
  "Function for determining if a notification is to be emitted.

If this is the case, the function should return non-nil.
The function must accept an optional single parameter, unused for
now."
  :type 'function
  :group 'mu4e-notification)

(defcustom mu4e-notification-function
  #'mu4e--default-notification-function
  "Function to emit a notification.

The function is invoked when we need to emit a new-mail
notification in some system-specific way. The function is invoked
when the query-items have been updated and
`mu4e-notification-filter' returns non-nil.

The function must accept an optional single parameter, unused for
now."
  :type 'function
  :group 'mu4e-notification)


;;; Implementation

(defvar mu4e--notification-id nil
  "The last notification id, so we can replace it.")

(defun mu4e--default-notification-filter (&optional _)
  "Return t if a notification should be shown.

This default implementation does so when the number of unread
messages changed since the last notification and is greater than
zero."
  (when-let* ((fav (mu4e-bookmark-favorite))
              (delta-unread (plist-get fav :delta-unread)))
    (when (and (> delta-unread 0)
               (not (= delta-unread mu4e--last-delta-unread)))
      (setq mu4e--last-delta-unread delta-unread) ;; update
      t ;; do show notification
      )))

(defun mu4e--default-notification-function (&optional _)
  "Default function for handling notifications.
The default implementation uses Emacs' built-in DBus-notification
support."
  (when-let* ((fav (mu4e-bookmark-favorite))
              (title "mu4e found new mail")
              (delta-unread (or (plist-get fav :delta-unread) 0))
              (body (format "%d new message%s in %s"
                            delta-unread
                            (if (= delta-unread 1) "" "s")
                            (plist-get fav :name))))
    (cond
     ((fboundp 'do-applescript)
      (do-applescript
       (format "display notification %S with title %S" body title)))
     ((fboundp 'notifications-notify)
      ;; notifications available
      (setq mu4e--notification-id
            (notifications-notify
             :title title
             :body body
             :app-name "mu4e@emacs"
             :replaces-id mu4e--notification-id
             ;; a custom mu4e icon would be nice...
             ;; :app-icon (ignore-errors
             ;;             (image-search-load-path
             ;;              "gnus/gnus.png"))
             :actions '("Show" "Favorite bookmark"
                        "default" "Favorite bookmark")
             :on-action (lambda (_1 _2) (mu4e-jump-to-favorite)))))
     ;; ... TBI: other notifications ...
     (t ;; last resort
      (mu4e-message "%s: %s" title body)))))

(defun mu4e--notification ()
  "Function called when the query items have been updated."
  (when (and  (funcall mu4e-notification-filter)
              (functionp mu4e-notification-function))
    (funcall mu4e-notification-function)))

(provide 'mu4e-notification)
;;; mu4e-notification.el ends here
