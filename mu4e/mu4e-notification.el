;;; mu4e-notification.el --- -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (C) 1996-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;;; Commentary:
;;; Generic support for showing new-mail notifications.

;;; Code:

(require 'mu4e-query-items)
(require 'mu4e-bookmarks)

;; for emacs' built-in desktop notifications to work, we need
;; dbus
(when (featurep 'dbus)
  (require 'notifications))

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

(defvar mu4e--notification-id nil
  "The last notification id, so we can replace it.")

(defun mu4e--default-notification-filter (&optional _)
  "Return t if a notification should be shown.

This default implementation does so when the number of unread
messages changed since the last notification and it is greater
than zero."
  (when-let* ((fav (mu4e-bookmark-favorite))
              (delta-unread (plist-get fav :delta-unread)))
    (when (and (> delta-unread 0)
               (not (= delta-unread mu4e--last-delta-unread)))
      (setq mu4e--last-delta-unread delta-unread) ;; update
      t ;; do show notification
      )))

(defun mu4e--default-notification-function (&optional _)
  "Default function for handling notifications.
The default implementation uses emacs' built-in dbus-notification
support."
  (when-let* ((fav (mu4e-bookmark-favorite))
              (title "mu4e found new mail")
              (delta-unread (or (plist-get fav :delta-unread) 0))
              (body (format "%d new message%s in %s"
                            delta-unread
                            (if (= delta-unread 1) "" "s")
                            (plist-get fav :name))))
    (cond
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
             :actions '("Show" "Favorite bookmark")
             :on-action (lambda (_ _) (mu4e-jump-to-favorite)))))
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
