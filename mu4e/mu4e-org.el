;;; mu4e-org -- Org-links to mu4e messages/queries -*- lexical-binding: t -*-
;; Copyright (C) 2012-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail
;; Version: 0.0

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of 1the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; The expect version here is org 8.x
(require 'org)

(defgroup mu4e-org nil
  "Settings for the org-mode related functionality in mu4e."
  :group 'mu4e
  :group 'org)

(defvar mu4e-org-link-query-in-headers-mode nil
  "Prefer linking to the query rather than to the message.
If non-nil, `org-store-link' in `mu4e-headers-mode' links to the
the current query; otherwise, it links to the message at point.")
(define-obsolete-variable-alias 'org-mu4e-link-query-in-headers-mode
  'mu4e-org-link-query-in-headers-mode "1.3.6")

(defcustom mu4e-org-link-desc-func
  (lambda (msg) (or (plist-get msg :subject) "No subject"))
  "Function that takes a msg and returns a description.
This can be use in org capture templates.

Example usage:

  (defun my-link-descr (msg)
    (let ((subject (or (plist-get msg :subject)
                       \"No subject\"))
          (date (or (format-time-string mu4e-headers-date-format
                    (mu4e-msg-field msg :date))
                    \"No date\")))
      (concat subject \" \" date)))

  (setq org-mu4e-link-desc-func 'my-link-descr)"
  :type 'function
  :group 'org-mu4e)
(define-obsolete-variable-alias 'org-mu4e-link-desc-func
  'mu4e-org-link-desc-func "1.3.6")

(defun mu4e~org-store-link-query ()
  "Store a link to a mu4e query."
  (let* ((query  (mu4e-last-query))
          (date (format-time-string (org-time-stamp-format)))
          ;; seems we get an error when there's no date...
          (link (concat "mu4e:query:" query)))
    (org-store-link-props
      :type "mu4e"
      :query query
      :date date)
    (org-add-link-props
      :link link
      :description (format "mu4e-query: '%s'" query))
    link))

(defun mu4e~org-first-address (msg field)
  "Get address field FIELD from MSG as a string or nil."
  (let* ((val (plist-get msg field))
          (name (when val (car (car val))))
          (addr (when val (cdr (car val)))))
    (when val
      (if name
        (format "%s <%s>" name addr)
        (format "%s" addr)))))

(defun mu4e~org-store-link-message ()
  "Store a link to a mu4e message."
  (let* ((msg      (mu4e-message-at-point))
          (msgid   (or (plist-get msg :message-id) "<none>"))
          (date    (plist-get msg :date))
          (date    (format-time-string (org-time-stamp-format) date))
          ;; seems we get an error when there's no date...
          (link    (concat "mu4e:msgid:" msgid)))
    (org-store-link-props
      :type        "mu4e"
      :message-id  msgid
      :to          (mu4e~org-first-address msg :to)
      :from        (mu4e~org-first-address msg :from)
      :date        date
      :subject     (plist-get msg :subject))
    (org-add-link-props
      :link link
      :description (funcall mu4e-org-link-desc-func msg))
    link))

(defun mu4e-org-store-link ()
  "Store a link to a mu4e message or query.
It links to the last known query when in `mu4e-headers-mode' with
`mu4e-org-link-query-in-headers-mode' set; otherwise it links to
a specific message, based on its message-id, so that links stay
valid even after moving the message around."
  (if (and (eq major-mode 'mu4e-headers-mode)
  mu4e-org-link-query-in-headers-mode)
    (mu4e~org-store-link-query)
    (when (mu4e-message-at-point t)
      (mu4e~org-store-link-message))))

;; org-add-link-type is obsolete as of org-mode 9. Instead we will use the
;; org-link-set-parameters method
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "mu4e"
           :follow #'mu4e-org-open
           :store  #'mu4e-org-store-link)
  (org-add-link-type "mu4e" 'mu4e-org-open)
  (add-hook 'org-store-link-functions 'mu4e-org-store-link))

(defun mu4e-org-open (link)
  "Open the org LINK.
Open the mu4e message (for links starting with 'msgid:') or run
the query (for links starting with 'query:')."
  (require 'mu4e)
  (cond
    ((string-match "^msgid:\\(.+\\)" link)
      (mu4e-view-message-with-message-id (match-string 1 link)))
    ((string-match "^query:\\(.+\\)" link)
      (mu4e-headers-search (match-string 1 link) current-prefix-arg))
    (t (mu4e-error "Unrecognized link type '%s'" link))))

(make-obsolete 'org-mu4e-open 'mu4e-org-open "1.3.6")

(defun mu4e-org-store-and-capture ()
  "Store a link to the current message or query.
\(depending on `mu4e-org-link-query-in-headers-mode', and capture
it with org)."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture))

(make-obsolete 'org-mu4e-store-and-capture 'org-mu4e-store-and-capture "1.3.6")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-org)
;;; mu4e-org.el ends here
