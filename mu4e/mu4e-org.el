;;; mu4e-org -- Org-links to mu4e messages/queries -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of 1the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The expect version here is org 9.x.

;;; Code:

(require 'org)
(require 'mu4e-view)
(require 'mu4e-utils)

(defgroup mu4e-org nil
  "Settings for the org-mode related functionality in mu4e."
  :group 'mu4e
  :group 'org)

(defvar mu4e-org-link-query-in-headers-mode nil
  "Prefer linking to the query rather than to the message.
If non-nil, `org-store-link' in `mu4e-headers-mode' links to the
the current query; otherwise, it links to the message at point.")

(defun mu4e~org-store-link-query ()
  "Store a link to a mu4e query."
  (setq org-store-link-plist nil) ; reset
  (org-store-link-props
   :type        "mu4e"
   :query       (mu4e-last-query)
   :date        (format-time-string "%FT%T") ;; avoid error
   :link        (concat "mu4e:query:" (mu4e-last-query))
   :description (format "[%s]" (mu4e-last-query))))

(defun mu4e~org-address (cell)
  "Get address field FIELD from MSG as a string or nil."
  (let ((name (car cell)) (addr (cdr cell)))
    (if name
        (format "%s <%s>" name addr)
      (format "%s" addr))))

(defun mu4e~org-store-link-message ()
  "Store a link to a mu4e message."
  (setq org-store-link-plist nil)
  (let* ((msg      (mu4e-message-at-point))
         (from     (car-safe (plist-get msg :from)))
         (to       (car-safe (plist-get msg :to)))
         (date     (format-time-string "%FT%T" (plist-get msg :date)))
         (msgid    (or (plist-get msg :message-id)
                       (mu4e-error "Cannot link message without message-id"))))
    (org-store-link-props
     :type                     "mu4e"
     :date                     date
     :from                     (when from
                                 (mu4e~org-address from))
     :maildir                  (plist-get msg :maildir)
     :message-id               msgid
     :path                     (plist-get msg :path)
     :subject                  (plist-get msg :subject)
     :to                       (when to
                                 (mu4e~org-address to))
     :link                     (concat "mu4e:msgid:" msgid)
     :description              (or (plist-get msg :subject) "No subject"))))

(defun mu4e-org-store-link ()
  "Store a link to a mu4e message or query.
It links to the last known query when in `mu4e-headers-mode' with
`mu4e-org-link-query-in-headers-mode' set; otherwise it links to
a specific message, based on its message-id, so that links stay
valid even after moving the message around."
  (when (derived-mode-p 'mu4e-view-mode 'mu4e-headers-mode)
    (if (and (derived-mode-p 'mu4e-headers-mode)
             mu4e-org-link-query-in-headers-mode)
        (mu4e~org-store-link-query)
      (when (mu4e-message-at-point)
        (mu4e~org-store-link-message)))))
                                         ;
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

(make-obsolete 'org-mu4e-store-and-capture
               'mu4e-org-store-and-capture "1.3.6")

;; install mu4e-link support.
(org-link-set-parameters "mu4e"
                         :follow #'mu4e-org-open
                         :store  #'mu4e-org-store-link)
(provide 'mu4e-org)
;;; mu4e-org.el ends here
