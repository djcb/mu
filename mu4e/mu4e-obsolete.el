;;; mu4e-obsolete.el --- Obsolete things -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Dirk-Jan C. Binnema

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

;; Obsolete variable & function aliases go here, so we don't clutter up the
;; code.

;;; Code:


;; mu4e-draft/compose

(make-obsolete-variable 'mu4e-reply-to-address
                        'mu4e-compose-reply-to-address
                        "v0.9.9")

(make-obsolete-variable 'mu4e-auto-retrieve-keys  "no longer used." "1.3.1")

(make-obsolete-variable 'mu4e-compose-func "no longer used" "1.11.26")

(make-obsolete-variable 'mu4e-compose-crypto-reply-encrypted-policy "The use of the
 'mu4e-compose-crypto-reply-encrypted-policy' variable is deprecated.
 'mu4e-compose-crypto-policy' should be used instead" "2020-03-06")

(make-obsolete-variable 'mu4e-compose-crypto-reply-plain-policy "The use of the
 'mu4e-compose-crypto-reply-plain-policy' variable is deprecated.
 'mu4e-compose-crypto-policy' should be used instead"
                        "2020-03-06")

(make-obsolete-variable 'mu4e-compose-crypto-reply-policy "The use of the
 'mu4e-compose-crypto-reply-policy' variable is deprecated.
 'mu4e-compose-crypto-reply-plain-policy' and
 'mu4e-compose-crypto-reply-encrypted-policy' should be used instead"
                        "2017-09-02")

(make-obsolete-variable 'mu4e-compose-auto-include-date
                        "This is done unconditionally now" "1.3.5")

(make-obsolete-variable 'mu4e-compose-signature-auto-include
                        "Usage message-signature directly" "1.11.22")

(define-obsolete-variable-alias
  'mu4e-compose-signature 'message-signature "1.11.22")
(define-obsolete-variable-alias
  'mu4e-compose-cite-function 'message-cite-function "1.11.22")
(define-obsolete-variable-alias
  'mu4e-compose-in-new-frame 'mu4e-compose-switch "1.11.22")

(define-obsolete-variable-alias 'mu4e-compose-hidden-headers
  'mu4e-draft-hidden-headers "1.12.5")


;; mu4e-message

(make-obsolete-variable 'mu4e-html2text-command "No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-view-prefer-html "No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-view-html-plaintext-ratio-heuristic
                        "No longer in use" "1.7.0")
(make-obsolete-variable 'mu4e-message-body-rewrite-functions
                        "No longer in use" "1.7.0")
;;; Html2Text
(make-obsolete 'mu4e-shr2text "No longer in use" "1.7.0")



;; old message view
(make-obsolete-variable 'mu4e-view-show-addresses
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-wrap-lines nil "0.9.9-dev7")
(make-obsolete-variable 'mu4e-view-hide-cited nil "0.9.9-dev7")
(make-obsolete-variable 'mu4e-view-date-format
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-image-max-width
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-image-max-height
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-save-multiple-attachments-without-asking
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-attachment-assoc
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-attachment-actions
                        "See mu4e-view-mime-part-actions" "1.7.0")
(make-obsolete-variable 'mu4e-view-header-field-keymap
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-header-field-keymap
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-contacts-header-keymap
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-view-attachments-header-keymap
                        "Unused with the new message view" "1.7.0")
(make-obsolete-variable 'mu4e-imagemagick-identify nil "1.7.0")
(make-obsolete-variable 'mu4e-view-show-images
                        "No longer used" "1.7.0")
(make-obsolete-variable 'mu4e-view-gnus     "Old view is gone" "1.7.0")
(make-obsolete-variable 'mu4e-view-use-gnus "Gnus view is the default" "1.5.10")

(make-obsolete-variable 'mu4e-cited-regexp "No longer used" "1.7.0")

(define-obsolete-variable-alias 'mu4e-view-blocked-images 'gnus-blocked-images
  "1.5.12")
(define-obsolete-variable-alias 'mu4e-view-inhibit-images 'gnus-inhibit-images
  "1.5.12")

(define-obsolete-variable-alias 'mu4e-after-view-message-hook
  'mu4e-view-rendered-hook "1.9.7")


;; mu4e-org
(define-obsolete-function-alias 'org-mu4e-open 'mu4e-org-open "1.3.6")
(define-obsolete-function-alias 'org-mu4e-store-and-capture
  'mu4e-org-store-and-capture "1.3.6")


;; mu4e-search
(define-obsolete-variable-alias 'mu4e-headers-results-limit
  'mu4e-search-results-limit "1.7.0")
(define-obsolete-variable-alias 'mu4e-headers-full-search
  'mu4e-search-full "1.7.0")
(define-obsolete-variable-alias 'mu4e-headers-show-threads
  'mu4e-search-threads "1.7.0")
(define-obsolete-variable-alias
  'mu4e-headers-search-bookmark-hook
  'mu4e-search-bookmark-hook "1.7.0")
(define-obsolete-variable-alias 'mu4e-headers-search-hook
  'mu4e-search-hook "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-search 'mu4e-search "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-search-edit
  'mu4e-search-edit "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-search-bookmark
  'mu4e-search-bookmark "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-search-bookmark-edit
  'mu4e-search-bookmark-edit "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-search-narrow
  'mu4e-search-narrow "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-rerun-search
  'mu4e-search-rerun "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-query-next
  'mu4e-search-next "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-query-prev
  'mu4e-search-prev "1.7.0")
(define-obsolete-function-alias 'mu4e-headers-forget-queries
  'mu4e-search-forget "1.7.0")
(define-obsolete-function-alias 'mu4e-read-query
  'mu4e-search-read-query "1.7.0")

(make-obsolete-variable 'mu4e-display-update-status-in-modeline
                        "No longer used" "1.9.11")

;; mu4e-headers
(make-obsolete-variable 'mu4e-headers-field-properties-function
                        "not used" "1.6.1")

(define-obsolete-function-alias 'mu4e-headers-toggle-setting
  'mu4e-headers-toggle-property "1.9.5")
(define-obsolete-function-alias 'mu4e-headers-toggle-threading
  'mu4e-headers-toggle-property "1.9.5")
(define-obsolete-function-alias 'mu4e-headers-toggle-full-search
  'mu4e-headers-toggle-property "1.9.5")
(define-obsolete-function-alias 'mu4e-headers-toggle-include-related
  'mu4e-headers-toggle-property "1.9.5")
(define-obsolete-function-alias 'mu4e-headers-toggle-skip-duplicates
  'mu4e-headers-toggle-property "1.9.5")

(define-obsolete-function-alias 'mu4e-headers-change-sorting
  'mu4e-search-change-sorting "1.9.11")
(define-obsolete-function-alias 'mu4e-headers-toggle-property
  'mu4e-search-toggle-property "1.9.11")

(define-obsolete-variable-alias 'mu4e-headers-include-related
  'mu4e-search-include-related "1.9.11")
(define-obsolete-variable-alias 'mu4e-headers-skip-duplicates
  'mu4e-search-skip-duplicates "1.9.11")
(define-obsolete-variable-alias 'mu4e-headers-sort-field
  'mu4e-search-sort-field "1.9.11")
(define-obsolete-variable-alias 'mu4e-headers-sort-direction
  'mu4e-search-sort-direction "1.9.11")

(define-obsolete-variable-alias 'mu4e-headers-hide-predicate
  'mu4e-search-hide-predicate "1.9.11")
(define-obsolete-variable-alias 'mu4e-headers-hide-enabled
                        'mu4e-search-hide-enabled "1.9.11")

(define-obsolete-variable-alias 'mu4e-headers-threaded-label
  'mu4e-search-threaded-label "1.9.12")
(define-obsolete-variable-alias 'mu4e-headers-full-label
  'mu4e-search-full-label "1.9.12")
(define-obsolete-variable-alias 'mu4e-headers-related-label
  'mu4e-search-related-label "1.9.12")
(define-obsolete-variable-alias 'mu4e-headers-skip-duplicates-label
  'mu4e-search-skip-duplicates-label "1.9.12")
(define-obsolete-variable-alias 'mu4e-headers-hide-label
  'mu4e-search-hide-label "1.9.12")
;; by exception, add alias for internal func
(define-obsolete-function-alias 'mu4e~headers-jump-to-maildir
  'mu4e-search-maildir "1.9.13")


;; mu4e-main
(define-obsolete-variable-alias
  'mu4e-main-buffer-hide-personal-addresses
  'mu4e-main-hide-personal-addresses "1.5.7")


;; mu4e-server

(make-obsolete-variable
 'mu4e-maildir
 "determined by server; see `mu4e-root-maildir'." "1.3.8")

(make-obsolete-variable 'mu4e-header-func "mu4e-headers-append-func" "1.7.4")
(make-obsolete-variable 'mu4e-temp-func "No longer used" "1.7.0")
(make-obsolete-variable 'mu4e-sent-func  "No longer used" "1.12.5")


;; mu4e-update
(define-obsolete-function-alias 'mu4e-interrupt-update-mail
  'mu4e-kill-update-mail "1.0-alpha0")

;; mu4e-helpers
(define-obsolete-function-alias 'mu4e-quote-for-modeline
  'mu4e--modeline-quote-and-truncate "1.9.16")

;; mu4e-folder
(make-obsolete-variable 'mu4e-cache-maildir-list "No longer used" "1.11.15")

;; mu4e-contacts

(define-obsolete-function-alias 'mu4e-user-mail-address-p
  'mu4e-personal-address-p "1.5.5")

;; don't use the older vars anymore
(make-obsolete-variable 'mu4e-user-mail-address-regexp
                        'mu4e-user-mail-address-list "0.9.9.x")
(make-obsolete-variable 'mu4e-my-email-addresses
                        'mu4e-user-mail-address-list "0.9.9.x")
(make-obsolete-variable 'mu4e-user-mail-address-list
                        "determined by server; see `mu4e-personal-addresses'."
                        "1.3.8")
(make-obsolete-variable 'mu4e-contact-rewrite-function
                        "mu4e-contact-process-function (see docstring)"
                        "1.3.2")
(make-obsolete-variable 'mu4e-compose-complete-ignore-address-regexp
                        "mu4e-contact-process-function (see docstring)"
                        "1.3.2")

(make-obsolete-variable 'mu4e-compose-reply-recipients
                        "use mu4e-compose-reply / mu4e-compose-wide-reply"
                        "1.11.23")
(make-obsolete-variable 'mu4e-compose-reply-ignore-address
                        "see: message-prune-recipient-rules" "1.11.23")

;; this is only a _rough_
(make-obsolete-variable 'mu4e-compose-dont-reply-to-self
                        "message-dont-reply-to-names"
                        "1.11.24")
;; calendar
(define-obsolete-function-alias 'mu4e-icalendar-setup
  'gnus-icalendar-setup '"1.11.22")

(make-obsolete-variable 'mu4e-icalendar-trash-after-reply
                        "Not functional after composer changes"
                        "1.12.5")
;; mu4e.
(define-obsolete-function-alias 'mu4e-clear-caches #'ignore "1.11.15")

(provide 'mu4e-obsolete)
;;; mu4e-obsolete.el ends here
