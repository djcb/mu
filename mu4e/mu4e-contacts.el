;;; mu4e-contacts.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dirk-Jan C. Binnema

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

;; Utility functions used in the mu4e

;;; Code:
(require 'cl-lib)
(require 'mu4e-helpers)


;;; Configuration
(defcustom mu4e-compose-complete-addresses t
  "Whether to do auto-completion of e-mail addresses."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-personal nil
  "Whether to consider only 'personal' e-mail addresses for completion.
That is, addresses from messages where user was explicitly in one
of the address fields (this excludes mailing list messages).
These addresses are the ones specified with `mu init'."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-after "2014-01-01"
  "Consider only contacts last seen after this date.

Date must be a string of the form YYY-MM-DD.

This is useful for limiting a potentially enormous set of
contacts for auto-completion to just those that are present in
the e-mail corpus in recent timses. Set to nil to not have any
time-based restriction."
  :type 'string
  :group 'mu4e-compose)

;; names and mail-addresses can be mapped onto their canonical
;; counterpart.  use the customizeable function
;; mu4e-canonical-contact-function to do that.  below the identity
;; function for mapping a contact onto the canonical one.
(defun mu4e-contact-identity (contact)
  "Return the name and the mail-address of a CONTACT.
It is used as the identity function for converting contacts to
their canonical counterpart; useful as an example."
  (let ((name (plist-get contact :name))
        (mail (plist-get contact :mail)))
    (list :name name :mail mail)))

(make-obsolete-variable 'mu4e-contact-rewrite-function
                        "mu4e-contact-process-function (see docstring)"
			"mu4e 1.3.2")
(make-obsolete-variable 'mu4e-compose-complete-ignore-address-regexp
                        "mu4e-contact-process-function (see docstring)"
			"mu4e 1.3.2")

(defcustom mu4e-contact-process-function
  (lambda(addr) ;; filter-out no-reply addresses
    (unless (string-match-p "no[t]?[-\\.]?repl\\(y\\|ies\\)" addr)
      addr))
  "Function for processing contact information for use in auto-completion.

The function receives the contact as a string, e.g
   \"Foo Bar <foo.bar@example.com>\"
   \"cuux@example.com\"

The function should return either:
- nil: do not use this contact for completion
- the (possibly rewritten) address, which must be
an RFC-2822-compatible e-mail address."
  :type 'function
  :group 'mu4e-compose)

(defcustom mu4e-compose-reply-ignore-address
  '("no-?reply")
  "Addresses to prune when doing wide replies.

This can be a regexp matching the address, a list of regexps or a
predicate function. A value of nil keeps all the addresses."
  :type '(choice
          (const nil)
          function
          string
          (repeat string))
  :group 'mu4e-compose)


;;; Internal variables
(defvar mu4e--contacts-tstamp "0"
  "Timestamp for the most recent contacts update." )

(defvar mu4e--contacts-hash nil
  "Hash that maps contacts (ie. 'name <e-mail>') to an integer for sorting.
We need to keep this information around to quickly re-sort
subsets of the contacts in the completions function in
mu4e-compose.")

;;; user mail address
(defun mu4e-personal-addresses(&optional no-regexp)
  "Get the list user's personal addresses, as passed to mu init.
The address are either plain e-mail address or /regular
 expressions/. When NO-REGEXP is non-nil, do not include regexp
 address patterns (if any)."
  (seq-remove
   (lambda(addr) (and no-regexp (string-match-p "^/.*/" addr)))
   (when (mu4e-server-properties)
     (plist-get (mu4e-server-properties) :personal-addresses))))

(defun mu4e-personal-address-p (addr)
  "Is ADDR a personal address?
Evaluate to nil if ADDR matches any of the personal addresses.
Uses (mu4e-personal-addresses) for the addresses with both the plain
addresses and /regular expressions/."
  (when addr
    (seq-find
     (lambda (m)
       (if (string-match "/\\(.*\\)/" m)
           (let ((rx (match-string 1 m))
                 (case-fold-search t))
             (if (string-match rx addr) t nil))
         (eq t (compare-strings addr nil nil m nil nil 'case-insensitive))))
     (mu4e-personal-addresses))))

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


(defun mu4e--update-contacts (contacts &optional tstamp)
  "Receive a sorted list of CONTACTS newer than TSTAMP.
Each of the contacts has the form
  (FULL_EMAIL_ADDRESS . RANK) and fill `mu4e--contacts-hash' with
it, with each contact mapped to an integer for their ranking.

This is used by the completion function in mu4e-compose."
  ;; We have our nicely sorted list, map them to a list
  ;; of increasing integers. We use that map in the composer
  ;; to sort them there. It would have been so much easier if emacs
  ;; allowed us to use the sorted-list as-is, but no such luck.
  (let ((n 0))
    (unless mu4e--contacts-hash
      (setq mu4e--contacts-hash (make-hash-table :test 'equal :weakness nil
                                           :size (length contacts))))
    (dolist (contact contacts)
      (cl-incf n)
      (let* ((address (plist-get contact :address))
             (address
              (if (functionp mu4e-contact-process-function)
                  (funcall mu4e-contact-process-function address)
                address)))
        (when address ;; note the explicit deccode; the strings we get are
		      ;; utf-8, but emacs doesn't know yet.
          (puthash (decode-coding-string address 'utf-8)
                   (plist-get contact :rank) mu4e--contacts-hash))))

    (setq mu4e--contacts-tstamp (or tstamp "0"))

    (unless (zerop n)
      (mu4e-index-message "Contacts updated: %d; total %d"
                          n (hash-table-count mu4e--contacts-hash)))))

(defun mu4e-contacts-info ()
  "Display information about the contacts-cache.
For testing/debugging."
  (interactive)
  (with-current-buffer (get-buffer-create "*mu4e-contacts-info*")
    (erase-buffer)
    (insert (format "complete addresses:        %s\n"
                    (if mu4e-compose-complete-addresses "yes" "no")))
    (insert (format "only personal addresses:   %s\n"
                    (if mu4e-compose-complete-only-personal "yes" "no")))
    (insert (format "only addresses seen after: %s\n"
                    (or mu4e-compose-complete-only-after "no restrictions")))

    (when mu4e--contacts-hash
      (insert (format "number of contacts cached: %d\n\n"
                      (hash-table-count mu4e--contacts-hash)))
      (let ((contacts))
        (maphash (lambda (addr rank)
                   (setq contacts (cons (cons rank addr) contacts)))
                 mu4e--contacts-hash)
        (setq contacts (sort contacts
                             (lambda(cell1 cell2) (< (car cell1) (car cell2)))))
        (dolist (contact contacts)
          (insert (format "%s\n" (cdr contact))))))

    (pop-to-buffer "*mu4e-contacts-info*")))

(declare-function mu4e~proc-contacts  "mu4e-proc")

(defun mu4e--request-contacts-maybe ()
  "If `mu4e-compose-complete-addresses' is non-nil, get/update
the list of contacts we use for autocompletion; otherwise, do
nothing."
  (when mu4e-compose-complete-addresses
    (mu4e~proc-contacts
     mu4e-compose-complete-only-personal
     mu4e-compose-complete-only-after
     mu4e--contacts-tstamp)))

(provide 'mu4e-contacts)
;;; mu4e-contacts.el ends here
