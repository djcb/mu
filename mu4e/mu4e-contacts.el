;;; mu4e-contacts.el --- Dealing with contacts -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Dirk-Jan C. Binnema

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

;; Functions and configuration for dealing with contacts in mu4e.

;;; Code:
(require 'cl-lib)
(require 'message)

;; opportunistic; if we have it, try convert pcre-style regexps (from the mu
;; server) to emacs-style.
(require 'pcre2el nil 'noerror)


(require 'mu4e-helpers)
(require 'mu4e-update)

;;; Configuration
(defcustom mu4e-compose-complete-addresses t
  "Whether to do auto-completion of e-mail addresses."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-personal nil
  "Whether to consider only \"personal\" e-mail addresses for completion.
That is, addresses from messages where user was explicitly in one
of the address fields (this excludes mailing list messages).
These addresses are the ones specified with \"mu init\"."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-after "2018-01-01"
  "Consider only contacts last seen after this date.

Date must be a string of the form YYYY-MM-DD.

This is useful for limiting a potentially enormous set of
contacts for auto-completion to just those that are present in
the e-mail corpus in recent times. Set to nil to not have any
time-based restriction."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-max nil
  "Limit the amount of contacts for completion, nil for no limits.
After considering the other constraints
\(`mu4e-compose-complete-addresses' and
`mu4e-compose-complete-only-after'), pick only the highest-ranked
<n>.

Lowering this variable reduces start-up time and memory usage."
  :type '(choice natnum (const :tag "No limits" nil))
  :group 'mu4e-compose)

;; names and mail-addresses can be mapped onto their canonical
;; counterpart.  Use the customizable function
;; mu4e-canonical-contact-function to do that.  below the identity
;; function for mapping a contact onto the canonical one.
(defun mu4e-contact-identity (contact)
  "Return the name and the mail-address of a CONTACT.
It is used as the identity function for converting contacts to
their canonical counterpart; useful as an example."
  (let ((name (plist-get contact :name))
        (mail (plist-get contact :mail)))
    (list :name name :mail mail)))

(defcustom mu4e-contact-process-function
  (lambda(addr)
    (cond
     ((string-match-p "reply" addr)
      ;; no-reply addresses are not useful of course, but neither are are
      ;; reply-xxxx addresses since they're auto-generated only useful for
      ;; direct replies.
      nil)
     (t addr)))
  "Function for processing contact information for use in auto-completion.

The function receives the contact as a string, e.g \"Foo Bar
   <foo.bar@example.com>\" \"cuux@example.com\"

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

(defvar mu4e--contacts-set nil
  "Set with the full contact addresses for autocompletion.")

(defun mu4e-rx-pcre-to-emacs (pcre-rx)
  "Convert a PCRE regular-expression PCRE-RX to Emacs-style.

This depends on the \"pcre2el\" package being available. If not,
simply returns its argument. The same happen for
regular-expressions that cannot be converted. See the pcre2el
documentation for further details."
  (let ((emacs-rx
         (if (fboundp 'pcre-to-elisp)
             (with-demoted-errors "pcre2el error: %S"
               (pcre-to-elisp pcre-rx))
           pcre-rx)))
    (unless (string= pcre-rx emacs-rx)
     (mu4e-log 'misc "converted %s => %s" pcre-rx emacs-rx))
    emacs-rx))

(defun mu4e--grab-rx-addr(addr)
  "Grab the regexp address (if any) from ADDR.
Address string ADDR is either a normal address like
\"foo@example.com\", or a regex-address like
\"/(foo|bar)@example\\.com/\". In the former case, return nil,
and in latter case, return the part between the slashes."
  (save-match-data
    (when (string-match "^/\\(.*\\)/$" addr)
      (match-string 1 addr))))

(defun mu4e--massage-addresses (addrs)
  "Convert list of addresses ADDRS into Emacs compatible.
This means any regexp-addresses are converted from PCRE to Emacs.
Other addresses remain unchanged."
  (seq-map
   (lambda (addr)
     (if-let* ((rxaddr (mu4e--grab-rx-addr addr)))
         (concat "/" (mu4e-rx-pcre-to-emacs rxaddr) "/")
       addr))
   addrs))

;;; user mail address
(defun mu4e-personal-addresses (&optional no-regexp)
  "Get the list user's personal addresses, as passed to \"mu init\".

The address are either plain e-mail addresses or regexps (strings
wrapped in / /). When NO-REGEXP is non-nil, do not include regexp
address patterns (if any)."
  (seq-remove
   (lambda (addr) (and no-regexp (mu4e--grab-rx-addr addr)))
   (when-let* ((props (mu4e-server-properties)))
     (plist-get props :personal-addresses))))

(defun mu4e-personal-address-p (addr)
  "Is ADDR a personal address?
Evaluate to nil if ADDR does not match any of the personal
addresses.  Uses \\=(mu4e-personal-addresses) for the addresses
with both the plain addresses and /regular expressions/."
  (when addr
    (seq-find
     (lambda (m)
       (if-let* ((rxaddr (mu4e--grab-rx-addr m)) (case-fold-search t))
           (string-match rxaddr addr)
         (eq t (compare-strings addr nil nil m nil nil 'case-insensitive))))
     (mu4e-personal-addresses))))

(defun mu4e-personal-or-alternative-address-p (addr)
  "Is ADDR either a personal or an alternative address?

That is, does it match either `mu4e-personal-address-p' or
`message-alternative-emails'.

Note that this expanded definition of user-addresses is only used
in Emacs, not in `mu' (e.g., when indexing).

Also see `mu4e-personal-or-alternative-address-or-empty-p'."
  (let ((alts message-alternative-emails))
    (or (mu4e-personal-address-p addr)
        (cond
         ((functionp alts) (funcall alts addr))
         ((stringp alts)   (string-match alts addr))
         (t nil)))))

(defun mu4e-personal-or-alternative-address-or-empty-p (addr)
  "Is ADDR either a personal, alternative address or nil?

This is like `mu4e-personal-or-alternative-address-p' but also
return t for _empty_ ADDR. This can be useful for use with
`message-dont-reply-to-names' since it can receive empty strings;
those can be filtered-out by returning t here.

See #2680 for further details."
  (or (and addr (string= addr ""))
      (mu4e-personal-or-alternative-address-p addr)))

;; Helpers

;;; RFC2822 handling of phrases in mail-addresses
;;
;; The optional display-name contains a phrase, it sits before the
;; angle-addr as specified in RFC2822 for email-addresses in header
;; fields.  Contributed by jhelberg.

(defun mu4e--rfc822-phrase-type (ph)
  "Return an atom or quoted-string for the phrase PH.
This checks for empty string first. Then quotes around the phrase
\(returning symbol `rfc822-quoted-string'). Then whether there is
a quote inside the phrase (returning symbol
`rfc822-containing-quote').

The reverse of the RFC atext definition is then tested. If it
matches, nil is returned, if not, it returns a symbol
`rfc822-atom'."
  (cond
   ((= (length ph) 0) 'rfc822-empty)
   ((= (aref ph 0) ?\")
    (if (string-match "\"\\([^\"\\\n]\\|\\\\.\\|\\\\\n\\)*\"" ph)
        'rfc822-quoted-string
      'rfc822-containing-quote))   ; starts with quote, but doesn't end with one
   ((string-match-p "[\"]" ph) 'rfc822-containing-quote)
   ((string-match-p "[\000-\037()\*<>@,;:\\\.]+" ph) nil)
   (t 'rfc822-atom)))

(defun mu4e--rfc822-quote-phrase (ph)
  "Quote an RFC822 phrase PH only if necessary.
Atoms and quoted strings don't need quotes. The rest do.  In
case a phrase contains a quote, it will be escaped."
  (let ((type (mu4e--rfc822-phrase-type ph)))
    (cond
     ((eq type 'rfc822-atom) ph)
     ((eq type 'rfc822-quoted-string) ph)
     ((eq type 'rfc822-containing-quote)
      (format "\"%s\""
              (replace-regexp-in-string "\"" "\\\\\"" ph)))
     (t (format "\"%s\"" ph)))))

(defsubst mu4e-contact-name (contact)
  "Get the name of this CONTACT, or nil."
  (plist-get contact :name))

(defsubst mu4e-contact-email (contact)
  "Get the name of this CONTACT, or nil."
  (plist-get contact :email))

(defsubst mu4e-contact-cons (contact)
  "Convert a CONTACT plist into a old-style (name . email)."
    (cons
     (mu4e-contact-name contact)
     (mu4e-contact-email contact)))

(defsubst mu4e-contact-make (name email)
  "Create a contact plist from NAME and EMAIL."
    `(:name ,name :email ,email))

(defun mu4e-contact-full (contact)
  "Get the full combination of name and email address from CONTACT."
  (let* ((email (mu4e-contact-email contact))
         (name (mu4e-contact-name contact)))
    (if (and name (> (length name) 0))
        (format "%s <%s>" (mu4e--rfc822-quote-phrase name) email)
      email)))

(defun mu4e--update-contacts (contacts &optional tstamp)
  "Receive a sorted list of CONTACTS newer than TSTAMP.
Update an internal set with it.

This is used by the completion function in mu4e-compose."
  (let ((n 0))
    (unless mu4e--contacts-set
      (setq mu4e--contacts-set (make-hash-table :test 'equal :weakness nil
                                           :size (length contacts))))
    (dolist (contact contacts)
      (cl-incf n)
      (when (functionp mu4e-contact-process-function)
        (setq contact (funcall mu4e-contact-process-function contact)))
      (when contact ;; note the explicit deccode; the strings we get are
                      ;; utf-8, but emacs doesn't know yet.
        (puthash (decode-coding-string contact 'utf-8) t mu4e--contacts-set)))
    (setq mu4e--contacts-tstamp (or tstamp "0"))
    (unless (zerop n)
      (mu4e-index-message "Contacts updated: %d; total %d"
                          n (hash-table-count mu4e--contacts-set)))))

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

    (when mu4e--contacts-set
      (insert (format "number of contacts cached: %d\n\n"
                      (hash-table-count mu4e--contacts-set)))
      (maphash (lambda (contact _)
                 (insert (format "%s\n" contact))) mu4e--contacts-set))
    (pop-to-buffer "*mu4e-contacts-info*")))

(declare-function mu4e--server-contacts  "mu4e-server")

(defun mu4e--request-contacts-maybe ()
  "Maybe update the set of contacts for autocompletion.

If `mu4e-compose-complete-addresses' is non-nil, get/update the
list of contacts we use for autocompletion; otherwise, do
nothing."
  (when mu4e-compose-complete-addresses
    (mu4e--server-contacts
     mu4e-compose-complete-only-personal
     mu4e-compose-complete-only-after
     mu4e-compose-complete-max
     mu4e--contacts-tstamp)))

(provide 'mu4e-contacts)
;;; mu4e-contacts.el ends here
