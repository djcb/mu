;;; mu4e-list-archives.el --- Locate online mailing list archives  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ruijie Yu

;; Author: Ruijie Yu <ruijie@netyu.xyz>

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

;; In this file, we attempt to locate an online archive for a mailing list
;; message.

;;; Code:
(eval-when-compile (require 'subr-x))   ; for `thread-last'
(require 'cl-lib)
(require 'mu4e-lists)

(defgroup mu4e-list-archives nil
  "Configuration group for retrieving online mailing list archives."
  :group 'mu4e-lists)

(defcustom mla-user-actions nil
  "An alist with cells (MAILING-LIST-ID . ARCHIVE-ACTION).
This variable is consulted before `mla-actions' when
determining the archive URL of a given mailing list.

ARCHIVE-ACTION can take one of the following three forms:

- nil: this mailing list has no online archive.  This is useful
to override a default setting in `mla-actions'.

- a string: it should contain a single `format' \"%s\" parameter,
  where the message ID is substituted.

- a function: the function should take one argument: the message
  object as returned by `mu4e-message-at-point', and return
  either nil to indicate no archive, or a string that will be
  treated as the archive URL for the current message."
  :type '(alist :key-type (string :tag "Mailing List ID")
                :value-type
                (choice (const :tag "No online archive" nil)
                        (string :tag "URL format with one \"%s\" parameter")
                        (function :tag "Function returning URL or nil"))))

(defun mla--get-recipients (msg)
  "Helper function to retrieve a list of recipients from MSG."
  (delete-dups
   (delq nil (mapcar (lambda (c) (plist-get c :email))
                     (apply #'append
                            (mapcar (apply-partially #'plist-get msg)
                                    '(:to :cc :bcc)))))))

(defun mla-resolve-debbug (base-url &optional debbug-mail-domain)
  "Return an ARCHIVE-ACTION function for debbug, based on BASE-URL.
This function sequentially checks that one of the following
conditions is true, and returns the url based on the found bug
number if possible, returning nil if all have failed.

1. That the subject contains \"bug#xxxxx\" (case-insensitive).

2. When DEBBUG-MAIL-DOMAIN is non-nil, that one of the recipients
   has address <xxxxx@DEBBUG-MAIL-DOMAIN>.

See `mu4e-list-archives-actions' for details on ARCHIVE-ACTION."
  (lambda (msg)
    "Returned by `mu4e-list-archives-resolve-debbug', which see."
    (and-let*
        ((match-subject (rx bow "bug#" (group-n 1 (+ digit))))
         (match-recipient (if debbug-mail-domain
                              (rx (group-n 1 (+ digit))
                                  ?@ (literal debbug-mail-domain))
                            (rx unmatchable)))
         (bug (cond
               ((let ((case-fold-search nil)) ; (1)
                  (save-match-data
                    (and-let* ((subject (plist-get msg :subject))
                               ((string-match match-subject subject)))
                      (match-string 1 subject)))))
               (debbug-mail-domain      ; (2)
                (save-match-data
                  (seq-drop-while
                   (lambda (recipient)
                     (and-let* (((string-match match-recipient recipient)))
                       (match-string 1 recipient)))
                   (mla--get-recipients msg)))))))
      (format "%s?bug=%s" base-url bug))))

(defun mla--resolve-namazu (search-url-fmt url-base msg)
  "Resolve the actual archive page from the namazu search result.
Return the actual url or nil if not found.

SEARCH-URL-FMT is a url format satisfying the second definition
of ARCHIVE-ACTION.

MSG is a plist returned by `mu4e-message-at-point'.

URL-BASE is the base url string."
  (defvar url-asynchronous)
  (let ((url-asynchronous nil))
    (and-let* (((libxml-available-p))
               (msgid (plist-get msg :message-id))
               (search-url (format search-url-fmt (url-hexify-string msgid)))
               (buf (url-retrieve search-url #'ignore)))
      (with-current-buffer buf
        (while (process-live-p (get-buffer-process buf))
          (sit-for 0.1 t))
        (save-excursion
          (save-match-data
            (goto-char 1)
            (and-let* (((re-search-forward (rx bol eol) nil t))
                       (url (thread-last
                              ;; emacs#63291
                              (libxml-parse-html-region (point) (point-max))
                              (alist-get 'body)   ; get body
                              (alist-get 'dl)     ; get first dl
                              (alist-get 'dt)     ; get first dt
                              (alist-get 'strong) ; get first strong
                              (alist-get 'a)      ; get first aref
                              (car)               ; peel the nested list
                              (alist-get 'href))) ; might be relative url
                       (url (url-generic-parse-url url))
                       (url-base (url-generic-parse-url url-base)))
              (prog2 (url-default-expander url url-base)
                  (url-recreate-url url)))))))))

(defcustom mla-resolve-namazu-search t
  "Whether to resolve namazu search page.
On mailman+namazu system, currently there is no direct way to get
the actual url for a given message id.  The only way is to
perform a \"search\" on namazu.

Enabling this option makes it to look into the structure of the
search page, and to locate the only match.  Requires enabling
libxml support at compile time."
  :type 'boolean
  :safe #'booleanp)

(defun mla-resolve-mailman-namazu (url-base internal-list-id)
  "Return an ARCHIVE-ACTION for mailman namazu.
URL-BASE is the base url such that \"URL-BASE/namazu.cgi\" is the search page.

INTERNAL-LIST-ID is the internal name for the list id, which is
usually the first part of the list id."
  (let ((search-url-fmt
         (format "%s/namazu.cgi?%s"
                 url-base
                 (string-join
                  (list "submit=Search%%21"
                        (format "idxname=%s" internal-list-id)
                        (format "query=%s:%%s" "%%2bmessage-id"))
                  "&"))))
    (lambda (msg)
      (cond
       ((and (libxml-available-p)
             mla-resolve-namazu-search
             ;; This may fail.  Fallback if failed.
             (mla--resolve-namazu search-url-fmt url-base msg)))
       ;; fallback
       ((format search-url-fmt (plist-get msg :message-id)))))))

(defconst mla-actions
  `(("bug-gnu-emacs.gnu.org" . ,(mla-resolve-debbug
                                 "https://debbugs.gnu.org/cgi/bugreport.cgi"))
    ("emacs-orgmode.gnu.org" . "https://list.orgmode.org/%s")
    ("help-gnu-emacs.gnu.org"
     . ,(mla-resolve-mailman-namazu
         "https://lists.gnu.org/archive/cgi-bin" "help-gnu-emacs")))
  "An alist with cells (MAILING-LIST-ID . ARCHIVE-ACTION).
See `mu4e-list-archives-user-actions' for further details.")

(defun mla-resolve (msg)
  "Return the archive url for a mailing list message MSG, or nil.
Based on `mu4e-lists-user-archive-urls' and
`mu4e-lists-archive-urls', in this order."
  (and-let* (msg
             (list-id (plist-get msg :list))
             (msg-id (plist-get msg :message-id))
             (archive-action
              (let ((get (lambda (actions)
                           (alist-get list-id actions nil nil #'string=))))
                (or (funcall get mu4e-list-archives-user-actions)
                    (funcall get mu4e-list-archives-actions)))))
    (pcase archive-action
      ((pred stringp) (format archive-action msg-id))
      ((pred functionp) (funcall archive-action msg)))))

(provide 'mu4e-list-archives)
;;; mu4e-list-archives.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("mla" . "mu4e-list-archives"))
;; End:
