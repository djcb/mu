;;; mu4e-vars.el --- Variables and faces for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2023 Dirk-Jan C. Binnema

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

(require 'message)
(require 'mu4e-helpers)

;;; Configuration
(defgroup mu4e nil
  "Mu4e - an email-client for Emacs."
  :group 'mail)

(defcustom mu4e-confirm-quit t
  "Whether to confirm to quit mu4e."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-modeline-support t
  "Support for showing information in the modeline."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-notification-support nil
  "Support for new-message notifications."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-org-support t
  "Support Org-mode links."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-speedbar-support nil
  "Support having a speedbar to navigate folders/bookmarks."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-eldoc-support nil
  "Support eldoc help in the headers-view."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-date-format-long "%c"
  "Date format to use in the message view.
Follows the format of `format-time-string'."
  :type 'string
  :group 'mu4e)

(defcustom mu4e-dim-when-loading t
  "Dim buffer text when loading new data.
If non-nil, dim some buffers during data retrieval and rendering,
and show some \"Loading\" banner."
  :type 'boolean
  :group 'mu4e)


;;; Faces

(defgroup mu4e-faces nil
  "Type faces (fonts) used in mu4e."
  :group 'mu4e
  :group 'faces)

(defface mu4e-unread-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for an unread message header."
  :group 'mu4e-faces)

(defface mu4e-trashed-face
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for an message header in the trash folder."
  :group 'mu4e-faces)

(defface mu4e-draft-face
  '((t :inherit font-lock-string-face))
  "Face for a draft message header.
I.e. a message with the draft flag set."
  :group 'mu4e-faces)

(defface mu4e-flagged-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for a flagged message header."
  :group 'mu4e-faces)

(defface mu4e-replied-face
  '((t :inherit font-lock-builtin-face :weight normal :slant normal))
  "Face for a replied message header."
  :group 'mu4e-faces)

(defface mu4e-forwarded-face
  '((t :inherit font-lock-builtin-face :weight normal :slant normal))
  "Face for a passed (forwarded) message header."
  :group 'mu4e-faces)

(defface mu4e-header-face
  '((t :inherit default))
  "Face for a header without any special flags."
  :group 'mu4e-faces)

(defface mu4e-related-face
  '((t :inherit default :slant italic))
  "Face for a \='related' header." :group 'mu4e-faces)

(defface mu4e-header-title-face
  '((t :inherit font-lock-type-face))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-header-highlight-face
  `((t :inherit hl-line :weight bold :underline t
       ,@(and (>= emacs-major-version 27) '(:extend t))))
  "Face for the header at point."
  :group 'mu4e-faces)

(defface mu4e-header-marks-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the mark in the headers list."
  :group 'mu4e-faces)

(defface mu4e-header-key-face
  '((t :inherit message-header-name :weight bold))
  "Face used to highlight items in various places."
  :group 'mu4e-faces)

(defface mu4e-header-field-face
  '((t :weight bold))
  "Face for a header field name (such as \"Subject:\" in \"Subject:\
Foo\")."
  :group 'mu4e-faces)

(defface mu4e-header-value-face
  '((t :inherit font-lock-type-face))
  "Face for a header value (such as \"Re: Hello!\")."
  :group 'mu4e-faces)

(defface mu4e-special-header-value-face
  '((t :inherit font-lock-builtin-face))
  "Face for special header values."
  :group 'mu4e-faces)

(defface mu4e-link-face
  '((t :inherit link))
  "Face for showing URLs and attachments in the message view."
  :group 'mu4e-faces)

(defface mu4e-contact-face
  '((t :inherit font-lock-variable-name-face))
  "Face for showing URLs and attachments in the message view."
  :group 'mu4e-faces)

(defface mu4e-highlight-face
  '((t :inherit highlight))
  "Face for highlighting things."
  :group 'mu4e-faces)

(defface mu4e-title-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-modeline-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for the query in the mode-line."
  :group 'mu4e-faces)

(defface mu4e-footer-face
  '((t :inherit font-lock-comment-face))
  "Face for message footers (signatures)."
  :group 'mu4e-faces)

(defface mu4e-url-number-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the number tags for URLs."
  :group 'mu4e-faces)

(defface mu4e-system-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for system message (such as the footers for message headers)."
  :group 'mu4e-faces)

(defface mu4e-ok-face
  '((t :inherit font-lock-comment-face :weight bold :slant normal))
  "Face for things that are okay."
  :group 'mu4e-faces)

(defface mu4e-warning-face
  '((t :inherit font-lock-warning-face :weight bold :slant normal))
  "Face for warnings / error."
  :group 'mu4e-faces)

(defface mu4e-compose-separator-face
  '((t :inherit message-separator :slant italic))
  "Face for the headers/message separator in mu4e-compose-mode."
  :group 'mu4e-faces)

(defface mu4e-region-code
    '((t (:background "DarkSlateGray")))
  "Face for highlighting marked region in mu4e-view buffer."
  :group 'mu4e-faces)

;;; Header information

(defconst mu4e-header-info
  '((:bcc
     . (:name "Bcc"
        :shortname "Bcc"
        :help "Blind Carbon-Copy recipients for the message"
        :sortable t))
    (:cc
     . (:name "Cc"
        :shortname "Cc"
        :help "Carbon-Copy recipients for the message"
        :sortable t))
    (:changed
     . (:name "Changed"
        :shortname "Chg"
        :help "Date/time when the message was changed most recently"
        :sortable t))
    (:date
     . (:name "Date"
        :shortname "Date"
        :help "Date/time when the message was sent"
        :sortable t))
    (:human-date
     . (:name "Date"
        :shortname "Date"
        :help "Date/time when the message was sent"
        :sortable :date))
    (:flags
     . (:name "Flags"
        :shortname "Flgs"
        :help "Flags for the message"
        :sortable nil))
    (:from
     . (:name "From"
        :shortname "From"
        :help "The sender of the message"
        :sortable t))
    (:from-or-to
     . (:name "From/To"
        :shortname "From/To"
        :help "Sender of the message if it's not me; otherwise the recipient"
        :sortable nil))
    (:maildir
     . (:name "Maildir"
        :shortname "Maildir"
        :help "Maildir for this message"
        :sortable t))
    (:list
     . (:name "List-Id"
        :shortname "List"
        :help "Mailing list id for this message"
        :sortable t))
    (:mailing-list
     . (:name "List"
        :shortname "List"
        :help "Mailing list friendly name for this message"
        :sortable :list))
    (:message-id
     . (:name "Message-Id"
        :shortname "MsgID"
        :help "Message-Id for this message"
        :sortable nil))
    (:path
     . (:name "Path"
        :shortname "Path"
        :help "Full filesystem path to the message"
        :sortable t))
    (:size
     . (:name "Size"
        :shortname "Size"
        :help "Size of the message"
        :sortable t))
    (:subject
     . (:name "Subject"
        :shortname "Subject"
        :help "Subject of the message"
        :sortable t))
    (:tags
     . (:name "Tags"
        :shortname "Tags"
        :help "Tags for the message"
        ;; sort by _first_ tag.
        :sortable t))
    (:thread-subject
     . (:name "Subject"
        :shortname "Subject"
        :help "Subject of the thread"
        :sortable :subject))
    (:to
     . (:name "To"
        :shortname "To"
        :help "Recipient of the message"
        :sortable t)))

  "An alist of all possible header fields and information about them.

This is used in the user-interface (the column headers in the
header list, and the fields the message view).

Most fields should be self-explanatory. A special one is
`:from-or-to', which is equal to `:from' unless `:from' matches
one of the addresses in `(mu4e-personal-addresses)', in which
case it will be equal to `:to'.

Furthermore, the property `:sortable' determines whether we can
sort by this field. This can be either a boolean (nil or t), or a
symbol for /another/ field. For example, the `:human-date' field
uses `:date' for that.

Note, `:sortable' is not supported for custom header fields.")

(defvar mu4e-header-info-custom
  '(
    ;; some examples & debug helpers.

    (:thread-path
     . ;; Shows the internal thread-path
     ( :name "Thread-path"
       :shortname "Thp"
       :help "The thread-path"
       :function (lambda (msg)
                   (let ((thread (mu4e-message-field msg :thread)))
                     (or (and thread (plist-get thread :path)) "")))))

    (:thread-date
     . ;; Shows the internal thread-date
     ( :name "Thread-date"
       :shortname "Thd"
       :help "The thread-date"
       :function (lambda (msg)
                   (let* ((thread (mu4e-message-field msg :thread))
                          (tdate (and thread (plist-get thread :date-tstamp))))
                     (format-time-string "%F %T " (or tdate 0))))))
    (:recipnum
     .
     ( :name "Number of recipients"
       :shortname "Recip#"
       :help "Number of recipients for this message"
       :function
       (lambda (msg)
         (format "%d"
                 (+ (length (mu4e-message-field msg :to))
                    (length (mu4e-message-field msg :cc))))))))

  "An  alist of custom (user-defined) headers.
The format is similar to `mu4e-header-info', but adds a :function
property, which should point to a function that takes a message
plist as argument, and returns a string. See the default value of
`mu4e-header-info-custom for an example.

Note that when using the gnus-based view, you only have access to
a limited set of message fields: only the ones used in the
header-view, not including, for instance, the message body.")

;;; Internals

(defvar-local mu4e~headers-view-win nil
  "The view window connected to this headers view.")

;; It's useful to have the current view message available to
;; `mu4e-view-mode-hooks' functions, and we set up this variable
;; before calling `mu4e-view-mode'.  However, changing the major mode
;; clobbers any local variables.  Work around that by declaring the
;; variable permanent-local.
(defvar-local mu4e--view-message nil "The message being viewed in view mode.")
(put 'mu4e--view-message 'permanent-local t)
;;; _
(provide 'mu4e-vars)
;;; mu4e-vars.el ends here
