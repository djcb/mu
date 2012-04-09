;;; mu4e-vars.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
 
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup mu4e nil
  "mu4e - mu for emacs"
  :group 'local)

(defcustom mu4e-mu-home nil
  "Location of the mu homedir, or nil for the default."
  :type 'directory
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-mu-binary (executable-find "mu")
  "Name of the mu-binary to use; if it cannot be found in your
PATH, you can specify the full path."
  :type 'file
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-maildir (expand-file-name "~/Maildir")
  "Your Maildir directory; by default, mu4e assumes
~/Maildir."
  :type 'directory
  :safe 'stringp
  :group 'mu4e)

(defcustom mu4e-get-mail-command nil
  "Shell command to run to retrieve new mail; e.g. 'offlineimap' or
'fetchmail'."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-update-interval nil
  "Number of seconds between automatic calls to retrieve mail and
update the database. If nil, don't update automatically. Note,
changes in `mu4e-update-interval' only take effect after restarting
mu4d."
  :type 'integer
  :group 'mu4e
  :safe 'integerp)

(defcustom mu4e-attachment-dir (expand-file-name "~/")
  "Default directory for saving attachments."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defvar mu4e-user-mail-address-regexp "$^"
  "Regular expression matching the user's mail address(es). This is
used to distinguish ourselves from others, e.g. when replying and
in :from-or-to headers. By default, match nothing.")

(defvar mu4e-date-format-long "%c"
  "Date format to use in the message view, in the format of
  `format-time-string'.")

(defvar mu4e-search-results-limit 1000
  "Maximum number of search results (or -1 for unlimited). Since
limiting search results speeds up searches significantly, it's
useful to limit this. Note, to ignore the limit, use a prefix
argument (C-u) before invoking the search.")

(defvar mu4e-debug nil
  "When set to non-nil, log debug information to the *mu4e-log* buffer.")

(defvar mu4e-bookmarks
  '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
     ("date:today..now"                  "Today's messages"     ?t)
     ("date:7d..now"                     "Last 7 days"          ?w)
     ("mime:image/*"                     "Messages with images" ?p))
  "A list of pre-defined queries; these will show up in the main
screen. Each of the list elements is a three-element list of the
form (QUERY DESCRIPTION KEY), where QUERY is a string with a mu
query, DESCRIPTION is a short description of the query (this will
show up in the UI), and KEY is a shortcut key for the query.")

(defvar mu4e-split-view 'horizontal
  "How to show messages / headers; a symbol which is either:
  * a symbol 'horizontal: split horizontally (headers on top)
  * a symbol 'vertical: split vertically (headers on the left).
  * anything else: don't split (show either headers or messages, not both)
Also see `mu4e-headers-visible-lines' and `mu4e-headers-visible-columns'.")

;; Sending
(defgroup mu4e-sending nil
  "E-mail-sending related settings for mu4e."
  :group 'mu4e)

(defcustom mu4e-sent-messages-behavior 'sent
  "Determines what mu4e does with sent messages - this is a symbol
which can be either:
'sent   --> move the sent message to the Sent-folder (`mu4e-sent-folder')
'trash  --> move the sent message to the Trash-folder (`mu4e-trash-folder')
'delete --> delete the sent message.
Note, when using GMail/IMAP, you should set this to either 'trash
or 'delete, since GMail already takes care of keeping copies in the
sent folder."
  :type 'symbol
  :safe 'symbolp
  :group 'mu4e-sending)

;; Folders

(defgroup mu4e-folders nil
  "Special folders."
  :group 'mu4e)

(defcustom mu4e-sent-folder "/sent"
  "Your folder for sent messages, relative to `mu4e-maildir',
  e.g. \"/Sent Items\"."
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)

(defcustom mu4e-drafts-folder "/drafts"
  "Your folder for draft messages, relative to `mu4e-maildir',
  e.g. \"/drafts\""
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)

(defcustom mu4e-trash-folder "/trash"
  "Your folder for trashed messages, relative to `mu4e-maildir',
  e.g. \"/trash\"."
  :type 'string
  :safe 'stringp
  :group 'mu4e-folders)


(defcustom mu4e-maildir-shortcuts nil
  "A list of maildir shortcuts to enable quickly going to the
 particular for, or quickly moving messages towards them (i.e.,
 archiving or refiling). The list contains elements of the form
 (maildir . shortcut), where MAILDIR is a maildir (such as
\"/archive/\"), and shortcut a single shortcut character. With
this, in the header buffer and view buffer you can execute
`mu4e-mark-for-move-quick' (or 'm', by default) or
`mu4e-jump-to-maildir' (or 'j', by default), followed by the
designated shortcut character for the maildir.")

;; the headers view
(defgroup mu4e-headers nil
  "Settings for the headers view."
  :group 'mu4e)


(defcustom mu4e-headers-fields
  '( (:date          .  25)
     (:flags         .   6)
     (:from          .  22)
     (:subject       .  nil))
  "A list of header fields to show in the headers buffer, and their
  respective widths in characters. A width of `nil' means
  'unrestricted', and this is best reserved fo the rightmost (last)
  field. For the complete list of available headers, see
  `mu4e-header-names'"
  :type (list 'symbol)
  :group 'mu4e-headers)

(defcustom mu4e-headers-date-format "%x %X"
  "Date format to use in the headers view, in the format of
  `format-time-string'."
  :type  'string
  :group 'mu4e-headers)

(defcustom mu4e-headers-leave-behavior 'ask
  "What to do when user leaves the headers view (e.g. quits,
  refreshes or does a new search). Value is one of the following
  symbols:
- ask (ask the user whether to ignore the marks)
- apply (automatically apply the marks before doing anything else)
- ignore (automatically ignore the marks without asking)."
  :type 'symbol
  :group 'mu4e-headers)


(defcustom mu4e-headers-visible-lines 10
  "Number of lines to display in the header view when using the
horizontal split-view. This includes the header-line at the top,
and the mode-line."
  :type 'integer
  :group 'mu4e-headers)


(defcustom mu4e-headers-visible-columns 30
  "Number of columns to display for the header view when using the
vertical split-view."
  :type 'integer
  :group 'mu4e-headers)

;; the message view
(defgroup mu4e-view nil
  "Settings for the message view."
  :group 'mu4e)

(defcustom mu4e-view-fields
  '(:from :to :cc :subject :flags :date :maildir :attachments)
  "Header fields to display in the message view buffer. For the
complete list of available headers, see `mu4e-header-names'."
  :type (list 'symbol)
  :group 'mu4e-view)

(defcustom mu4e-view-date-format "%c"
  "Date format to use in the message view, in the format of
  `format-time-string'."
  :type 'string
  :group 'mu4e-view)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the HTML-version of the
e-mail message (if there is any."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-html2text-command nil
  "Shel command that converts HTML from stdin into plain text on
stdout. If this is not defined, the emacs `html2text' tool will be
used when faced with html-only message. If you use htmltext, it's
recommended you use \"html2text -utf8 -width 72\"."
  :type 'string
  :group 'mu4e-view
  :safe 'stringp)


(defcustom mu4e-view-wrap-lines nil
  "Whether to automatically wrap lines in the body of messages when
viewing them. Note that wrapping does not work well with all
messages, but you can always toggle between wrapped/unwrapped
display with `mu4e-view-toggle-wrap-lines (default keybinding: <w>)."
  :group 'mu4e-view)

(defcustom mu4e-view-wrap-lines nil
  "Whether to automatically wrap lines in the body of messages when
viewing them. Note that wrapping does not work well with all
messages, but you can always toggle between wrapped/unwrapped
display with `mu4e-view-toggle-wrap-lines (default keybinding: <w>)."
  :group 'mu4e-view)


(defcustom mu4e-view-hide-cited nil
  "Whether to automatically hide cited parts of messages (as
determined by the presence of '> ' at the beginning of the
line). Note that you can always toggle between hidden/unhidden
display with `mu4e-view-toggle-hide-cited (default keybinding:
<w>)."
  :group 'mu4e-view)


;; Composing / Sending messages
(defgroup mu4e-compose nil
  "Customizations for composing/sending messages."
  :group 'mu4e)

(defcustom mu4e-send-citation-prefix "> "
  "String to prefix cited message parts with."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-send-reply-prefix "Re: "
  "String to prefix the subject of replied messages with."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-send-forward-prefix "Fwd: "
  "String to prefix the subject of forwarded messages with."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-user-agent nil
  "The user-agent string; leave at `nil' for the default."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-sent-messages-behavior 'sent
  "Determines what mu4e does with sent messages - this is a symbol
which can be either:
'sent   --> move the sent message to the Sent-folder (`mu4e-sent-folder')
'trash  --> move the sent message to the Trash-folder (`mu4e-trash-folder')
'delete --> delete the sent message.
Note, when using GMail/IMAP, you should set this to either 'trash
or 'delete, since GMail already takes care of keeping copies in the
sent folder."
  :type 'symbol
  :safe 'symbolp
  :group 'mu4e-compose)


;; Faces

(defgroup mu4e-faces nil
  "Type faces (fonts) used in mu4e."
  :group 'mu4e
  :group 'faces)


(defface mu4e-unread-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for an unread message header."
  :group 'mu4e-faces)

(defface mu4e-moved-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for a message header that has been moved to some
folder (it's still visible in the search results, since we cannot
be sure it no longer matches)."
  :group 'mu4e-faces)

(defface mu4e-trashed-face
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for an message header in the trash folder."
  :group 'mu4e-faces)

(defface mu4e-draft-face
  '((t :inherit font-lock-string-face))
  "Face for a draft message header (i.e., a message with the draft
flag set)."
  :group 'mu4e-faces)

(defface mu4e-header-face
  '((t :inherit default))
  "Face for a header without any special flags."
  :group 'mu4e-faces)

(defface mu4e-header-title-face
  '((t :inherit font-lock-type-face))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-view-header-key-face
  '((t :inherit font-lock-builtin-face :bold t))
  "Face for a header title (such as \"Subject\") in the message
  view."
  :group 'mu4e-faces)

(defface mu4e-header-highlight-face
  '((t :inherit default :weight bold :underline t))
  "Face for the header at point."
  :group 'mu4e-faces)

(defface mu4e-view-header-value-face
  '((t :inherit font-lock-doc-face))
  "Face for a header value (such as \"Re: Hello!\") in the message
  view."
  :group 'mu4e-faces)

(defface mu4e-view-link-face
  '((t :inherit font-lock-type-face :underline t))
  "Face for showing URLs and attachments in the message view."
  :group 'mu4e-faces)

(defface mu4e-highlight-face
  '((t :inherit font-lock-pseudo-keyword-face :bold t))
  "Face for highlighting things."
  :group 'mu4e-faces)

(defface mu4e-title-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-view-url-number-face
  '((t :inherit font-lock-reference-face :bold t))
  "Face for the number tags for URLs."
  :group 'mu4e-faces)

(defface mu4e-view-attach-number-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for the number tags for attachments."
  :group 'mu4e-faces)

(defface mu4e-cited-1-face
  '((t :inherit font-lock-builtin-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'mu4e-faces)

(defface mu4e-cited-2-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'mu4e-faces)

(defface mu4e-cited-3-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'mu4e-faces)

(defface mu4e-cited-4-face
  '((t :inherit font-lock-pseudo-keyword-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'mu4e-faces)

(defface mu4e-view-footer-face
  '((t :inherit font-lock-comment-face))
  "Face for message footers (signatures)."
  :group 'mu4e-faces)

(defface mu4e-hdrs-marks-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the mark in the headers list."
  :group 'mu4e-faces)

(defface mu4e-system-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for system message (such as the footers for message
headers)."
  :group 'mu4e-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal variables / constants

(defconst mu4e-header-names
  '( (:attachments   .  "Attach")
     (:bcc           .  "Bcc")
     (:cc            .  "Cc")
     (:date          .  "Date")
     (:flags         .  "Flgs")
     (:from          .  "From")
     (:from-or-to    .  "From/To")
     (:maildir       .  "Maildir")
     (:path          .  "Path")
     (:subject       .  "Subject")
     (:to            .  "To"))
"A alist of all possible header fields; this is used in the UI (the
column headers in the header list, and the fields the message
view). Most fields should be self-explanatory. A special one is
`:from-or-to', which is equal to `:from' unless `:from' matches ,
in which case it will be equal to `:to'.)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time vars used in multiple places

;; headers
(defvar mu4e-last-expr nil "*internal* The most recent search expression.")
(defconst mu4e-hdrs-buffer-name "*mu4e-headers*"
  "*internal* Name of the buffer for message headers.")
(defvar mu4e-hdrs-buffer nil "*internal* Buffer for message headers")

;; view
(defconst mu4e-view-buffer-name "*mu4e-view*"
  "*internal* Name for the message view buffer")
(defvar mu4e-view-buffer nil "*internal* The view buffer.")
(defvar mu4e-current-msg nil
  "*internal* The plist describing the currently viewed message.")

(provide 'mu4e-vars)
