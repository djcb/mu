;;; mu4e-vars.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2016 Dirk-Jan C. Binnema

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
(require 'mu4e-meta)
(require 'message)

(defgroup mu4e nil
  "mu4e - mu for emacs"
  :group 'mail)

(defcustom mu4e-mu-home nil
  "Location of the mu homedir, or nil for the default."
  :group 'mu4e
  :type '(choice (const :tag "Default location" nil)
                 (directory :tag "Specify location"))
  :safe 'stringp)

(defcustom mu4e-mu-binary (executable-find "mu")
  "Name of the mu-binary to use.
If it cannot be found in your PATH, you can specify the full
path."
  :type 'file
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-maildir (expand-file-name "~/Maildir")
  "The file system path to your Maildir. Must not be a symbolic
link."
  :type 'directory
  :safe 'stringp
  :group 'mu4e)

(defcustom mu4e-get-mail-command "true"
  "Shell command to run to retrieve new mail.
Common values are \"offlineimap\", \"fetchmail\" or \"mbsync\", but
arbitrary shell-commands can be used.

When set to the literal string \"true\" (the default), the
command simply finishes succesfully (running the 'true' command)
without retrieving any mail. This can be useful when mail is
already retrieved in another way."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-index-update-error-warning t
  "Whether to display warnings when we the retrieval process (as
  per `mu4e-get-mail-command') finished with a non-zero exit code."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-update-error-continue t
  "Whether to continue with indexing when we the retrieval
  process (as per `mu4e-get-mail-command') finished with a non-zero
  exit code."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-update-in-background t
  "Whether to run the automatic mail retrieval in the
background."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-cleanup t
  "Whether to run a cleanup phase after indexing -- that is, see
if the is a message in the filesystem for each file in the
message store. Having this option as `t' ensures that no
non-existing mesages are shown but can also be quite slow with
large message stores."  :type 'boolean :group 'mu4e :safe
'booleanp)

(defcustom mu4e-index-lazy-check nil
  "Whether to only use a 'lazy check' to decide whether a message
needs (re)indexing or not. When this is set to `t', mu only uses
the directory timestamps to decide on whether it needs to check
the messages beneath it, which would miss messages that are
modified outside mu. On the other hand, it's significantly
faster."  :type 'boolean :group 'mu4e :safe 'booleanp)


(defcustom mu4e-update-interval nil
  "Number of seconds between automatic calls to retrieve mail and
update the database. If nil, don't update automatically. Note,
changes in `mu4e-update-interval' only take effect after restarting
mu4e."
  :type '(choice (const :tag "No automatic update" nil)
                 (integer :tag "Seconds"))
  :group 'mu4e
  :safe 'integerp)

(defvar mu4e-update-pre-hook nil
  "Hook run just *before* the mail-retrieval / database updating process starts.
 You can use this hook for example to `mu4e-get-mail-command' with
 some specific setting.")

(defcustom mu4e-hide-index-messages nil
  "Whether to hide the \"Indexing...\" messages, or any messages
relating to updated contacts."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-change-filenames-when-moving nil
  "When moving messages to different folders, normally mu/mu4e keep
the base filename the same (the flags-part of the filename may
change still). With this option set to non-nil, mu4e instead
changes the filename. This latter behavior works better with some
IMAP-synchronization programs such as mbsync; the default works
better with e.g. offlineimap."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-attachment-dir (expand-file-name "~/")
  "Default directory for saving attachments.
This can be either a string (a file system path), or a function
that takes a filename and the mime-type as arguments, and returns
the attachment dir. See Info node `(mu4e) Attachments' for details."
  :type 'directory
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-user-mail-address-list `(,user-mail-address)
  "List of e-mail addresses to consider 'my email addresses'.
I.e. addresses whose presence in an email imply that it is a
personal message. Note that the local part (the part before '@')
of e-mail addresses is case-sensitive, as per RFC531.  In
practice however, most of the mail systems do not distinguish
addresses based on the case, so the emails in this list will be
matched case-insensitively."
  :type '(repeat (string :tag "Address"))
  :group 'mu4e)

;; don't use the older vars anymore
(make-obsolete-variable 'mu4e-user-mail-address-regexp
  'mu4e-user-mail-address-list "0.9.9.x")

(make-obsolete-variable 'mu4e-my-email-addresses
  'mu4e-user-mail-address-list "0.9.9.x")

(defcustom mu4e-use-fancy-chars nil
  "Whether to use fancy (Unicode) characters for marks and
threads. You can customize the exact fancy characters used with
`mu4e-marks' and various `mu4e-headers-..-mark' and
`mu4e-headers..-prefix' variables."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-date-format-long "%c"
  "Date format to use in the message view, in the format of
  `format-time-string'."
  :type 'string
  :group 'mu4e)

(defvar mu4e-debug nil
  "When set to non-nil, log debug information to the *mu4e-log* buffer.")

(defcustom mu4e-bookmarks
  '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
     ("date:today..now"                  "Today's messages"     ?t)
     ("date:7d..now"                     "Last 7 days"          ?w)
     ("mime:image/*"                     "Messages with images" ?p))
  "A list of pre-defined queries.
These will show up in the main screen. Each of the list elements
is a three-element list of the form (QUERY DESCRIPTION KEY),
where QUERY is a string with a mu query, DESCRIPTION is a short
description of the query (this will show up in the UI), and KEY
is a shortcut key for the query."
  :type '(repeat (list (string :tag "Query")
		   (string :tag "Description")
		   character))
  :group 'mu4e)

(defvar mu4e-bookmarks
  `( ,(make-mu4e-bookmark
	:name  "Unread messages"
	:query "flag:unread AND NOT flag:trashed"
	:key ?u)
     ,(make-mu4e-bookmark
	:name "Today's messages"
	:query "date:today..now"
	:key ?t)
     ,(make-mu4e-bookmark
	:name "Last 7 days"
	:query "date:7d..now"
	:key ?w)
     ,(make-mu4e-bookmark
	:name "Messages with images"
	:query "mime:image/*"
	:key ?p))
  "A list of pre-defined queries. Each query is represented by a
mu4e-bookmark structure with parameters @t{:name} with the name
of the bookmark, @t{:query} with the query expression (a query
string or an s-expression that evaluates to query string) and a
@t{:key}, which is the shortcut-key for the query.

An older form of bookmark, a 3-item list with (QUERY DESCRIPTION
KEY) is still recognized as well, for backward-compatibility.")


(defcustom mu4e-split-view 'horizontal
  "How to show messages / headers.
A symbol which is either:
 * `horizontal':   split horizontally (headers on top)
 * `vertical':     split vertically (headers on the left).
 * anything else:  don't split (show either headers or messages,
                  not both)
Also see `mu4e-headers-visible-lines'
and `mu4e-headers-visible-columns'."
  :type '(choice (const :tag "Split horizontally" horizontal)
                 (const :tag "Split vertically" vertical)
                 (const :tag "Don't split" nil))
  :group 'mu4e-headers)

(defcustom mu4e-view-show-images nil
  "Whether to automatically display attached images in the message
view buffer."
  :type 'boolean
  :group 'mu4e-view)

(make-obsolete-variable 'mu4e-show-images
  'mu4e-view-show-images "0.9.9.x")

(defcustom mu4e-confirm-quit t
  "Whether to confirm to quit mu4e."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-cited-regexp  "^\\(\\([[:alpha:]]+\\)\\|\\( *\\)\\)\\(\\(>+ ?\\)+\\)"
  "Regular expression that determines whether a line is a
  citation. This recognizes lines starting with numbers of '>'
  and spaces as well as citations of the type \"John> ... \"."
  :type 'string
  :group 'mu4e)

(defcustom mu4e-completing-read-function 'ido-completing-read
  "Function to be used to receive input from the user with
completion. This is used to receive the name of the maildir
to switch to via `mu4e~headers-jump-to-maildir'.

Suggested possible values are:
 * `completing-read':      built-in completion method
 * `ido-completing-read':  dynamic completion within the minibuffer."
  :type 'function
  :options '(completing-read ido-completing-read)
  :group 'mu4e)

(defcustom mu4e-context-policy 'ask-if-none
  "The policy to determine the context when entering the mu4e main view.

If the value is `always-ask', ask the user unconditionally.

In all other cases, if any context matches (using its match
function), this context is used. Otherwise, if none of the
contexts match, we have the following choices:

- `pick-first': pick the first of the contexts available (ie. the default)
- `ask': ask the user
- `ask-if-none': ask if there is no context yet, otherwise leave it as it is
-  nil: return nil; leaves the current context as is.

Also see `mu4e-compose-context-policy'."
  :type '(choice
	   (const :tag "Always ask what context to use, even if one matches"
	     always-ask)
	   (const :tag "Ask if none of the contexts match" ask)
	   (const :tag "Ask when there's no context yet" ask-if-none)
	   (const :tag "Pick the first context if none match" pick-first)
	   (const :tag "Don't change the context when none match" nil))
  :group 'mu4e)


;; crypto
(defgroup mu4e-crypto nil
  "Crypto-related settings."
  :group 'mu4e)

(defcustom mu4e-auto-retrieve-keys nil
  "Attempt to automatically retrieve public keys when needed."
  :type 'boolean
  :group 'mu4e-crypto)

(defcustom mu4e-decryption-policy t
  "Policy for dealing with encrypted parts.
The setting is a symbol:
 * t:     try to decrypt automatically
 * `ask': ask before decrypting anything
 * nil:   don't try to decrypt anything."
  :type '(choice (const :tag "Try to decrypt automatically" t)
                 (const :tag "Ask before decrypting anything" ask)
                 (const :tag "Don't try to decrypt anything" nil))
  :group 'mu4e-crypto)

;; completion; we put them here rather than in mu4e-compose, as mu4e-utils needs
;; the variables.

(defgroup mu4e-compose nil
  "Message-composition related settings."
  :group 'mu4e)

;; address completion
(defcustom mu4e-compose-complete-addresses t
  "Whether to do auto-completion of e-mail addresses."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-personal nil
  "Whether to consider only 'personal' e-mail addresses,
i.e. addresses from messages where user was explicitly in one of
the address fields (this excludes mailing list messages). See
`mu4e-user-mail-address-list' and the mu-index manpage for details for
details (in particular, how to define your own e-mail addresses)."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-complete-only-after "2010-01-01"
  "Consider only contacts last seen after this date.
Date must be a string, in a format parseable by
`org-parse-time-string'. This excludes really old contacts.
Set to nil to not have any time-based restriction."
  :type 'string
  :group 'mu4e-compose)


;;; names and mail-addresses can be mapped onto their canonical
;;; counterpart.  use the customizeable function
;;; mu4e-canonical-contact-function to do that.  below the identity
;;; function for mapping a contact onto the canonical one.
(defun mu4e-contact-identity (contact)
  "This returns the name and the mail-address of a contact.
It is used as the identity function for converting contacts to
their canonical counterpart; useful as an example."
    (let ((name (plist-get contact :name))
          (mail (plist-get contact :mail)))
      (list :name name :mail mail)))

(defcustom mu4e-contact-rewrite-function nil
  "Either nil or a function to be used for when processing
contacts and rewrite them or remove them altogether.

If the function receives the contact as a list of the form
     (:name NAME :mail EMAIL ... other properties ... )
(other properties may be there as well)

The function should return either:
 - nil: remove this contact, or
- the rewritten cell, or
- the existing cell as-is

For rewriting, it is recommended to use `plist-put' to set the
changed parameters, so the other properties stay in place. Those
are needed for sorting the contacts."
  :type 'function
  :group 'mu4e-compose)


(defcustom mu4e-compose-complete-ignore-address-regexp "no-?reply"
  "Ignore any e-mail addresses for completion if they match this regexp."
  :type 'string
  :group 'mu4e-compose)

(defcustom mu4e-compose-reply-to-address nil
  "The Reply-To address (if this, for some reason, is not equal to
the From: address.)"
  :type 'string
  :group 'mu4e-compose)


;; backward compatibility
(make-obsolete-variable 'mu4e-reply-to-address 'mu4e-compose-reply-to-address
  "v0.9.9")

(defcustom mu4e-compose-keep-self-cc nil
  "Non-nil means your e-mail address is kept on the CC list when
replying to messages."
  :type 'boolean
  :group 'mu4e-compose)

(defvar mu4e-compose-parent-message nil
  "The parent message plist.
This is the message being replied to, forwarded or edited; used
in `mu4e-compose-pre-hook'. For new messages, it is nil.")

;; Folders
(defgroup mu4e-folders nil
  "Special folders."
  :group 'mu4e)

(defcustom mu4e-drafts-folder "/drafts"
  "Your folder for draft messages, relative to `mu4e-maildir'.
e.g. \"/drafts\". Instead of a string, may also be a function that
takes a message (a msg plist, see `mu4e-message-get-field'), and
returns a folder.  Note, the message parameter refers to the
original message being replied to / being forwarded / re-edited and
is nil otherwise. `mu4e-drafts-folder' is only evaluated once."
  :type '(choice
	   (string :tag "Folder name")
	   (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-refile-folder "/archive"
  "Your folder for refiling messages, relative to `mu4e-maildir',
e.g. \"/Archive\". Instead of a string, may also be a function that
takes a message (a msg plist, see `mu4e-message-get-field'), and
returns a folder. Note that the message parameter refers to the
message-at-point."
  :type '(choice
	   (string :tag "Folder name")
	   (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-sent-folder "/sent"
  "Your folder for sent messages, relative to `mu4e-maildir',
e.g. \"/Sent Items\". Instead of a string, may also be a function
that takes a message (a msg plist, see `mu4e-message-get-field'),
and returns a folder.  Note that the message parameter refers to
the original message being replied to / being forwarded /
re-edited, and is nil otherwise."
  :type '(choice
	   (string :tag "Folder name")
	   (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-trash-folder "/trash"
  "Your folder for trashed messages, relative to `mu4e-maildir',
e.g. \"/trash\". Instead of a string, may also be a function that
takes a message (a msg plist, see `mu4e-message-get-field'), and
returns a folder.  When using `mu4e-trash-folder' in the headers
view (when marking messages for trash). Note that the message
parameter refers to the message-at-point. When using it when
composing a message (see `mu4e-sent-messages-behavior'), this
refers to the original message being replied to / being forwarded /
re-edited, and is nil otherwise."
  :type '(choice
	   (string :tag "Folder name")
	   (function :tag "Function return folder name"))
  :group 'mu4e-folders)


(defcustom mu4e-maildir-shortcuts nil
  "A list of maildir shortcuts. This makes it possible to quickly
go to a particular maildir (folder), or quickly moving messages to
them (e.g., for archiving or refiling). The list contains elements
of the form (maildir . shortcut), where MAILDIR is a maildir (such
as \"/archive/\"), and shortcut is a single character.

You can use these shortcuts in the headers and view buffers, for
example with `mu4e-mark-for-move-quick' (or 'm', by default) or
`mu4e-jump-to-maildir' (or 'j', by default), followed by the
designated shortcut character for the maildir.

Unlike in search queries, folder names with spaces in them must NOT
be quoted, since mu4e does this automatically for you."
  :type '(repeat (cons (string :tag "Maildir") character))
  :group 'mu4e-folders)

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
  "Face for a message header that has been moved to some folder.
\(It's still visible in the search results, since we cannot
be sure it no longer matches)."
  :group 'mu4e-faces)

(defface mu4e-trashed-face
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for an message header in the trash folder."
  :group 'mu4e-faces)

(defface mu4e-draft-face
  '((t :inherit font-lock-string-face))
  "Face for a draft message header
I.e. a message with the draft flag set."
  :group 'mu4e-faces)

(defface mu4e-flagged-face
  '((t :inherit font-lock-constant-face :bold t))
  "Face for a flagged message header."
  :group 'mu4e-faces)

(defface mu4e-replied-face
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face for a replied message header."
  :group 'mu4e-faces)

(defface mu4e-forwarded-face
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face for a passed (forwarded) message header."
  :group 'mu4e-faces)

(defface mu4e-header-face
  '((t :inherit default))
  "Face for a header without any special flags."
  :group 'mu4e-faces)

(defface mu4e-header-title-face
  '((t :inherit font-lock-type-face))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-header-highlight-face
  '((t :inherit region :weight bold :underline t))
  "Face for the header at point."
  :group 'mu4e-faces)

(defface mu4e-header-marks-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the mark in the headers list."
  :group 'mu4e-faces)

(defface mu4e-header-key-face
  '((t :inherit message-header-name :bold t))
  "Face for a header key (such as \"Foo\" in \"Subject:\ Foo\")."
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
  '((t :inherit font-lock-type-face :bold t))
  "Face for a header title in the headers view."
  :group 'mu4e-faces)

(defface mu4e-context-face
  '((t :inherit mu4e-title-face :bold t))
  "Face for displaying the context in the modeline."
  :group 'mu4e-faces)

(defface mu4e-modeline-face
  '((t :inherit font-lock-string-face :bold t))
  "Face for the query in the mode-line."
  :group 'mu4e-faces)

(defface mu4e-view-body-face
  '((t :inherit default))
  "Face for the body in the message-view."
  :group 'mu4e-faces)

(defface mu4e-footer-face
  '((t :inherit font-lock-comment-face))
  "Face for message footers (signatures)."
  :group 'mu4e-faces)

(defface mu4e-url-number-face
  '((t :inherit font-lock-constant-face :bold t))
  "Face for the number tags for URLs."
  :group 'mu4e-faces)

(defface mu4e-attach-number-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for the number tags for attachments."
  :group 'mu4e-faces)

(defface mu4e-cited-1-face
  '((t :inherit font-lock-builtin-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'mu4e-faces)

(defface mu4e-cited-2-face
  '((t :inherit font-lock-preprocessor-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'mu4e-faces)

(defface mu4e-cited-3-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'mu4e-faces)

(defface mu4e-cited-4-face
  '((t :inherit font-lock-keyword-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'mu4e-faces)

(defface mu4e-cited-5-face
  '((t :inherit font-lock-comment-face :bold nil :italic t))
  "Face for cited message parts (level 5)."
  :group 'mu4e-faces)

(defface mu4e-cited-6-face
  '((t :inherit font-lock-comment-delimiter-face :bold nil :italic t))
  "Face for cited message parts (level 6)."
  :group 'mu4e-faces)

(defface mu4e-cited-7-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 7)."
  :group 'mu4e-faces)

(defface mu4e-system-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for system message (such as the footers for message headers)."
  :group 'mu4e-faces)

(defface mu4e-ok-face
  '((t :inherit font-lock-comment-face :bold t :slant normal))
  "Face for things that are okay."
  :group 'mu4e-faces)

(defface mu4e-warning-face
  '((t :inherit font-lock-warning-face :bold t :slant normal))
  "Face for warnings / error."
  :group 'mu4e-faces)

(defface mu4e-compose-separator-face
  '((t :inherit message-separator :slant italic))
  "Face for the separator between headers / message in
mu4e-compose-mode."
  :group 'mu4e-faces)

(defface mu4e-compose-header-face
  '((t :inherit message-separator :slant italic))
  "Face for the separator between headers / message in
mu4e-compose-mode."
  :group 'mu4e-faces)

(defface mu4e-region-code
    '((t (:background "DarkSlateGray")))
  "Face for highlighting marked region in mu4e-view buffer."
  :group 'mu4e-faces)

;; headers info
(defconst mu4e-header-info
  '( (:attachments .
       ( :name "Attachments"
	 :shortname "Atts"
	 :help "Message attachments"
	 :require-full t
	 :sortable nil))
     (:bcc .
       ( :name "Bcc"
	 :shortname "Bcc"
	 :help "Blind Carbon-Copy recipients for the message"
	 :sortable t))
     (:cc .
       ( :name "Cc"
	 :shortname "Cc"
	 :help "Carbon-Copy recipients for the message"
	 :sortable t))
     (:date .
       ( :name "Date"
	 :shortname "Date"
	 :help "Date/time when the message was written"
	 :sortable t))
     (:human-date .
       ( :name "Date"
	 :shortname "Date"
	 :help "Date/time when the message was written."
	 :sortable :date))
     (:flags .
       ( :name "Flags"
	 :shortname "Flgs"
	 :help "Flags for the message"
	 :sortable nil))
     (:from .
       ( :name "From"
	 :shortname "From"
	 :help "The sender of the message"
	 :sortable t))
     (:from-or-to .
       ( :name "From/To"
	 :shortname "From/To"
	 :help "Sender of the message if it's not me; otherwise the recipient"
	 :sortable nil))
     (:maildir .
       ( :name "Maildir"
	 :shortname "Maildir"
	 :help "Maildir for this message"
	 :sortable t))
     (:mailing-list .
       ( :name "List"
	 :shortname "List"
	 :help "Mailing list for this message"
	 :sortable nil))
     (:message-id .
       ( :name "Message-Id"
	 :shortname "MsgID"
	 :help "Message-Id for this message"
	 :sortable nil))
     (:path .
       ( :name "Path"
	 :shortname "Path"
	 :help "Full filesystem path to the message"
	 :sortable t))
     (:signature .
       ( :name "Signature"
	 :shortname "Sgn"
	 :help "Check for the cryptographic signature"
	 :require-full t
	 :sortable nil))
     (:decryption .
       ( :name "Decryption"
	 :shortname "Dec"
	 :help "Check the cryptographic decryption status"
	 :require-full t
	 :sortable nil))
     (:size .
       ( :name "Size"
	 :shortname "Size"
	 :help "Size of the message"
	 :sortable t))
     (:subject .
       ( :name "Subject"
	 :shortname "Subject"
	 :help "Subject of the message"
	 :sortable t))
     (:tags .
       ( :name "Tags"
	 :shortname "Tags"
	 :help "Tags for the message"
	 :sortable nil))
     (:thread-subject .
       ( :name "Subject"
	 :shortname "Subject"
	 :help "Subject of the thread"
	 :sortable :subject))
     (:to .
       ( :name "To"
	 :shortname "To"
	 :help "Recipient of the message"
	 :sortable t))
     (:user-agent .
       ( :name "User-Agent"
	 :shortname "UA"
	 :help "Program used for writing this message"
	 :require-full t
	 :sortable t)))
  "An alist of all possible header fields and information about them.
This is used in the user-interface (the column headers in the header list, and
the fields the message view).

Most fields should be self-explanatory. A special one is
`:from-or-to', which is equal to `:from' unless `:from' matches one
of the addresses in `mu4e-user-mail-address-list', in which case it
will be equal to `:to'.

Furthermore, the property `:sortable' determines whether we can
sort by this field.  This can be either a boolean (nil or t), or a
symbol for /another/ field. For example, the `:human-date' field
uses `:date' for that.

Fields with which have the property `:require-full' set to
non-nil require a full message; in practice this means that you
cannot use such fieds as part of `mu4e-headers-fields', but only
in `mu4e-view-fields.'

Note, `:sortable' is not supported for custom header fields.")


(defvar mu4e-header-info-custom
  '( (:recipnum .
       ( :name "Number of recipients"
	 :shortname "Recip#"
	 :help "Number of recipients for this message"
	 :function
	 (lambda (msg)
	   (format "%d"
	     (+ (length (mu4e-message-field msg :to))
	       (length (mu4e-message-field msg :cc))))))))
"A list of custom (user-defined) headers. The format is similar
to `mu4e-header-info', but adds a :function property, which
should point to a function that takes a message p-list as
argument, and returns a string. See the default value of
`mu4e-header-info-custom for an example.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time vars used in multiple places

;; headers
(defconst mu4e~headers-buffer-name "*mu4e-headers*"
  "Name of the buffer for message headers.")
(defvar mu4e~headers-buffer nil "Buffer for message headers.")
; view
(defconst mu4e~view-buffer-name "*mu4e-view*"
  "Name for the message view buffer.")

(defconst mu4e~view-embedded-buffer-name " *mu4e-embedded-view*"
  "Name for the embedded message view buffer.")

(defvar mu4e~view-buffer nil "The view buffer.")

(defvar mu4e~view-msg nil "The message being viewed in view mode.")

(defvar mu4e~view-headers-buffer nil
  "The headers buffer connected to this view.")

(defvar mu4e~contacts nil
  "Hash of that maps contacts (ie. 'name <e-mail>') to an integer
with their sort order. We need to keep this information around to
quickly re-sort subsets of the contacts in the completions function in
mu4e-compose.")

(defvar mu4e~server-props nil
  "Properties we receive from the mu4e server process.
\(in the 'pong-handler').")

(defvar mu4e~headers-last-query nil
  "The present (most recent) query.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; our handlers funcs
;; these handler funcs define what happens when we receive a certain message
;; from the server
(defun mu4e~default-handler (&rest args)
  "*internal* Dummy handler function."
  (error "Not handled: %S" args))

(defvar mu4e-error-func 'mu4e~default-handler
  "A function called for each error returned from the server
process; the function is passed an error plist as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-update-func 'mu4e~default-handler
  "A function called for each :update sexp returned from the server
process; the function is passed a msg sexp as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-remove-func  'mu4e~default-handler
  "A function called for each :remove sexp returned from the server
process, when some message has been deleted. The function is passed
the docid of the removed message.")

(defvar mu4e-sent-func  'mu4e~default-handler
  "A function called for each :sent sexp returned from the server
process, when some message has been sent. The function is passed
the docid and the draft-path of the sent message.")

(defvar mu4e-view-func  'mu4e~default-handler
  "A function called for each single message sexp returned from the
server process. The function is passed a message sexp as
argument. See `mu4e~proc-filter' for the format.")

(defvar mu4e-header-func  'mu4e~default-handler
  "A function called for each message returned from the server
process; the function is passed a msg plist as argument. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-found-func  'mu4e~default-handler
  "A function called for when we received a :found sexp after the
headers have returns, to report on the number of matches. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-erase-func 'mu4e~default-handler
  "A function called for when we received an :erase sexp after the
headers have returns, to clear the current headers buffer. See
`mu4e~proc-filter' for the format.")

(defvar mu4e-compose-func  'mu4e~default-handler
  "A function called for each message returned from the server
process that is used as basis for composing a new message (ie.,
either a reply or a forward); the function is passed msg and a
symbol (either reply or forward). See `mu4e~proc-filter' for the
format of <msg-plist>.")

(defvar mu4e-info-func  'mu4e~default-handler
  "A function called for each (:info type ....) sexp received from
the server process.")

(defvar mu4e-pong-func 'mu4e~default-handler
  "A function called for each (:pong type ....) sexp received from
the server process.")

(defvar mu4e-contacts-func 'mu4e~default-handler
  "A function called for each (:contacts (<list-of-contacts>) sexp
received from the server process.")

(defvar mu4e-temp-func 'mu4e~default-handler
  "A function called for each (:temp <file> <cookie>) sexp received
from the server process.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mu4e-vars)
;;; End of mu4e-vars.el
