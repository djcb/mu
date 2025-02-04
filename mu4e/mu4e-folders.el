;;; mu4e-folders.el --- Maildirs & folders -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Dirk-Jan C. Binnema

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

;; Dealing with maildirs & folders

;;; Code:
(require 'mu4e-helpers)
(require 'mu4e-context)
(require 'mu4e-server)

;;; Customization
(defgroup mu4e-folders nil
  "Special folders."
  :group 'mu4e)

(defcustom mu4e-drafts-folder "/drafts"
  "Folder for draft messages, relative to the root maildir.
For instance, \"/drafts\".

Instead of a string, may also be a function that takes a
message (a msg plist, see `mu4e-message-field'), and returns a
folder. Note, the message parameter refers to the original
message being replied to / being forwarded / re-edited and is nil
otherwise. `mu4e-drafts-folder' is only evaluated once.

The form of draft messages is not necessarily compatible with
other e-mail programs, e.g. when it involves attachments and the
like.
"
  :type '(choice
          (string :tag "Folder name")
          (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-refile-folder "/archive"
  "Folder for refiling messages, relative to the root maildir.
For instance \"/Archive\". Instead of a string, may also be a
function that takes a message (a msg plist, see
`mu4e-message-field'), and returns a folder. Note that the
message parameter refers to the message-at-point."
  :type '(choice
          (string :tag "Folder name")
          (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-sent-folder "/sent"
  "Folder for sent messages, relative to the root maildir.
For instance, \"/Sent Items\". Instead of a string, may also be a
function that takes a message (a msg plist, see
`mu4e-message-field'), and returns a folder. Note that the
message parameter refers to the original message being replied to
/ being forwarded / re-edited, and is nil otherwise."
  :type '(choice
          (string :tag "Folder name")
          (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-trash-folder "/trash"
  "Folder for trashed messages, relative to the root maildir.
For instance, \"/trash\". Instead of a string, may also be a
function that takes a message (a msg plist, see
`mu4e-message-field'), and returns a folder. When using
`mu4e-trash-folder' in the headers view (when marking messages
for trash). Note that the message parameter refers to the
message-at-point. When using it when composing a message (see
`mu4e-sent-messages-behavior'), this refers to the original
message being replied to / being forwarded / re-edited, and is
nil otherwise."
  :type '(choice
          (string :tag "Folder name")
          (function :tag "Function return folder name"))
  :group 'mu4e-folders)

(defcustom mu4e-maildir-shortcuts nil
  "A list of maildir shortcuts.
This makes it possible to quickly go to a particular
maildir (folder), or quickly moving messages to them (e.g., for
archiving or refiling).

Each of the list elements is a plist with at least:
`:maildir'  - the maildir for the shortcut (e.g. \"/archive\")
`:key'      - the shortcut key.

Optionally, you can add the following:
`:name' - name of the maildir to be displayed in main-view.
`:hide'  - if t, the shortcut is hidden from the main-view.
`:hide-unread' - do not show the counts of unread/total number
 of matches for the maildir in the main-view, and is implied
from `:hide'.

For backward compatibility, an older form is recognized as well:

   (maildir . key), where MAILDIR is a maildir (such as
\"/archive/\"), and key is a single character.

You can use these shortcuts in the headers and view buffers, for
example with `mu4e-mark-for-move-quick' (or \"m\", by default) or
`mu4e-jump-to-maildir' (or \"j\", by default), followed by the
designated shortcut character for the maildir.

Unlike in search queries, folder names with spaces in them must
NOT be quoted, since mu4e does this for you."
  :type '(choice
          (alist :key-type (string :tag "Maildir")
                 :value-type character
                 :tag "Alist (old format)")
          (repeat (plist
                   :key-type (choice (const :tag "Maildir" :maildir)
                                     (const :tag "Shortcut" :key)
                                     (const :tag "Name of maildir" :name)
                                     (const :tag "Hide from main view" :hide)
                                     (const :tag "Do not count" :hide-unread))
                   :tag "Plist (new format)")))
  :version "1.3.9"
  :group 'mu4e-folders)

(defcustom mu4e-maildir-initial-input "/"
  "Initial input for `mu4e-completing-completing-read' function."
  :type 'string
  :group 'mu4e-folders)

(defcustom mu4e-maildir-info-delimiter
  (if (member system-type '(ms-dos windows-nt cygwin))
      ";" ":")
  "Separator character between message identifier and flags.
It defaults to ':' on most platforms, except on Windows, where it
is not allowed and we use ';' for compatibility with mbsync,
offlineimap and other programs."
  :type 'string
  :group 'mu4e-folders)

(defcustom mu4e-attachment-dir (expand-file-name "~/")
  "Default directory for attaching and saving attachments.

This can be either a string (a file system path), or a function
that takes a filename and the mime-type as arguments, and returns
the attachment dir. See Info node `(mu4e) Attachments' for
details.

When this called for composing a message, both filename and
mime-type are nil."
  :type 'directory
  :group 'mu4e-folders
  :safe 'stringp)

(defvar mu4e-maildir-list nil
  "Cached list of maildirs.")


(defun mu4e-maildir-shortcuts ()
  "Get `mu4e-maildir-shortcuts' in the (new) format.
Converts from the old format if needed."
  (seq-map (lambda (item) ;; convert from old format?
             (if (and (consp item) (not (consp (cdr item))))
                 `(:maildir  ,(car item) :key ,(cdr item))
               item))
           mu4e-maildir-shortcuts))

(declare-function mu4e-query-items "mu4e-query-items")
(declare-function mu4e--query-item-display-short-counts "mu4e-query-items")

(defun mu4e--query-item-for-maildir-shortcut (mds)
  "Find the corresponding query-item for some maildir shortcut MDS.
This is based on their query. Return nil if not found."
  (seq-find (lambda (qitem)
              (equal (plist-get qitem :maildir) (plist-get mds :maildir)))
            (mu4e-query-items 'maildirs)))

;; the standard folders can be functions too
(defun mu4e--get-folder (foldervar msg)
  "Within the mu-context of MSG, get message folder FOLDERVAR.
If FOLDER is a string, return it, if it is a function, evaluate
this function with MSG as parameter which may be nil, and return
the result."
  (unless (member foldervar
                  '(mu4e-sent-folder mu4e-drafts-folder
                                     mu4e-trash-folder mu4e-refile-folder))
    (mu4e-error "Folder must be one of mu4e-(sent|drafts|trash|refile)-folder"))
  ;; get the value with the vars for the relevants context let-bound
  (with-mu4e-context-vars (mu4e-context-determine msg nil)
      (let* ((folder (symbol-value foldervar))
             (val
              (cond
               ((stringp   folder) folder)
               ((functionp folder) (funcall folder msg))
               (t (mu4e-error "Unsupported type for %S" folder)))))
        (or val (mu4e-error "%S evaluates to nil" foldervar)))))

(defun mu4e-get-drafts-folder (&optional msg)
  "Get the drafts folder, optionally based on MSG.
See `mu4e-drafts-folder'." (mu4e--get-folder 'mu4e-drafts-folder msg))

(defun mu4e-get-refile-folder (&optional msg)
  "Get the folder for refiling, optionally based on MSG.
See `mu4e-refile-folder'." (mu4e--get-folder 'mu4e-refile-folder msg))

(defun mu4e-get-sent-folder (&optional msg)
  "Get the sent folder, optionally based on MSG.
See `mu4e-sent-folder'." (mu4e--get-folder 'mu4e-sent-folder msg))

(defun mu4e-get-trash-folder (&optional msg)
  "Get the trash folder, optionally based on MSG.
See `mu4e-trash-folder'." (mu4e--get-folder 'mu4e-trash-folder msg))

;;; Maildirs
(defun mu4e--guess-maildir (path)
  "Guess the maildir for PATH, or nil if cannot find it."
  (let ((idx (string-match (mu4e-root-maildir) path)))
    (when (and idx (zerop idx))
      (replace-regexp-in-string
       (mu4e-root-maildir)
       ""
       (expand-file-name
        (mu4e-join-paths path ".." ".."))))))

(defun mu4e-create-maildir-maybe (dir)
  "Offer to create maildir DIR if it does not exist yet.
Return t if it already exists or (after asking) an attempt has been
to create it; otherwise return nil."
  (let ((seems-to-exist (file-directory-p dir)))
    (when (or seems-to-exist
              (yes-or-no-p (mu4e-format "%s does not exist yet. Create now?" dir)))
      ;; even when the maildir already seems to exist, call mkdir for a deepe
      ;; check. However only get an update when the maildir is totally new.
      (mu4e--server-mkdir dir (not seems-to-exist))
      t)))

(defun mu4e-get-maildirs ()
  "Get maildirs under `mu4e-maildir'."
  mu4e-maildir-list)

(defun mu4e-ask-maildir (prompt)
  "Ask the user for a maildir (using PROMPT).

If the special shortcut \"o\" (for _o_ther) is used, or
if (mu4e-maildir-shortcuts) evaluates to nil, let user choose
from all maildirs under `mu4e-maildir'.

The names of the maildirs are displayed in the minibuffer,
suffixed with the short version of the unread counts, as per
`mu4e--query-item-display-short-counts'."
  (let* ((options
         (seq-map
          (lambda (md)
            (let* ((qitem (mu4e--query-item-for-maildir-shortcut md))
                   (unreads (mu4e--query-item-display-short-counts qitem)))
              (cons
               (format "%c%s%s"
                       (plist-get md :key)
                       (or (plist-get md :name)
                           (plist-get md :maildir))
                       unreads)
               (plist-get md :maildir))))
                  (mu4e-filter-single-key (mu4e-maildir-shortcuts))))
        (response
         (if (not options)
             'other
           (mu4e-read-option prompt
                             (append options
                                     '(("oOther..." . other)))))))
    (substring-no-properties
     (if (eq response 'other)
         (progn
           (funcall mu4e-completing-read-function prompt
                    (mu4e-get-maildirs) nil nil
                    mu4e-maildir-initial-input))
       response))))

(defun mu4e-ask-maildir-check-exists (prompt)
  "Like `mu4e-ask-maildir', PROMPT for existence of the maildir.
Offer to create it if it does not exist yet."
  (let* ((mdir (mu4e-ask-maildir prompt))
         (fullpath (mu4e-join-paths (mu4e-root-maildir) mdir)))
    (unless (file-directory-p fullpath)
      (and (yes-or-no-p
            (mu4e-format "%s does not exist. Create now?" fullpath))
           (mu4e--server-mkdir fullpath)))
    mdir))

;; mu4e-attachment-dir is either a string or a function that takes a
;; filename and the mime-type as argument, either (or both) which can
;; be nil

(defun mu4e-determine-attachment-dir (&optional fname mimetype)
  "Get the target-directory for attachments.

This is based on the variable `mu4e-attachment-dir', which is either:
- if is a string, used it as-is
-  a function taking two string parameters, both of which can be nil:
    (1) FNAME, a filename or a URL
    (2) MIMETYPE, a mime-type (such as \"text/plain\"."
  (let ((dir
         (cond
          ((stringp mu4e-attachment-dir)
           mu4e-attachment-dir)
          ((functionp mu4e-attachment-dir)
           (funcall mu4e-attachment-dir fname mimetype))
          (t
           (mu4e-error "Unsupported type for mu4e-attachment-dir" )))))
    (if dir
        (expand-file-name dir)
      (mu4e-error "Mu4e-attachment-dir evaluates to nil"))))

(provide 'mu4e-folders)
;;; mu4e-folders.el ends here
