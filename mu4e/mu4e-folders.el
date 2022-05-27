;;; mu4e-folders.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Dirk-Jan C. Binnema

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
For instance, \"/drafts\". Instead of a string, may also be a
function that takes a message (a msg plist, see
`mu4e-message-field'), and returns a folder. Note, the message
parameter refers to the original message being replied to / being
forwarded / re-edited and is nil otherwise. `mu4e-drafts-folder'
is only evaluated once."
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
`:hide'  - if t, the shortcut is hidden from the main-view and
speedbar.
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
  :type '(repeat (cons (string :tag "Maildir") character))
  :version "1.3.9"
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
mime-type are nill."
  :type 'directory
  :group 'mu4e-folders
  :safe 'stringp)


(defun mu4e-maildir-shortcuts ()
  "Get `mu4e-maildir-shortcuts' in the (new) format.
Converts from the old format if needed."
  (seq-map (lambda (item) ;; convert from old format?
             (if (and (consp item) (not (consp (cdr item))))
                 `(:maildir  ,(car item) :key ,(cdr item))
               item))
           mu4e-maildir-shortcuts))

(defun mu4e--maildirs-with-query ()
  "Like `mu4e-maildir-shortcuts', but with :query populated.
This is compatibile with `mu4e-bookmarks'."
  (seq-map
   (lambda (item)
     (let* ((maildir (plist-get item :maildir))
	    (item (plist-put item :name maildir))
	    (item (plist-put item :query (format "maildir:\"%s\"" maildir))))
       item)) ;; we don't need ":maildir", but it's harmless.
   (mu4e-maildir-shortcuts)))

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
  "Get the sent folder, optionallly based on MSG.
See `mu4e-drafts-folder'." (mu4e--get-folder 'mu4e-drafts-folder msg))

(defun mu4e-get-refile-folder (&optional msg)
  "Get the folder for refiling, optionallly based on MSG.
See `mu4e-refile-folder'." (mu4e--get-folder 'mu4e-refile-folder msg))

(defun mu4e-get-sent-folder (&optional msg)
  "Get the sent folder, optionallly based on MSG.
See `mu4e-sent-folder'." (mu4e--get-folder 'mu4e-sent-folder msg))

(defun mu4e-get-trash-folder (&optional msg)
  "Get the sent folder, optionallly based on MSG.
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
        (concat path "/../.."))))))

(defun mu4e-create-maildir-maybe (dir)
  "Offer to create maildir DIR if it does not exist yet.
Return t if the dir already existed, or an attempt has been made to
create it -- we cannot be sure creation succeeded here, since this
is done asynchronously. Otherwise, return nil. NOte, DIR has to be
an absolute path."
  (if (and (file-exists-p dir) (not (file-directory-p dir)))
      (mu4e-error "File %s exists, but is not a directory" dir))
  (cond
   ((file-directory-p dir) t)
   ((yes-or-no-p (mu4e-format "%s does not exist yet. Create now?" dir))
    (mu4e--server-mkdir dir) t)
   (t nil)))

(defun mu4e~get-maildirs-1 (path mdir)
  "Get maildirs for MDIR under PATH.
Do so recursively and produce a list of relative paths."
  (let ((dirs)
        (dentries
         (ignore-errors
           (directory-files-and-attributes
            (concat path mdir) nil
            "^[^.]\\|\\.[^.][^.]" t))))
    (dolist (dentry dentries)
      (when (and (booleanp (cadr dentry)) (cadr dentry))
        (if (file-accessible-directory-p
             (concat (mu4e-root-maildir) "/" mdir "/" (car dentry) "/cur"))
            (setq dirs (cons (concat mdir (car dentry)) dirs)))
        (unless (member (car dentry) '("cur" "new" "tmp"))
          (setq dirs
		(append dirs
			(mu4e~get-maildirs-1 path
                                             (concat mdir
						     (car dentry) "/")))))))
    dirs))

(defvar mu4e-cache-maildir-list nil
  "Whether to cache the list of maildirs.
Set it to t if you find that generating the list on the fly is
too slow. If you do, you can set `mu4e-maildir-list' to nil to
force regenerating the cache the next time `mu4e-get-maildirs'
gets called.")

(defvar mu4e-maildir-list nil
  "Cached list of maildirs.")

(defun mu4e-get-maildirs ()
  "Get maildirs under `mu4e-maildir'.
Do so recursively, and produce a list of relative paths (ie.,
/archive, /sent etc.). Most of the work is done in
`mu4e~get-maildirs-1'. Note, these results are /cached/ if
`mu4e-cache-maildir-list' is customized to non-nil. In that case,
the list of maildirs will not change until you restart mu4e."
  (unless (and mu4e-maildir-list mu4e-cache-maildir-list)
    (setq mu4e-maildir-list
          (sort
           (append
            (when (file-accessible-directory-p
                   (concat (mu4e-root-maildir) "/cur")) '("/"))
            (mu4e~get-maildirs-1 (mu4e-root-maildir) "/"))
           (lambda (s1 s2) (string< (downcase s1) (downcase s2))))))
  mu4e-maildir-list)

(defun mu4e-ask-maildir (prompt)
  "Ask the user for a shortcut (using PROMPT).
As per (mu4e-maildir-shortcuts), then return the corresponding
folder name. If the special shortcut \"o\" (for _o_ther) is used,
or if (mu4e-maildir-shortcuts) evaluates to nil, let user choose
from all maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (if (not (mu4e-maildir-shortcuts))
        (substring-no-properties
         (funcall mu4e-completing-read-function prompt (mu4e-get-maildirs)))
      (let* ((mlist (append (mu4e-maildir-shortcuts)
                            '((:maildir "ther"  :key ?o))))
             (fnames
              (mapconcat
               (lambda (item)
                 (concat
                  "["
                  (propertize (make-string 1 (plist-get item :key))
                              'face 'mu4e-highlight-face)
                  "]"
                  (plist-get item :maildir)))
               mlist ", "))
             (kar (read-char (concat prompt fnames))))
        (if (member kar '(?/ ?o)) ;; user chose 'other'?
            (substring-no-properties
             (funcall mu4e-completing-read-function prompt
                      (mu4e-get-maildirs) nil nil "/"))
          (or (plist-get
               (seq-find (lambda (item) (= kar (plist-get item :key)))
                         (mu4e-maildir-shortcuts)) :maildir)
              (mu4e-warn "Unknown shortcut '%c'" kar)))))))

(defun mu4e-ask-maildir-check-exists (prompt)
  "Like `mu4e-ask-maildir', PROMPT for existence of the maildir.
Offer to create it if it does not exist yet."
  (let* ((mdir (mu4e-ask-maildir prompt))
         (fullpath (concat (mu4e-root-maildir) mdir)))
    (unless (file-directory-p fullpath)
      (and (yes-or-no-p
            (mu4e-format "%s does not exist. Create now?" fullpath))
           (mu4e--server-mkdir fullpath)))
    mdir))

;; mu4e-attachment-dir is either a string or a function that takes a
;; filename and the mime-type as argument, either (or both) which can
;; be nil

(defun mu4e~get-attachment-dir (&optional fname mimetype)
  "Get the directory for saving attachments from `mu4e-attachment-dir'.
This is optionally based on the file-name FNAME and its MIMETYPE."
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
