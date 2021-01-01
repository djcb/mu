;;; mu4e-utils.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2011-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

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

;; Utility functions used in the mu4e

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'cl-seq nil 'noerror)
(require 'mu4e-vars)
(require 'mu4e-message)
(require 'mu4e-meta)
(require 'mu4e-lists)
(require 'doc-view)

;; keep the byte-compiler happy
(declare-function mu4e~proc-mkdir     "mu4e-proc")
(declare-function mu4e~proc-ping      "mu4e-proc")
(declare-function mu4e~proc-contacts  "mu4e-proc")
(declare-function mu4e~proc-kill      "mu4e-proc")
(declare-function mu4e~proc-index     "mu4e-proc")
(declare-function mu4e~proc-add       "mu4e-proc")
(declare-function mu4e~proc-mkdir     "mu4e-proc")
(declare-function mu4e~proc-running-p "mu4e-proc")

(declare-function mu4e-message-field-at-point     "mu4e-proc")
(declare-function mu4e~proc-running-p "mu4e-proc")

(declare-function mu4e~main-view          "mu4e-main")

(declare-function mu4e~context-autoswitch "mu4e-context")
(declare-function mu4e-context-determine  "mu4e-context")
(declare-function mu4e-context-vars       "mu4e-context")
(declare-function mu4e-context-current    "mu4e-context")

(declare-function show-all "org")

;;; Various

(defun mu4e-copy-message-path ()
  "Copy the message-path of message at point to the kill-ring."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path)))
    (kill-new path)
    (mu4e-message "Saved '%s' to kill-ring" path)))

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

(defmacro with~mu4e-context-vars (context &rest body)
  "Evaluate BODY, with variables let-bound for CONTEXT (if any).
`funcall'."
  (declare (indent 2))
  `(let* ((vars (and ,context (mu4e-context-vars ,context))))
     (cl-progv ;; XXX: perhaps use eval's lexical environment instead of progv?
         (mapcar (lambda(cell) (car cell)) vars)
         (mapcar (lambda(cell) (cdr cell)) vars)
       (eval ,@body))))

;;; Folders (1/2)

;; the standard folders can be functions too
(defun mu4e~get-folder (foldervar msg)
  "Within the mu-context of MSG, get message folder FOLDERVAR.
If FOLDER is a string, return it, if it is a function, evaluate
this function with MSG as parameter (which may be `nil'), and
return the result."
  (unless (member foldervar
                  '(mu4e-sent-folder mu4e-drafts-folder
                                     mu4e-trash-folder mu4e-refile-folder))
    (mu4e-error "Folder must be one of mu4e-(sent|drafts|trash|refile)-folder"))
  ;; get the value with the vars for the relevants context let-bound
  (with~mu4e-context-vars (mu4e-context-determine msg nil)
      (let* ((folder (symbol-value foldervar))
             (val
              (cond
               ((stringp   folder) folder)
               ((functionp folder) (funcall folder msg))
               (t (mu4e-error "unsupported type for %S" folder)))))
        (or val (mu4e-error "%S evaluates to nil" foldervar)))))

(defun mu4e-get-drafts-folder (&optional msg)
  "Get the sent folder. See `mu4e-drafts-folder'."
  (mu4e~get-folder 'mu4e-drafts-folder msg))

(defun mu4e-get-refile-folder (&optional msg)
  "Get the folder for refiling. See `mu4e-refile-folder'."
  (mu4e~get-folder 'mu4e-refile-folder msg))

(defun mu4e-get-sent-folder (&optional msg)
  "Get the sent folder. See `mu4e-sent-folder'."
  (mu4e~get-folder 'mu4e-sent-folder msg))

(defun mu4e-get-trash-folder (&optional msg)
  "Get the sent folder. See `mu4e-trash-folder'."
  (mu4e~get-folder 'mu4e-trash-folder msg))

;;; Self-destructing files

(defun mu4e-remove-file-later (filename)
  "Remove FILENAME in a few seconds."
  (run-at-time "30 sec" nil
               (lambda () (ignore-errors (delete-file filename)))))

(defun mu4e-make-temp-file (ext)
  "Create a temporary file with extension EXT. The file will
self-destruct in a few seconds, enough to open it in another
program."
  (let ((tmpfile (make-temp-file "mu4e-" nil (concat "." ext))))
    (mu4e-remove-file-later tmpfile)
    tmpfile))

;;; Folders (2/2)
;;
;; mu4e-attachment-dir is either a string or a function that takes a
;; filename and the mime-type as argument, either (or both) which can
;; be nil

(defun mu4e~get-attachment-dir (&optional fname mimetype)
  "Get the directory for saving attachments from
`mu4e-attachment-dir' (which can be either a string or a function,
see its docstring)."
  (let
      ((dir
        (cond
         ((stringp mu4e-attachment-dir)
          mu4e-attachment-dir)
         ((functionp mu4e-attachment-dir)
          (funcall mu4e-attachment-dir fname mimetype))
         (t
          (mu4e-error "unsupported type for mu4e-attachment-dir" )))))
    (if dir
        (expand-file-name dir)
      (mu4e-error "mu4e-attachment-dir evaluates to nil"))))

;;; Maildir (1/2)

(defun mu4e~guess-maildir (path)
  "Guess the maildir for some path, or nil if cannot find it."
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
      (mu4e-error "%s exists, but is not a directory." dir))
  (cond
   ((file-directory-p dir) t)
   ((yes-or-no-p (mu4e-format "%s does not exist yet. Create now?" dir))
    (mu4e~proc-mkdir dir) t)
   (t nil)))

;;; Messages, warnings and errors

(defun mu4e-format (frm &rest args)
  "Create [mu4e]-prefixed string based on format FRM and ARGS."
  (concat
   "[" (propertize "mu4e" 'face 'mu4e-title-face) "] "
   (apply 'format frm
          (mapcar (lambda (x)
                    (if (stringp x)
                        (decode-coding-string x 'utf-8)
                      x))
                  args))))

(defun mu4e-message (frm &rest args)
  "Like `message', but prefixed with mu4e.
If we're waiting for user-input or if there's some message in the
echo area, don't show anything."
  (unless (or (active-minibuffer-window))
    (message "%s" (apply 'mu4e-format frm args))))

(defun mu4e-index-message (frm &rest args)
  "Like `mu4e-message', but specifically for
index-messages. Doesn't display anything if
`mu4e-hide-index-messages' is non-nil. "
  (unless mu4e-hide-index-messages
    (apply 'mu4e-message frm args)))

(defun mu4e-error (frm &rest args)
  "Create [mu4e]-prefixed error based on format FRM and ARGS.
Does a local-exit and does not return, and raises a
debuggable (backtrace) error."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (error "%s" (apply 'mu4e-format frm args)))

;; the user-error function is only available in emacs-trunk
(unless (fboundp 'user-error)
  (defalias 'user-error 'error))

(defun mu4e-warn (frm &rest args)
  "Create [mu4e]-prefixed warning based on format FRM and ARGS.
Does a local-exit and does not return. In emacs versions below
24.2, the functions is the same as `mu4e-error'."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (user-error "%s" (apply 'mu4e-format frm args)))

;;; Reading user input

(defun mu4e~read-char-choice (prompt choices)
  "Read and return one of CHOICES, prompting for PROMPT.
Any input that is not one of CHOICES is ignored. This mu4e's
version of `read-char-choice', that becomes case-insentive after
trying an exact match."
  (let ((choice) (chosen) (inhibit-quit nil))
    (while (not chosen)
      (message nil);; this seems needed...
      (setq choice (read-char-exclusive prompt))
      (if (eq choice 27) (keyboard-quit)) ;; quit if ESC is pressed
      (setq chosen (or (member choice choices)
                       (member (downcase choice) choices)
                       (member (upcase choice) choices))))
    (car chosen)))

(defun mu4e-read-option (prompt options)
  "Ask user for an option from a list on the input area.
PROMPT describes a multiple-choice question to the user.
OPTIONS describe the options, and is a list of cells describing
particular options. Cells have the following structure:

   (OPTIONSTRING . RESULT)

where OPTIONSTRING is a non-empty string describing the
option. The first character of OPTIONSTRING is used as the
shortcut, and obviously all shortcuts must be different, so you
can prefix the string with an uniquifying character.

The options are provided as a list for the user to choose from;
user can then choose by typing CHAR.  Example:
  (mu4e-read-option \"Choose an animal: \"
              '((\"Monkey\" . monkey) (\"Gnu\" . gnu) (\"xMoose\" . moose)))

User now will be presented with a list: \"Choose an animal:
   [M]onkey, [G]nu, [x]Moose\".

Function will return the cdr of the list element."
  (let* ((prompt (mu4e-format "%s" prompt))
         (optionsstr
          (mapconcat
           (lambda (option)
             ;; try to detect old-style options, and warn
             (when (characterp (car-safe (cdr-safe option)))
               (mu4e-error
                (concat "Please use the new format for options/actions; "
                        "see the manual")))
             (let ((kar (substring (car option) 0 1)))
               (concat
                "[" (propertize kar 'face 'mu4e-highlight-face) "]"
                (substring (car option) 1))))
           options ", "))
         (response
          (mu4e~read-char-choice
           (concat prompt optionsstr
                   " [" (propertize "C-g" 'face 'mu4e-highlight-face)
                   " to cancel]")
           ;; the allowable chars
           (cl-map 'list (lambda(elm) (string-to-char (car elm))) options)))
         (chosen
          (cl-find-if
           (lambda (option) (eq response (string-to-char (car option))))
           options)))
    (if chosen
        (cdr chosen)
      (mu4e-warn "Unknown shortcut '%c'" response))))

;;; Maildir (1/2)

(defun mu4e~get-maildirs-1 (path mdir)
  "Get maildirs under path, recursively, as a list of relative paths."
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
          (setq dirs (append dirs (mu4e~get-maildirs-1 path
                                                       (concat mdir (car dentry) "/")))))))
    dirs))

(defvar mu4e-cache-maildir-list nil
  "Whether to cache the list of maildirs; set it to t if you find
that generating the list on the fly is too slow. If you do, you
can set `(mu4e-root-maildir)-list' to nil to force regenerating the
cache the next time `mu4e-get-maildirs' gets called.")

(defvar mu4e-maildir-list nil
  "Cached list of maildirs.")

(defun mu4e-get-maildirs ()
  "Get maildirs under `mu4e-maildir', recursively, as a list of
relative paths (ie., /archive, /sent etc.). Most of the work is
done in `mu4e~get-maildirs-1'. Note, these results are /cached/
if `mu4e-cache-maildir-list' is customized to non-nil. In that case,
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
  "Ask the user for a shortcut (using PROMPT) as per
(mu4e-maildir-shortcuts), then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`(mu4e-maildir-shortcuts)' evaluates to nil, let user choose from
all maildirs under `mu4e-maildir'."
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
               (cl-find-if (lambda (item) (= kar (plist-get item :key)))
                           (mu4e-maildir-shortcuts)) :maildir)
              (mu4e-warn "Unknown shortcut '%c'" kar)))))))

(defun mu4e-ask-maildir-check-exists (prompt)
  "Like `mu4e-ask-maildir', but check for existence of the maildir,
and offer to create it if it does not exist yet."
  (let* ((mdir (mu4e-ask-maildir prompt))
         (fullpath (concat (mu4e-root-maildir) mdir)))
    (unless (file-directory-p fullpath)
      (and (yes-or-no-p
            (mu4e-format "%s does not exist. Create now?" fullpath))
           (mu4e~proc-mkdir fullpath)))
    mdir))

;;; Bookmarks
(defun mu4e-ask-bookmark (prompt)
  "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query. In order
to have the counts alway up to date, consider adding `mu4e~start'
to the following hooks `mu4e-message-changed-hook' and
`mu4e-index-updated-hook' like so:
  (add-hook 'mu4e-message-changed-hook #'mu4e~start)
  (add-hook 'mu4e-index-updated-hook #'mu4e~start)"
  (unless (mu4e-bookmarks) (mu4e-error "No bookmarks defined"))
  (let* ((prompt (mu4e-format "%s" prompt))
         (server-queries (plist-get mu4e~server-props :queries))
         (bmarks
          (mapconcat
           (lambda (bm)
             (let ((bm-server-query (seq-find (lambda (q)
                                                (string= (plist-get q :query)
                                                         (plist-get bm :query)))
                                              server-queries)))
               (format "[%s]%s (%s/%s)"
                       (propertize (make-string 1 (plist-get bm :key))
                                   'face 'mu4e-highlight-face)
                       (plist-get bm :name)
                       (plist-get bm-server-query :unread)
                       (plist-get bm-server-query :count))))
           (mu4e-bookmarks)
           ", "))
         (kar (read-char (concat prompt bmarks))))
    (mu4e-get-bookmark-query kar)))

(defun mu4e-get-bookmark-query (kar)
  "Get the corresponding bookmarked query for shortcut character
KAR, or raise an error if none is found."
  (let* ((chosen-bm
          (or (cl-find-if
               (lambda (bm)
                 (= kar (plist-get bm :key)))
               (mu4e-bookmarks))
              (mu4e-warn "Unknown shortcut '%c'" kar)))
         (expr (plist-get chosen-bm :query))
         (expr (if (not (functionp expr)) expr
                 (funcall expr)))
         (query (eval expr)))
    (if (stringp query)
        query
      (mu4e-warn "Expression must evaluate to query string ('%S')" expr))))


(defun mu4e-bookmark-define (query name key)
  "Define a bookmark for QUERY with name NAME and
shortcut-character KEY in the list of `mu4e-bookmarks'. This
replaces any existing bookmark with KEY."
  (setq mu4e-bookmarks
        (cl-remove-if
         (lambda (bm)
           (= (plist-get bm :key) key))
         (mu4e-bookmarks)))
  (cl-pushnew `(:name  ,name
                       :query ,query
                       :key   ,key)
              mu4e-bookmarks :test 'equal))


;;; Converting flags->string and vice-versa

(defun mu4e~flags-to-string-raw (flags)
  "Convert a list of flags into a string as seen in Maildir
message files; flags are symbols draft, flagged, new, passed,
replied, seen, trashed and the string is the concatenation of the
uppercased first letters of these flags, as per [1]. Other flags
than the ones listed here are ignored.
Also see `mu4e-flags-to-string'.
\[1\]: http://cr.yp.to/proto/maildir.html"
  (when flags
    (let ((kar (cl-case (car flags)
                 ('draft     ?D)
                 ('flagged   ?F)
                 ('new       ?N)
                 ('passed    ?P)
                 ('replied   ?R)
                 ('seen      ?S)
                 ('trashed   ?T)
                 ('attach    ?a)
                 ('encrypted ?x)
                 ('signed    ?s)
                 ('unread    ?u))))
      (concat (and kar (string kar))
              (mu4e~flags-to-string-raw (cdr flags))))))

(defun mu4e-flags-to-string (flags)
  "Remove duplicates and sort the output of `mu4e~flags-to-string-raw'."
  (concat
   (sort (cl-remove-duplicates
          (append (mu4e~flags-to-string-raw flags) nil)) '>)))

(defun mu4e~string-to-flags-1 (str)
  "Convert a string with message flags as seen in Maildir
messages into a list of flags in; flags are symbols draft,
flagged, new, passed, replied, seen, trashed and the string is
the concatenation of the uppercased first letters of these flags,
as per [1]. Other letters than the ones listed here are ignored.
Also see `mu4e-flags-to-string'.
\[1\]: http://cr.yp.to/proto/maildir.html."
  (when (/= 0 (length str))
    (let ((flag
           (cl-case (string-to-char str)
             (?D   'draft)
             (?F   'flagged)
             (?P   'passed)
             (?R   'replied)
             (?S   'seen)
             (?T   'trashed))))
      (append (when flag (list flag))
              (mu4e~string-to-flags-1 (substring str 1))))))

(defun mu4e-string-to-flags (str)
  "Convert a string with message flags as seen in Maildir messages
into a list of flags in; flags are symbols draft, flagged, new,
passed, replied, seen, trashed and the string is the concatenation
of the uppercased first letters of these flags, as per [1]. Other
letters than the ones listed here are ignored.  Also see
`mu4e-flags-to-string'.  \[1\]:
http://cr.yp.to/proto/maildir.html "
  ;;  "Remove duplicates from the output of `mu4e~string-to-flags-1'"
  (cl-remove-duplicates (mu4e~string-to-flags-1 str)))

;;; Various

(defun mu4e-display-size (size)
  "Get a string representation of SIZE (in bytes)."
  (cond
   ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
   ((and (>= size 1000) (< size 1000000))
    (format "%2.1fK" (/ size 1000.0)))
   ((< size 1000) (format "%d" size))
   (t (propertize "?" 'face 'mu4e-system-face))))


(defun mu4e-display-manual ()
  "Display the mu4e manual page for the current mode.
Or go to the top level if there is none."
  (interactive)
  (info (cl-case major-mode
          ('mu4e-main-mode "(mu4e)Main view")
          ('mu4e-headers-mode "(mu4e)Headers view")
          ('mu4e-view-mode "(mu4e)Message view")
          (t               "mu4e"))))

;;; Misc

(defun mu4e-last-query ()
  "Get the most recent query or nil if there is none."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer  (mu4e-get-headers-buffer)
      mu4e~headers-last-query)))

(defun mu4e-get-view-buffer ()
  (get-buffer mu4e~view-buffer-name))

(defun mu4e-get-headers-buffer ()
  (get-buffer mu4e~headers-buffer-name))

(defun mu4e-select-other-view ()
  "When the headers view is selected, select the message view (if
that has a live window), and vice versa."
  (interactive)
  (let* ((other-buf
          (cond
           ((eq major-mode 'mu4e-headers-mode)
            (mu4e-get-view-buffer))
           ((eq major-mode 'mu4e-view-mode)
            (mu4e-get-headers-buffer))))
         (other-win (and other-buf (get-buffer-window other-buf))))
    (if (window-live-p other-win)
        (select-window other-win)
      (mu4e-message "No window to switch to"))))


(defconst mu4e-output-buffer-name "*mu4e-output*"
  "*internal* Name of the mu4e output buffer.")

(defun mu4e-process-file-through-pipe (path pipecmd)
  "Process file at PATH through a pipe with PIPECMD."
  (let ((buf (get-buffer-create mu4e-output-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process-shell-command pipecmd path t t)
        (view-mode)))
    (switch-to-buffer buf)))

(defvar mu4e~lists-hash nil
  "Hashtable of mailing-list-id => shortname, based on
  `mu4e~mailing-lists' and `mu4e-user-mailing-lists'.")

(defun mu4e-get-mailing-list-shortname (list-id)
  "Get the shortname for a mailing-list with list-id LIST-ID. based
on `mu4e~mailing-lists', `mu4e-user-mailing-lists', and
`mu4e-mailing-list-patterns'."
  (unless mu4e~lists-hash
    (setq mu4e~lists-hash (make-hash-table :test 'equal))
    (dolist (cell mu4e~mailing-lists)
      (puthash (car cell) (cdr cell) mu4e~lists-hash))
    (dolist (cell mu4e-user-mailing-lists)
      (puthash (car cell) (cdr cell) mu4e~lists-hash)))
  (or
   (gethash list-id mu4e~lists-hash)
   (and (boundp 'mu4e-mailing-list-patterns)
        (cl-member-if
         (lambda (pattern)
           (string-match pattern list-id))
         mu4e-mailing-list-patterns)
        (match-string 1 list-id))
   ;; if it's not in the db, take the part until the first dot if there is one;
   ;; otherwise just return the whole thing
   (if (string-match "\\([^.]*\\)\\." list-id)
       (match-string 1 list-id)
     list-id)))

(defvar mu4e-index-updated-hook nil
  "Hook run when the indexing process had one or more updated messages.
This can be used as a simple way to invoke some action when new
messages appear, but note that an update in the index does not
necessarily mean a new message.")

(defvar mu4e-message-changed-hook nil
  "Hook run when there is a message changed in db. For new
messages, it depends on `mu4e-index-updated-hook'. This can be
used as a simple way to invoke some action when a message
changed.")

(make-obsolete-variable 'mu4e-msg-changed-hook
                        'mu4e-message-changed-hook "0.9.19")

(defvar mu4e~contacts-tstamp "0"
  "Timestamp for the most recent contacts update." )

;;; Some handler functions for server messages

(defun mu4e-info-handler (info)
  "Handler function for (:info ...) sexps received from the server
process."
  (let* ((type (plist-get info :info))
         (processed (plist-get info :processed))
         (updated (plist-get info :updated))
         (cleaned-up (plist-get info :cleaned-up))
         (mainbuf (get-buffer mu4e-main-buffer-name)))
    (cond
     ((eq type 'add) t) ;; do nothing
     ((eq type 'index)
      (if (eq (plist-get info :status) 'running)
          (mu4e-index-message
           "Indexing... processed %d, updated %d" processed updated)
        (progn
          (mu4e-index-message
           "Indexing completed; processed %d, updated %d, cleaned-up %d"
           processed updated cleaned-up)
          ;; call the updated hook if anything changed.
          (unless (zerop (+ updated cleaned-up))
            (run-hooks 'mu4e-index-updated-hook))
          (unless (and (not (string= mu4e~contacts-tstamp "0"))
                       (zerop (plist-get info :updated)))
            (mu4e~request-contacts-maybe))
          (when (and (buffer-live-p mainbuf) (get-buffer-window mainbuf))
            (save-window-excursion
              (select-window (get-buffer-window mainbuf))
              (mu4e~main-view 'refresh))))))
     ((plist-get info :message)
      (mu4e-index-message "%s" (plist-get info :message))))))

(defun mu4e-error-handler (errcode errmsg)
  "Handler function for showing an error."
  ;; don't use mu4e-error here; it's running in the process filter context
  (cl-case errcode
    (4 (user-error "No matches for this search query."))
    (t (error "Error %d: %s" errcode errmsg))))


;;; Contacts

(defun mu4e~update-contacts (contacts &optional tstamp)
  "Receive a sorted list of CONTACTS.
Each of the contacts has the form
  (FULL_EMAIL_ADDRESS . RANK) and fill the hash
`mu4e~contacts' with it, with each contact mapped to an integer
for their ranking.

This is used by the completion function in mu4e-compose."
  ;; We have our nicely sorted list, map them to a list
  ;; of increasing integers. We use that map in the composer
  ;; to sort them there. It would have been so much easier if emacs
  ;; allowed us to use the sorted-list as-is, but no such luck.
  (let ((n 0))
    (unless mu4e~contacts
      (setq mu4e~contacts (make-hash-table :test 'equal :weakness nil
                                           :size (length contacts))))
    (dolist (contact contacts)
      (cl-incf n)
      (let* ((address (plist-get contact :address))
             (address
              (if (functionp mu4e-contact-process-function)
                  (funcall mu4e-contact-process-function address)
                address)))
        (when address ;; note the explicit deccode; the strings we get are  utf-8,
          ;; but emacs doesn't know yet.
          (puthash (decode-coding-string address 'utf-8)
                   (plist-get contact :rank) mu4e~contacts))))

    (setq mu4e~contacts-tstamp (or tstamp "0"))

    (unless (zerop n)
      (mu4e-index-message "Contacts updated: %d; total %d"
                          n (hash-table-count mu4e~contacts)))))

(defun mu4e-contacts-info ()
  "Display information about the cache used for contacts
completion; for testing/debugging."
  (interactive)
  (with-current-buffer (get-buffer-create "*mu4e-contacts-info*")
    (erase-buffer)
    (insert (format "complete addresses:        %s\n"
                    (if mu4e-compose-complete-addresses "yes" "no")))
    (insert (format "only personal addresses:   %s\n"
                    (if mu4e-compose-complete-only-personal "yes" "no")))
    (insert (format "only addresses seen after: %s\n"
                    (or mu4e-compose-complete-only-after "no restrictions")))

    (when mu4e~contacts
      (insert (format "number of contacts cached: %d\n\n"
                      (hash-table-count mu4e~contacts)))
      (let ((contacts))
        (maphash (lambda (addr rank)
                   (setq contacts (cons (cons rank addr) contacts))) mu4e~contacts)
        (setq contacts (sort contacts
                             (lambda(cell1 cell2) (< (car cell1) (car cell2)))))
        (dolist (contact contacts)
          (insert (format "%s\n" (cdr contact))))))

    (pop-to-buffer "*mu4e-contacts-info*")))

(defun mu4e~check-requirements ()
  "Check for the settings required for running mu4e."
  (unless (>= emacs-major-version 25)
    (mu4e-error "Emacs >= 25.x is required for mu4e"))
  (when mu4e~server-props
    (unless (string= (mu4e-server-version) mu4e-mu-version)
      (mu4e-error "mu server has version %s, but we need %s"
                  (mu4e-server-version) mu4e-mu-version)))
  (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
    (mu4e-error "Please set `mu4e-mu-binary' to the full path to the mu
    binary."))
  (dolist (var '(mu4e-sent-folder mu4e-drafts-folder
                                  mu4e-trash-folder))
    (unless (and (boundp var) (symbol-value var))
      (mu4e-error "Please set %S" var))
    (unless (functionp (symbol-value var)) ;; functions are okay, too
      (let* ((dir (symbol-value var))
             (path (concat (mu4e-root-maildir) dir)))
        (unless (string= (substring dir 0 1) "/")
          (mu4e-error "%S must start with a '/'" dir))
        (unless (mu4e-create-maildir-maybe path)
          (mu4e-error "%s (%S) does not exist" path var))))))

(defun mu4e-running-p ()
  "Whether mu4e is running.
Checks whether the server process is live."
  (mu4e~proc-running-p))

;;; Starting / getting mail / updating the index

(defvar mu4e~update-timer nil
  "The mu4e update timer.")
(defconst mu4e~update-name " *mu4e-update*"
  "Name of the process and buffer to update mail.")
(defconst mu4e~update-buffer-height 8
  "Height of the mu4e message retrieval/update buffer.")

(defvar mu4e~get-mail-ask-password "mu4e get-mail: Enter password: "
  "Query string for `mu4e-get-mail-command' password.")
(defvar mu4e~get-mail-password-regexp "^Remote: Enter password: $"
  "Regexp to match a password query in the `mu4e-get-mail-command' output.")

(defun mu4e~request-contacts-maybe ()
  "If `mu4e-compose-complete-addresses' is non-nil, get/update
the list of contacts we use for autocompletion; otherwise, do
nothing."
  (when mu4e-compose-complete-addresses
    (setq mu4e-contacts-func 'mu4e~update-contacts)
    (mu4e~proc-contacts
     mu4e-compose-complete-only-personal
     mu4e-compose-complete-only-after
     mu4e~contacts-tstamp)))

(defun mu4e~pong-handler (data func)
  "Handle 'pong' responses from the mu server."
  (setq mu4e~server-props (plist-get data :props)) ;; save info from the server
  (let ((doccount (plist-get mu4e~server-props :doccount)))
    (mu4e~check-requirements)
    (when func (funcall func))
    (when (zerop doccount)
      (mu4e-message "Store is empty; (re)indexing. This may take a while.") ;
      (mu4e-update-index))
    (when (and mu4e-update-interval (null mu4e~update-timer))
      (setq mu4e~update-timer
            (run-at-time 0 mu4e-update-interval
                         (lambda () (mu4e-update-mail-and-index
                                     mu4e-index-update-in-background)))))))

(defun mu4e-last-query-results ()
  "Get the results (counts) of the last cached queries.

The cached queries are the bookmark / maildir queries that are
used to populated the read/unread counts in the main view. They
are refreshed when calling `(mu4e)', i.e., when going to the main
view.

The results are a list of elements of the form
   (:query \"query string\"
            :count  <total number matching count>
            :unread <number of unread messages in count>)"
  (plist-get mu4e~server-props :queries))


(defun mu4e-last-query-result (query)
  "Get the last result for some cached query, as per
  `mu4e-bookmark-query-results' or nil if not found."
  (cl-find-if
   (lambda (elm) (string= (plist-get elm :query) query))
   (mu4e-last-query-results)))


(defun mu4e~start (&optional func)
  "If `mu4e-contexts' have been defined, but we don't have a
context yet, switch to the matching one, or none matches, the
first. If mu4e is already running, execute function FUNC (if
non-nil). Otherwise, check various requireme`'nts, then start mu4e.
When successful, call FUNC (if non-nil) afterwards."
  (unless (mu4e-context-current)
    (mu4e~context-autoswitch nil mu4e-context-policy))
  (setq mu4e-pong-func (lambda (info) (mu4e~pong-handler info func)))
  (mu4e~proc-ping
   (mapcar ;; send it a list of queries we'd like to see read/unread info for
    (lambda (bm)
      (funcall (or mu4e-query-rewrite-function #'identity)
               (plist-get bm :query)))
    ;; exclude bookmarks that are not strings, and with certain flags
    (seq-filter (lambda (bm)
                  (and (stringp (plist-get bm :query))
                       (not (or (plist-get bm :hide) (plist-get bm :hide-unread)))))
                (append (mu4e-bookmarks)
                        (mu4e~maildirs-with-query)))))
  ;; maybe request the list of contacts, automatically refreshed after
  ;; reindexing
  (unless mu4e~contacts (mu4e~request-contacts-maybe)))

(defun mu4e-clear-caches ()
  "Clear any cached resources."
  (setq
   mu4e-maildir-list nil
   mu4e~contacts nil
   mu4e~contacts-tstamp "0"))

(defun mu4e~stop ()
  "Stop the mu4e session."
  (when mu4e~update-timer
    (cancel-timer mu4e~update-timer)
    (setq mu4e~update-timer nil))
  (mu4e-clear-caches)
  (mu4e~proc-kill)
  ;; kill all mu4e buffers
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (member major-mode
                     '(mu4e-headers-mode mu4e-view-mode mu4e-main-mode))
         (kill-buffer))))
   (buffer-list)))

(defun mu4e~maildirs-with-query ()
  "Return a copy of `mu4e-maildirs-shortcuts' with :query populated.

This is meant to be the exact same data structure as
`mu4e-bookmarks'."
  (cl-mapcar
   (lambda (m)
     (append
      ;; we want to change the :maildir key to :name, and add a :query key
      (list :name (plist-get m :maildir)
            :query (format "maildir:\"%s\"" (plist-get m :maildir)))
      ;; next we want to append any other keys to our previous list (e.g. :hide,
      ;; :key, etc) but skipping :maildir (since it's renamed to :name)
      (cl-loop for (key value) on m by 'cddr
               when (not (equal key :maildir))
               append (list key value))))
   (mu4e-maildir-shortcuts)))

(defun mu4e~longest-of-maildirs-and-bookmarks ()
  "Return the length of longest name of bookmarks and maildirs."
  (cl-loop for b in (append (mu4e-bookmarks)
                            (mu4e~maildirs-with-query))
           maximize (string-width (plist-get b :name))))


;;; Indexing & Updating

(defvar mu4e~progress-reporter nil
  "Internal, the progress reporter object.")

(defun mu4e~get-mail-process-filter (proc msg)
  "Filter the output of `mu4e-get-mail-command'.
Currently the filter only checks if the command asks for a password
by matching the output against `mu4e~get-mail-password-regexp'.
The messages are inserted into the process buffer.

Also scrolls to the final line, and update the progress throbber."
  (when mu4e~progress-reporter
    (progress-reporter-update mu4e~progress-reporter))

  (when (string-match mu4e~get-mail-password-regexp msg)
    (if (process-get proc 'x-interactive)
        (process-send-string proc
                             (concat (read-passwd mu4e~get-mail-ask-password)
                                     "\n"))
      ;; TODO kill process?
      (mu4e-error "Unrecognized password request")))
  (when (process-buffer proc)
    (let ((inhibit-read-only t)
          (procwin (get-buffer-window (process-buffer proc))))
      ;; Insert at end of buffer. Leave point alone.
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (if (string-match ".*\r\\(.*\\)" msg)
            (progn
              ;; kill even with \r
              (end-of-line)
              (let ((end (point)))
                (beginning-of-line)
                (delete-region (point) end))
              (insert (match-string 1 msg)))
          (insert msg)))
      ;; Auto-scroll unless user is interacting with the window.
      (when (and (window-live-p procwin)
                 (not (eq (selected-window) procwin)))
        (with-selected-window procwin
          (goto-char (point-max)))))))

(defun mu4e-update-index ()
  "Update the mu4e index."
  (interactive)
  (mu4e~proc-index  mu4e-index-cleanup mu4e-index-lazy-check))

(defvar mu4e~update-buffer nil
  "Internal, store the buffer of the update process when
  updating.")

(define-derived-mode mu4e~update-mail-mode special-mode "mu4e:update"
  "Major mode used for retrieving new e-mail messages in `mu4e'.")

(define-key mu4e~update-mail-mode-map (kbd "q") 'mu4e-kill-update-mail)

(defun mu4e~temp-window (buf height)
  "Create a temporary window with HEIGHT at the bottom of the
frame to display buffer BUF."
  (let ((win
         (split-window
          (frame-root-window)
          (- (window-height (frame-root-window)) height))))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    win))

(defun mu4e~update-sentinel-func (proc _msg)
  "Sentinel function for the update process."
  (when mu4e~progress-reporter
    (progress-reporter-done mu4e~progress-reporter)
    (setq mu4e~progress-reporter nil))
  (unless mu4e-hide-index-messages
    (message nil))
  (if (or (not (eq (process-status proc) 'exit))
          (/= (process-exit-status proc) 0))
      (progn
        (when mu4e-index-update-error-warning
          (mu4e-message "Update process returned with non-zero exit code")
          (sit-for 5))
        (when mu4e-index-update-error-continue
          (mu4e-update-index)))
    (mu4e-update-index))
  (when (buffer-live-p mu4e~update-buffer)
    (unless (eq mu4e-split-view 'single-window)
      (mapc #'delete-window (get-buffer-window-list mu4e~update-buffer)))
    (kill-buffer mu4e~update-buffer)))

;; complicated function, as it:
;;   - needs to check for errors
;;   - (optionally) pop-up a window
;;   - (optionally) check password requests
(defun mu4e~update-mail-and-index-real (run-in-background)
  "Get a new mail by running `mu4e-get-mail-command'. If
RUN-IN-BACKGROUND is non-nil (or called with prefix-argument),
run in the background; otherwise, pop up a window."
  (let* ((process-connection-type t)
         (proc (start-process-shell-command
                "mu4e-update" mu4e~update-name
                mu4e-get-mail-command))
         (buf (process-buffer proc))
         (win (or run-in-background
                  (mu4e~temp-window buf mu4e~update-buffer-height))))
    (setq mu4e~update-buffer buf)
    (when (window-live-p win)
      (with-selected-window win
        ;; ;;(switch-to-buffer buf)
        ;; (set-window-dedicated-p win t)
        (erase-buffer)
        (insert "\n") ;; FIXME -- needed so output starts
        (mu4e~update-mail-mode)))
    (setq mu4e~progress-reporter
          (unless mu4e-hide-index-messages
            (make-progress-reporter
             (mu4e-format "Retrieving mail..."))))
    (set-process-sentinel proc 'mu4e~update-sentinel-func)
    ;; if we're running in the foreground, handle password requests
    (unless run-in-background
      (process-put proc 'x-interactive (not run-in-background))
      (set-process-filter proc 'mu4e~get-mail-process-filter))))

(defun mu4e-update-mail-and-index (run-in-background)
  "Get a new mail by running `mu4e-get-mail-command'. If
run-in-background is non-nil (or called with prefix-argument), run
in the background; otherwise, pop up a window."
  (interactive "P")
  (unless mu4e-get-mail-command
    (mu4e-error "`mu4e-get-mail-command' is not defined"))
  (if (and (buffer-live-p mu4e~update-buffer)
           (process-live-p (get-buffer-process mu4e~update-buffer)))
      (mu4e-message "Update process is already running")
    (progn
      (run-hooks 'mu4e-update-pre-hook)
      (mu4e~update-mail-and-index-real run-in-background))))

(defun mu4e-kill-update-mail ()
  "Stop the update process by killing it."
  (interactive)
  (let* ((proc (and (buffer-live-p mu4e~update-buffer)
                    (get-buffer-process mu4e~update-buffer))))
    (when (process-live-p proc)
      (kill-process proc t))))

(define-obsolete-function-alias 'mu4e-interrupt-update-mail
  'mu4e-kill-update-mail)


;;; Logging / debugging

(defconst mu4e~log-max-size 1000000
  "Max number of characters to keep around in the log buffer.")
(defconst mu4e~log-buffer-name "*mu4e-log*"
  "*internal* Name of the logging buffer.")

(defun mu4e~get-log-buffer ()
  "Fetch (and maybe create) the log buffer."
  (unless (get-buffer mu4e~log-buffer-name)
    (with-current-buffer (get-buffer-create mu4e~log-buffer-name)
      (view-mode)

      (when (fboundp 'so-long-mode)
        (unless (eq major-mode 'so-long-mode)
          (eval '(so-long-mode))))

      (setq buffer-undo-list t)))
  mu4e~log-buffer-name)

(defun mu4e-log (type frm &rest args)
  "Write a message of TYPE with format-string FRM and ARGS in
*mu4e-log* buffer, if the variable mu4e-debug is non-nil. Type is
either 'to-server, 'from-server or 'misc. This function is meant for debugging."
  (when mu4e-debug
    (with-current-buffer (mu4e~get-log-buffer)
      (let* ((inhibit-read-only t)
             (tstamp (propertize (format-time-string "%Y-%m-%d %T.%3N"
                                                     (current-time))
                                 'face 'font-lock-string-face))
             (msg-face
              (cl-case type
                (from-server 'font-lock-type-face)
                (to-server   'font-lock-function-name-face)
                (misc        'font-lock-variable-name-face)
                (error       'font-lock-warning-face)
                (otherwise   (mu4e-error "Unsupported log type"))))
             (msg (propertize (apply 'format frm args) 'face msg-face)))
        (save-excursion
          (goto-char (point-max))
          (insert tstamp
                  (cl-case type
                    (from-server " <- ")
                    (to-server   " -> ")
                    (error       " !! ")
                    (otherwise   " "))
                  msg "\n")

          ;; if `mu4e-log-max-lines is specified and exceeded, clearest the oldest
          ;; lines
          (when (> (buffer-size) mu4e~log-max-size)
            (goto-char (- (buffer-size) mu4e~log-max-size))
            (beginning-of-line)
            (delete-region (point-min) (point))))))))

(defun mu4e-toggle-logging ()
  "Toggle between enabling/disabling debug-mode (in debug-mode,
mu4e logs some of its internal workings to a log-buffer. See
`mu4e-visit-log'."
  (interactive)
  (mu4e-log 'misc "logging disabled")
  (setq mu4e-debug (not mu4e-debug))
  (mu4e-message "debug logging has been %s"
                (if mu4e-debug "enabled" "disabled"))
  (mu4e-log 'misc "logging enabled"))

(defun mu4e-show-log ()
  "Visit the mu4e debug log."
  (interactive)
  (unless mu4e-debug (mu4e-toggle-logging))
  (let ((buf (get-buffer mu4e~log-buffer-name)))
    (unless (buffer-live-p buf)
      (mu4e-warn "No debug log available"))
    (switch-to-buffer buf)))


(defun mu4e-split-ranges-to-numbers (str n)
  "Convert STR containing attachment numbers into a list of numbers.
STR is a string; N is the highest possible number in the list.
This includes expanding e.g. 3-5 into 3,4,5.  If the letter
\"a\" ('all')) is given, that is expanded to a list with numbers [1..n]."
  (let ((str-split (split-string str))
        beg end list)
    (dolist (elem str-split list)
      ;; special number "a" converts into all attachments 1-N.
      (when (equal elem "a")
        (setq elem (concat "1-" (int-to-string n))))
      (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" elem)
          ;; we have found a range A-B, which needs converting
          ;; into the numbers A, A+1, A+2, ... B.
          (progn
            (setq beg (string-to-number (match-string 1 elem))
                  end (string-to-number (match-string 2 elem)))
            (while (<= beg end)
              (cl-pushnew beg list :test 'equal)
              (setq beg (1+ beg))))
        ;; else just a number
        (cl-pushnew (string-to-number elem) list :test 'equal)))
    ;; Check that all numbers are valid.
    (mapc
     (lambda (x)
       (cond
        ((> x n)
         (mu4e-warn "Attachment %d bigger than maximum (%d)" x n))
        ((< x 1)
         (mu4e-warn "Attachment number must be greater than 0 (%d)" x))))
     list)))

;;; Misc 2

(defvar mu4e-imagemagick-identify "identify"
  "Name/path of the Imagemagick 'identify' program.")

(defun mu4e~image-width-scale (width height max_width max_height)
  "Returns a width to use for proportional image scaling
to satisfy both MAX_WIDTH and MAX_HEIGHT restrictions."
  (floor
   (if (<= width max_width)
       (if (<= height max_height)
           width                                  ; both width and height ok, just return width
         (* (/ max_height (float height)) width)) ; height is too large, scale width by hmax/h
     (if (<= height max_height)
         max_width                                ; width is too large, return max_width as scaling
       (let ((width_heightscale (* (/ max_height (float height)) width)))
         (min max_width width_heightscale))))))    ; both too large, return smallest width

(defun mu4e-display-image (imgpath &optional maxwidth maxheight)
  "Display image IMG at point; optionally specify MAXWIDTH and
MAXHEIGHT. Function tries to use imagemagick if available (ie.,
emacs was compiled with imagemagick support); otherwise MAXWIDTH
and MAXHEIGHT are ignored."
  (let* ((have-im (and (fboundp 'imagemagick-types)
                       (imagemagick-types))) ;; hmm, should check for specific type
         (identify (and have-im maxwidth
                        (executable-find mu4e-imagemagick-identify)))
         (props (and identify (mapcar 'string-to-number
                                      (split-string (shell-command-to-string
                                                     (format "%s -format '%%w %%h' %s"
                                                             identify (shell-quote-argument imgpath)))))))
         (width (and props (car props)))
         (height (and props (car (cdr props))))

         (img (if have-im
                  (create-image imgpath 'imagemagick nil
                                :width (mu4e~image-width-scale width height maxwidth maxheight))
                (create-image imgpath))))
    (when img
      (save-excursion
        (insert "\n")
        (let ((size (image-size img))) ;; inspired by gnus..
          (insert-char ?\n
                       (max 0 (round (- (window-height) (or maxheight (cdr size)) 1) 2)))
          (insert-char ?\.
                       (max 0 (round (- (window-width)  (or maxwidth (car size))) 2)))
          (insert-image img))))))


(defun mu4e-hide-other-mu4e-buffers ()
  "Bury mu4e-buffers (main, headers, view) (and delete all windows
displaying it). Do _not_ bury the current buffer, though."
  (interactive)
  (unless (eq mu4e-split-view 'single-window)
    (let ((curbuf (current-buffer)))
      ;; note: 'walk-windows' does not seem to work correctly when modifying
      ;; windows; therefore, the doloops here
      (dolist (frame (frame-list))
        (dolist (win (window-list frame nil))
          (with-current-buffer (window-buffer win)
            (unless (eq curbuf (current-buffer))
              (when (member major-mode '(mu4e-headers-mode mu4e-view-mode))
                (when (eq t (window-deletable-p win))
                  (delete-window win))))))) t)))


(defun mu4e-get-time-date (prompt)
  "Determine the emacs time value for the time/date entered by user
  after PROMPT. Formats are all that are accepted by
  `parse-time-string'."
  (let ((timestr (read-string (mu4e-format "%s" prompt))))
    (apply 'encode-time (org-parse-time-string timestr))))


;;; Mu4e-org-mode

(define-derived-mode mu4e-org-mode org-mode "mu4e:org"
  "Major mode for mu4e documents, derived from
  `org-mode'.")

(defun mu4e-info (path)
  "Show a buffer with the information (an org-file) at PATH."
  (unless (file-exists-p path)
    (mu4e-error "Cannot find %s" path))
  (let ((curbuf (current-buffer)))
    (find-file path)
    (mu4e-org-mode)
    (setq buffer-read-only t)
    (define-key mu4e-org-mode-map (kbd "q")
      `(lambda ()
         (interactive)
         (bury-buffer)
         (switch-to-buffer ,curbuf)))))

(defun mu4e-about ()
  "Show the mu4e 'about' page."
  (interactive)
  (mu4e-info (concat mu4e-doc-dir "/mu4e-about.org")))

(defun mu4e-news ()
  "Show the mu4e 'about' page."
  (interactive)
  (mu4e-info (concat mu4e-doc-dir "/NEWS.org")))

;;; Misc 3

(defun mu4e-refresh-message (path)
  "Re-parse message at PATH; if this works, we will
receive (:info add :path <path> :docid <docid>) as well as (:update
<msg-sexp>)."
  (mu4e~proc-add path))


(defun mu4e~fontify-cited ()
  "Colorize message content based on the citation level. This is
used in the view and compose modes."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^\n" nil t) ;; search the first empty line
      (while (re-search-forward mu4e-cited-regexp nil t)
        (let* ((level (string-width (replace-regexp-in-string
                                     "[^>]" "" (match-string 0))))
               (face  (unless (zerop level)
                        (intern-soft (format "mu4e-cited-%d-face" level)))))
          (when face
            (add-text-properties (line-beginning-position 1)
                                 (line-end-position 1) `(face ,face))))))))

(defun mu4e~fontify-signature ()
  "Give the message signatures a distinctive color. This is used in
the view and compose modes and will color each signature in digest messages adhering to RFC 1153."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (while (re-search-forward "^-- *$" nil t)
        (let ((p (point))
              (end (or
                    (re-search-forward "\\(^-\\{30\\}.*$\\)" nil t) ;; 30 by RFC1153
                    (point-max))))
          (add-text-properties p end '(face mu4e-footer-face)))))))

;;; Misc 4

(defun mu4e~quote-for-modeline (str)
  "Quote a string to be used literally in the modeline. The
string will be shortened to fit if its length exceeds
`mu4e-modeline-max-width'."
  (replace-regexp-in-string
   "%" "%%"
   (truncate-string-to-width str mu4e-modeline-max-width 0 nil t)))

(defun mu4e~active-composition-buffers ()
  "Return all active mu4e composition buffers"
  (let (buffers)
    (save-excursion
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (eq major-mode 'mu4e-compose-mode)
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))


;;
;; Loading messages
;;

(defvar mu4e-loading-mode-map nil  "Keymap for *mu4e-loading* buffers.")
(unless mu4e-loading-mode-map
  (setq mu4e-loading-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map "n" 'ignore)
          (define-key map "p" 'ignore)
          (define-key map "q"
            (lambda()(interactive)
              (if (eq mu4e-split-view 'single-window)
                  'kill-buffer
                'kill-buffer-and-window)))
          map)))
(fset 'mu4e-loading-mode-map mu4e-loading-mode-map)

(define-derived-mode mu4e-loading-mode special-mode
  "mu4e:loading"
  (use-local-map mu4e-loading-mode-map)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Loading message..."
                        'face 'mu4e-system-face 'intangible t))))

;;; _
(provide 'mu4e-utils)
;;; mu4e-utils.el ends here
