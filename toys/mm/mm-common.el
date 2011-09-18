;;; mm-common.el -- part of mm, the mu mail user agent
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

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

(eval-when-compile (require 'cl))
(require 'ido)





;;; converting flags->string and vice-versa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mm/flags-to-string (flags)
  "Remove duplicates and sort the output of `mm/flags-to-string-raw'."
  (concat
    (sort (remove-duplicates (append (mm/flags-to-string-raw flags) nil)) '>)))

(defun mm/flags-to-string-raw (flags)
  "Convert a list of flags into a string as seen in Maildir
message files; flags are symbols draft, flagged, new, passed,
replied, seen, trashed and the string is the concatenation of the
uppercased first letters of these flags, as per [1]. Other flags
than the ones listed here are ignored.

Also see `mm/flags-to-string'.

\[1\]: http://cr.yp.to/proto/maildir.html"
  (when flags
    (let ((kar (case (car flags)
		 ('draft     ?D)
		 ('flagged   ?F)
		 ('new       ?N)
		 ('passed    ?P)
		 ('replied   ?R)
		 ('seen      ?S)
		 ('trashed   ?T)
		 ('encrypted ?x)
		 ('signed    ?s)
		 ('unread    ?u))))
      (concat (and kar (string kar))
	(mm/flags-to-string-raw (cdr flags))))))


(defun mm/string-to-flags (str)
  "Remove duplicates from the output of `mm/string-to-flags-1'"
  (remove-duplicates (mm/string-to-flags-1 str)))

(defun mm/string-to-flags-1 (str)
  "Convert a string with message flags as seen in Maildir
messages into a list of flags in; flags are symbols draft,
flagged, new, passed, replied, seen, trashed and the string is
the concatenation of the uppercased first letters of these flags,
as per [1]. Other letters than the ones listed here are ignored.
Also see `mu/flags-to-string'.

\[1\]: http://cr.yp.to/proto/maildir.html"
  (when (/= 0 (length str))
    (let ((flag
	    (case (string-to-char str)
	      (?D   'draft)
	      (?F   'flagged)
	      (?P   'passed)
	      (?R   'replied)
	      (?S   'seen)
	      (?T   'trashed))))
      (append (when flag (list flag))
	(mm/string-to-flags-1 (substring str 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;; moving message files, changing flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm/move-msg (uid &optional targetdir flags ignore-already)
  "Move message identified by UID to TARGETDIR using 'mu mv', and
update the database with the new situation. TARGETDIR must be a
maildir - that is, the part _without_ cur/ or new/. 'mu mv' will
calculate the target directory and the exact file name. See
`mm/msg-map' for a discussion about UID.

After the file system move (rename) has been done, 'mu remove'
and/or 'mu add' are invoked asynchronously to update the database
with the changes.

Optionally, you can specify the FLAGS for the new file. The FLAGS
parameter can have the following forms:
  1. a list of flags such as '(passed replied seen)
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mm/string-to-flags' and `mm/flags-to-string'.

If TARGETDIR is '/dev/null', remove SRC. After the file system
move, the database will be updated as well, using the 'mu add'
and 'mu remove' commands.

If IGNORE-ALREADY is non-nil, don't consider it an error when the target file is
the same as the source file.

Function returns t the move succeeds, in other cases, it returns
nil.

\[1\]  URL `http://cr.yp.to/proto/maildir.html'."
  (let* ((src (mm/msg-map-get-path uid)))
    (unless src (error "Source path not registered for %S" uid))
    (unless (or targetdir src) (error "Either targetdir or flags required"))
    (unless (file-readable-p src) (error "Source is unreadable (%S)" src))
    (let* ((flagstr (if (stringp flags) flags (mm/flags-to-string flags)))
	    (argl  (remove-if 'not  ;; build up the arg list
		     (list "mv" "--print-target" "--ignore-dups"
		       (when flagstr (concat "--flags=" flagstr))
		       src targetdir)))
	    ;; execute it, and get the results
	    (rv (apply 'mm/mu-run argl))
	    (code (car rv)) (output (cdr rv)))
      (unless (= 0 code) (error "Moving message failed: %S" output))
	;; success!
      (let ((targetpath (substring output 0 -1)))
	(when (and targetpath (not (string= src targetpath)))
	  (mm/msg-map-update uid targetpath) ;; update the UID-map
	  (mm/db-remove-async src) 	  ;; remove the src from the db
	  (unless (string= targetdir "/dev/null")
	    (mm/db-add-async targetpath))) ;; add the target to the db
	(mm/db-update-execute)
	t))))






;;; other helper function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: make this recursive
(defun mm/get-sub-maildirs (maildir)
  "Get all readable sub-maildirs under MAILDIR."
  (let ((maildirs (remove-if
		    (lambda (dentry)
		      (let ((path (concat maildir "/" dentry)))
			(or
			  (string= dentry ".")
			  (string= dentry "..")
			  (not (file-directory-p path))
			  (not (file-readable-p path))
			  (file-exists-p (concat path "/.noindex")))))
		    (directory-files maildir))))
    (map 'list (lambda (dir) (concat "/" dir)) maildirs)))


(defun mm/ask-maildir (prompt)
  "Ask user with PROMPT for a maildir name, if fullpath is
non-nill, return the fulpath (i.e., `mm/maildir' prepended to the
chosen folder)."
  (unless (and mm/inbox-folder mm/drafts-folder mm/sent-folder)
    (error "`mm/inbox-folder', `mm/drafts-folder' and
    `mm/sent-folder' must be set"))
  (unless mm/maildir (error "`mm/maildir' must be set"))
  (interactive)
  (ido-completing-read prompt (mm/get-sub-maildirs mm/maildir)))


(defun mm/new-buffer (bufname)
  "Return a new buffer BUFNAME; if such already exists, kill the
old one first."
  (when (get-buffer bufname)
    (progn
      (message (format "Killing %s" bufname))
      (kill-buffer bufname)))
  (get-buffer-create bufname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mm-common)
