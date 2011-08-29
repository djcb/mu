;;; mua-msg.el -- part of mua, the mu mail user agent
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

;; mua

;;; Code:
 
(eval-when-compile (require 'cl))


(defvar mua/msg-map nil
  "*internal* a map of uid->message.

This map adds a level of indirection for message files; many
actions (such moving, responding to or even reading a message)
cause the file names to change. Here we map the initial file to a
uid, the latter which stays constant over the lifetime of a
message in the system (in practice, the lifetime of a particular
headers buffer).

When creating the headers buffer, the file names are registered
with `mua/msg-map-add'.

All operation that change file names ultimately (should) end up
in `mua/msg-move', which will update the map after the
moving (using `mua/msg-map-update')

Other places of the code can use the uid to get the *current*
path of the file using `mua/msg-map-get-path'.
")

(defun mua/msg-map-add (path)
  "Add a message PATH to the `mua/msg-map', and return the uid
  for it."
  (unless mua/msg-map
    (setq mua/msg-map (make-hash-table :size 256 :rehash-size 2 :weakness t)))
  (let ((uid (sha1 path)))
    (puthash uid path mua/msg-map)
    uid))

(defun mua/msg-map-update (uid path)
  "Set the new path for the message identified by UID to PATH."
  (if (gethash uid mua/msg-map)
    (puthash uid path mua/msg-map)
    (mua/warn "No message file registered for uid")))

(defun mua/msg-map-get-path (uid)
  "Get the current path for the message identified by UID."
  (gethash uid mua/msg-map))

(defun mua/msg-move (uid &optional targetdir flags ignore-already)
  "Move message identified by UID to TARGETDIR using 'mu mv', and
update the database with the new situation. SRC must be the full,
absolute path to a message file, while TARGETDIR must be a
maildir - that is, the part _without_ cur/ or new/. 'mu mv' will
calculate the target directory and the exact file name. See
`mua/msg-map' for a discussion about UID.

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
`mua/msg-string-to-flags' and `mua/msg-flags-to-string'.

If TARGETDIR is '/dev/null', remove SRC. After the file system
move, the database will be updated as well, using the 'mu add'
and 'mu remove' commands.

If IGNORE-ALREADY is non-nil, don't consider it an error when the target file is
the same as the source file.

Function returns t the move succeeds, in other cases, it returns
nil.

\[1\]  URL `http://cr.yp.to/proto/maildir.html'."
  (condition-case err
    (let ((src (mua/msg-map-get-path uid)))
      (unless src (error "Source path not registered for %S" uid))
      (unless (or targetdir src) (error "Either targetdir or flags required"))
      (unless (file-readable-p src) (error "Source is unreadable (%S)" src))
      (let* ((flagstr 
	       (if (stringp flags) flags (mua/msg-flags-to-string flags)))
	      (argl (append ;; build-up the command line 
		      '("mv" "--print-target" "--ignore-dups") 
		      (when flagstr (list (concat "--flags=" flagstr)))
		      (list src)
		      (when targetdir (list targetdir))))
	      ;; execute it, and get the results
	      (rv (apply 'mua/mu-run argl))
	      (code (car rv)) (output (cdr rv)))
	(unless (= 0 code)
	  (error "Moving message failed: %S" output))
	
	;; success!
	(let ((targetpath (substring output 0 -1)))

	  (when (and targetpath (not (string= src targetpath)))
	    ;; update the UID-map
	    (mua/msg-map-update uid targetpath)
	    ;; remove the src file
	    (mua/mu-remove-async src)
	    ;; and add the target file, unless it's dead now
	    (unless (string= targetdir "/dev/null")
		(mua/mu-add-async targetpath)))
	  t)))

    (error (mua/warn "error: %s" (error-message-string err)))))


(defun mua/msg-flags-from-path (path)
  "Get the flags for the message at PATH, which does not have to exist.
The flags are returned as a list consisting of one or more of
DFNPRST, mean resp. Deleted, Flagged, New, Passed Replied, Seen
and Trash, as defined in [1]. See `mua/msg-string-to-flags'
and `mua/msg-flags-to-string'.
\[1\]  http://cr.yp.to/proto/maildir.html." 
  (when (string-match ",\\(\[A-Z\]*\\)$" path)
    (mua/msg-string-to-flags (match-string 1 path))))


(defun mua/msg-maildir-from-path (path &optional dont-strip-prefix)
  "Get the maildir from PATH; in this context, 'maildir' is the
part between the `mua/maildir' and the /cur or /new; so
e.g. \"/home/user/Maildir/foo/bar/cur/12345:2,S\" would have
\"/foo/bar\" as its maildir. If DONT-STRIP-PREFIX is non-nil,
function will instead _not_ remove the `mua/maildir' from the
front - so in that case, the example would return
\"/home/user/Maildir/foo/bar/\". If the maildir cannot be
determined, return `nil'."
  (when (and (string-match "^\\(.*\\)/\\(cur\\|new\\)/\[^/\]*$" path))
    (let ((mdir (match-string 1 path)))	 
      (when (and (< (length mua/maildir) (length mdir))
	      (string= (substring mdir 0 (length mua/maildir)) mua/maildir))
	(if dont-strip-prefix
	  mdir
	  (substring mdir (length mua/maildir)))))))

(defun mua/msg-flags-to-string (flags)
  "Remove duplicates and sort the output of `mua/msg-flags-to-string-1'."
  (concat
    (sort (remove-duplicates
	     (append (mua/msg-flags-to-string-1 flags) nil)) '>)))

(defun mua/msg-flags-to-string-1 (flags)
  "Convert a list of flags into a string as seen in Maildir
message files; flags are symbols draft, flagged, new, passed,
replied, seen, trashed and the string is the concatenation of the
uppercased first letters of these flags, as per [1]. Other flags
than the ones listed here are ignored.

Also see `mua/msg-string-to-flags'.

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
	(mua/msg-flags-to-string-1 (cdr flags))))))


(defun mua/msg-string-to-flags (str)
  "Remove duplicates from the output of `mua/msg-string-to-flags-1'"
  (remove-duplicates (mua/msg-string-to-flags-1 str)))

(defun mua/msg-string-to-flags-1 (str)
  "Convert a string with message flags as seen in Maildir
messages into a list of flags in; flags are symbols draft,
flagged, new, passed, replied, seen, trashed and the string is
the concatenation of the uppercased first letters of these flags,
as per [1]. Other letters than the ones listed here are ignored.
Also see `mua/msg-flags-to-string'.

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
	(mua/msg-string-to-flags-1 (substring str 1))))))

(provide 'mua-msg-file)
