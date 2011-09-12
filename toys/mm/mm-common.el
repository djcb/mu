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


(defun mm/eval-msg-string (str)
  "Get the plist describing an email message, from STR containing
a message sexp.

 a message sexp looks something like:
 \(
  :from ((\"Donald Duck\" . \"donald@example.com\"))
  :to ((\"Mickey Mouse\" . \"mickey@example.com\"))
  :subject \"Wicked stuff\"
  :date (20023 26572 0)
  :size 15165
  :references (\"200208121222.g7CCMdb80690@msg.id\")
  :in-reply-to \"200208121222.g7CCMdb80690@msg.id\"
  :message-id \"foobar32423847ef23@pluto.net\"
  :maildir: \"/archive\"
  :path \"/home/mickey/Maildir/inbox/cur/1312254065_3.32282.pluto,4cd5bd4e9:2,\"
  :priority high
  :flags (new unread)
  :attachments ((2 \"hello.jpg\" \"image/jpeg\") (3 \"laah.mp3\" \"audio/mp3\"))
  :body-txt \" <message body>\"
\)
other fields are :cc, :bcc, :body-html

When the s-expression comes from the database ('mu find'), the
fields :attachments, :body-txt, :body-html, :references, :in-reply-to
are missing (because that information is not stored in the
database -- at least not in a usable way."
  (condition-case nil
    (car (read-from-string str));; read-from-string returns a cons
    (error "Failed to parse message")))


(defun mm/msg-field (msg field)
  "Get a field from this message, or nil. The fields are the
fields of the message, which are the various items of the plist
as described in `mm/eval-msg-string'

There is also the special field :body (which is either :body-txt,
or if not available, :body-html converted to text)."
  (case field
    (:body
      (let* ((body (mm/msg-field msg :body-txt))
	      (body (or body (with-temp-buffer
			       (mm/msg-field msg :body-html)
			       (html2text)
			       (buffer-string)))))))
    (t (plist-get msg field))))






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

;;; some functions for *asyncronously* updating the database

(defvar mm/db-update-proc  nil
  "*internal* Process for async db updates.")
(defvar mm/db-update-name "*mm-db-update*"
  "*internal* name of the db-update process")
(defvar mm/db-add-paths     nil
  "*internal* List of message paths to add to the database.")
(defvar mm/db-remove-paths  nil
  "*internal* List of message paths to remove from the database.")


(defun mm/db-update-proc-sentinel (proc msg)
  "Check the database update process upon completion."
  (let ((procbuf (process-buffer proc))
	 (status (process-status proc))
	 (exit-status (process-exit-status proc)))
    (when (and (buffer-live-p procbuf) (memq status '(exit signal)))
      (case status
	('signal (mm/log "Process killed"))
	('exit
	  (case exit-status
	    (mm/log "Result: %s" (mm/error-string exit-status))))))
    ;; try to update again, maybe there are some new updates
    (mm/db-update-execute)))


(defun mm/db-update-execute ()
  "Update the database; remove paths in `mm/db-remove-paths',
and add paths in `mm/db-add-paths'. Updating is ansynchronous."

  ;; when it's already running, do nothing
  (unless (and mm/db-update-proc (eq (process-status mm/db-update-proc) 'run))
    (when mm/db-remove-paths
      (let ((remove-paths (copy-list mm/db-remove-paths)))
	(mm/log (concat mm/mu-binary " remove "
		  (mapconcat 'identity remove-paths " ")))
	(setq mm/db-remove-paths nil) ;; clear the old list
	(setq mm/db-update-proc
	  (apply 'start-process mm/db-update-name mm/db-update-name mm/mu-binary
	    "remove" remove-paths))
	(set-process-sentinel mm/db-update-proc 'mm/db-update-proc-sentinel)))))

  ;; when it's already running, do nothing
  (unless (and mm/db-update-proc (eq (process-status mm/db-update-proc) 'run))
    (when mm/db-add-paths
      (let ((add-paths (copy-list mm/db-add-paths)))
	(mm/log (concat mm/mu-binary " add " (mapconcat 'identity add-paths " ")))
	(setq mm/db-add-paths nil) ;; clear the old list
	(setq mm/db-update-proc
	  (apply 'start-process mm/db-update-name mm/db-update-name mm/mu-binary
	    "add" add-paths))
	(set-process-sentinel mm/db-update-proc 'mm/db-update-proc-sentinel))))

(defun mm/db-add-async (path-or-paths)
  "Asynchronously add msg at PATH-OR-PATHS to
database. PATH-OR-PATHS is either a single path or a list of them."
  (setq mm/db-add-paths
    (append mm/db-add-paths
      (if (listp path-or-paths) path-or-paths `(,path-or-paths)))))
;;  (mm/db-update-execute))

(defun mm/db-remove-async (path-or-paths)
  "Asynchronously remove msg at PATH-OR-PATHS from
database. PATH-OR-PATHS is either a single path or a list of
them."
  (setq mm/db-remove-paths
    (append mm/db-remove-paths
      (if (listp path-or-paths) path-or-paths `(,path-or-paths)))))
;;  (mm/db-update-execute))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;;; error codes / names ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generated with:
;; cat mu-util.h | sed 's/\([A-Z_]\+\).*=\(.*\),/(defconst \L\1 \2)/' < "$<" \
;;     	| sed 's/_/-/g' > mu-errors.el
(defconst mm/err				1)
(defconst mm/err-in-parameters			2)
(defconst mm/err-internal			3)
(defconst mm/err-no-matches			4)
(defconst mm/err-xapian				11)
(defconst mm/err-xapian-query			13)
(defconst mm/err-xapian-dir-not-accessible	14)
(defconst mm/err-xapian-not-up-to-date		15)
(defconst mm/err-xapian-missing-data		16)
(defconst mm/err-xapian-corruption		17)
(defconst mm/err-xapian-cannot-get-writelock	18)
(defconst mm/err-gmime				30)
(defconst mm/err-contacts			50)
(defconst mm/err-contacts-cannot-retrieve	51)
(defconst mm/err-file				70)
(defconst mm/err-file-invalid-name		71)
(defconst mm/err-file-cannot-link		72)
(defconst mm/err-file-cannot-open		73)
(defconst mm/err-file-cannot-read		74)
(defconst mm/err-file-cannot-create		75)
(defconst mm/err-file-cannot-mkdir		76)
(defconst mm/err-file-stat-failed		77)
(defconst mm/err-file-readdir-failed		78)
(defconst mm/err-file-invalid-source		79)
(defconst mm/err-file-target-equals-source	80)

;; TODO: use 'case' instead...
(defun mm/error-string (err)
  "Convert an exit code from mu into a string."
  (cond
    ((eql err mm/err)				"General error")
    ((eql err mm/err-in-parameters)		"Error in parameters")
    ((eql err mm/err-internal)			"Internal error")
    ((eql err mm/err-no-matches)		"No matches")
    ((eql err mm/err-xapian)			"Xapian error")
    ((eql err mm/err-xapian-query)		"Error in query")
    ((eql err mm/err-xapian-dir-not-accessible)	"Database dir not accessible")
    ((eql err mm/err-xapian-not-up-to-date)	"Database is not up-to-date")
    ((eql err mm/err-xapian-missing-data)	"Missing data")
    ((eql err mm/err-xapian-corruption)		"Database seems to be corrupted")
    ((eql err mm/err-xapian-cannot-get-writelock)"Database is locked")
    ((eql err mm/err-gmime)			"GMime-related error")
    ((eql err mm/err-contacts)			"Contacts-related error")
    ((eql err mm/err-contacts-cannot-retrieve)	"Failed to retrieve contacts")
    ((eql err mm/err-file)			"File error")
    ((eql err mm/err-file-invalid-name)		"Invalid file name")
    ((eql err mm/err-file-cannot-link)		"Failed to link file")
    ((eql err mm/err-file-cannot-open)		"Cannot open file")
    ((eql err mm/err-file-cannot-read)		"Cannot read file")
    ((eql err mm/err-file-cannot-create)	"Cannot create file")
    ((eql err mm/err-file-cannot-mkdir)		"mu-mkdir failed")
    ((eql err mm/err-file-stat-failed)		"stat(2) failed")
    ((eql err mm/err-file-readdir-failed)	"readdir failed")
    ((eql err mm/err-file-invalid-source)	"Invalid source file")
    ((eql err mm/err-file-target-equals-source)	"Source is same as target")
    (t (format					"Unknown error (%d)" err))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;; other helper function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm/mu-run (&rest args)
  "Run 'mu' synchronously with ARGS as command-line argument;,
where <exit-code> is the exit code of the program, or 1 if the
process was killed. <str> contains whatever the command wrote on
standard output/error, or nil if there was none or in case of
error. `mm/mu-run' is like `shell-command-to-string', but with
better possibilities for error handling. The --muhome= parameter is
added automatically if `mm/mu-home' is non-nil."
  (let* ((rv)
	  (allargs (remove-if 'not
		     (append args (when mm/mu-home (concat "--muhome=" mm/mu-home)))))
	  (cmdstr (concat mm/mu-binary " " (mapconcat 'identity allargs " ")))
	  (str (with-output-to-string
		 (with-current-buffer standard-output ;; but we also get stderr...
		   (setq rv (apply 'call-process mm/mu-binary nil t nil
			      args))))))
    (mm/log "%s %s => %S" mm/mu-binary (mapconcat 'identity args " ") rv)
    (when (and (numberp rv) (/= 0 rv))
      (error (mm/error-string rv)))
    `(,(if (numberp rv) rv 1) . ,str)))


(defun mm/ask-maildir (prompt &optional fullpath)
  "Ask user with PROMPT for a maildir name, if fullpath is
non-nill, return the fulpath (i.e., `mm/maildir' prepended to the
chosen folder)."
  (unless (and mm/inbox-folder mm/drafts-folder mm/sent-folder)
    (error "`mm/inbox-folder', `mm/drafts-folder' and
    `mm/sent-folder' must be set"))
  (unless mm/maildir (error "`mm/maildir' must be set"))
  (interactive)
  (let* ((showfolders
	   (append (list mm/inbox-folder mm/drafts-folder mm/sent-folder)
	     mm/working-folders))
	  (chosen (ido-completing-read prompt showfolders)))
    (concat (if fullpath mm/maildir "") chosen)))


(defun mm/new-buffer (bufname)
  "Return a new buffer BUFNAME; if such already exists, kill the
old one first."
  (when (get-buffer bufname)
    (progn
      (message (format "Killing %s" bufname))
      (kill-buffer bufname)))
  (get-buffer-create bufname))


(defconst mm/log-buffer-name "*mm-log*"
  "*internal* Name of the logging buffer.")

(defun mm/log (frm &rest args)
  "Write something in the *mm-log* buffer - mainly useful for debugging."
  (with-current-buffer (get-buffer-create mm/log-buffer-name)
    (goto-char (point-max))
    (insert (apply 'format (concat (format-time-string "%x %X " (current-time))
			     frm "\n") args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mm-common)
