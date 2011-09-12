;;; mm-proc.el -- part of mm, the mu mail user agent
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

(require 'mm-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal vars

(defvar mm/mu-proc nil
  "*internal* The mu-server process")

(defvar mm/proc-header-func nil
  "*internal* A function called for each message returned from the
server process; the function is passed a msg plist as argument. See
`mm/proc-eval-server-output' for the format.")

(defvar mm/proc-error-func nil
  "*internal* A function called for each error returned from the
server process; the function is passed an error plist as
argument. See `mm/proc-eval-server-output' for the format.")

(defvar mm/proc-update-func nil
  "*internal* A function called for each update sexp returned from
the server process; the function is passed an update plist as
argument. See `mm/proc-eval-server-output' for the format.")

(defvar mm/proc-message-func nil
  "*internal* A function called for each message sexp returned from
the server process. This is designed for viewing a message. See
`mm/proc-eval-server-output' for the format.")


(defconst mm/eox-mark "\n;;eox\n"
  "*internal* Marker for the end of a sexp")

(defvar mm/buf ""
  "*internal* Buffer for results data.")

(defun mm/start-proc ()
  "Start the mu server process."
  ;; TODO: add version check
  (unless (file-executable-p mm/mu-binary)
    (error (format "%S is not executable" mm/mu-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
	  (args '("server"))
	  (args (append args (when mm/mu-home
			       (list (concat "--muhome=" mm/mu-home))))))
    (setq mm/mu-proc (apply 'start-process "*mu-server*" "*mu-server*"
		       mm/mu-binary args))
    (when mm/mu-proc
      (set-process-filter mm/mu-proc 'mm/proc-filter)
      (set-process-sentinel mm/mu-proc 'mm/proc-sentinel))))

(defun mm/kill-proc ()
  "Kill the mu server process."
  (when (mm/proc-is-running)
    (let ((delete-exited-processes t))
      (kill-process mm/mu-proc)
      (setq mm/mu-proc nil))))

(defun mm/proc-is-running ()
  (and mm/mu-proc (eq (process-status mm/mu-proc) 'run)))


(defun mm/proc-filter (proc str)
  "A process-filter for the 'mu server' output; it accumulates the
  strings into valid sexps by checking of the ';;eox' end-of-msg
  marker, and then evaluating them."
  (setq mm/buf (concat mm/buf str)) ;; update our buffer
  (let ((eox (string-match mm/eox-mark mm/buf)))
    (while eox
      ;; Process the sexp in `mm/buf', and remove it if it worked and return
      ;; t. If no complete sexp is found, return nil."
      (let ( (after-eox (match-end 0))
	     (sexp (mm/proc-eval-server-output (substring mm/buf 0 eox))))
	;; the sexp we get can either be a message or an error
	(message "[%S]" sexp)
	(cond
	  ((plist-get sexp :error)  (funcall mm/proc-error-func sexp))
	  ;; if it has :docid, it's a message; if it's dbonly prop is `t', it's
	  ;; a header, otherwise it's a message (for viewing)
	  ((eq (plist-get sexp :msgtype) 'header)
	    (funcall mm/proc-header-func sexp))
	  ((eq (plist-get sexp :msgtype) 'view)
	    (funcall mm/proc-message-func sexp))
	  ((plist-get sexp :update) (funcall mm/proc-update-func sexp))
	  (t (message "%S" sexp)))
	;;(t (error "Unexpected data from server"))))
	(setq mm/buf (substring mm/buf after-eox)))
      (setq eox (string-match mm/eox-mark mm/buf)))))

(defun mm/proc-sentinel (proc msg)
  "Function that will be called when the mu-server process
terminates."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq mm/mu-proc nil)
    (setq mm/buf "") ;; clear any half-received sexps
    (cond
      ((eq status 'signal)
	(message (format "mu server process received signal %d" code)))
      ((eq status 'exit)
	(cond
	  ((eq code 11) (message "Database is locked by another process"))
	  (t (message (format "mu server process ended with exit code %d" code)))))
      (t
	(message "something bad happened to the mu server process")))))

(defun mm/proc-eval-server-output (str)
  "Evaluate a blob of server output; the output describe either a
message, a database update or an error.

An error sexp looks something like:

  (:error 2 :error-message \"unknown command\")
;; eox

a message sexp looks something like:

 \(
  :docid 1585
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
;; eox

a database update looks like:
\(:update 1585 :path \"/home/user/Maildir/foo/cur/12323213:,R\")
  when a message has been moved to a new location, or
\(:update 1585 :path \"/dev/null\")
  when it has been removed.

other fields are :cc, :bcc, :body-html

When the s-expression comes from the database ('mu find'), the
fields :attachments, :body-txt, :body-html, :references, :in-reply-to
are missing (because that information is not stored in the
database).

On the other hand, if the information comes from the message file,
there won't be a :docid field."
  (condition-case nil
    (car (read-from-string str));; read-from-string returns a cons
    (error "Failed to parse sexp [%S]" str)))


(defun mm/proc-remove-msg (docid)
  "Remove message identified by DOCID. The results are reporter
through either (:update ... ) or (:error ) sexp, which are handled
my `mm/proc-update-func' and `mm/proc-error-func', respectively."
  (unless (mm/proc-is-running) (mm/start-proc))
  (when mm/mu-proc
    (process-send-string mm/mu-proc (format "remove %d\n" docid))))


(defun mm/proc-find (expr)
  "Start a database query for EXPR. For each result found, a
function is called, depending on the kind of result. The variables
`mm/proc-header-func' and `mm/proc-error-func' contain the function
that will be called for, resp., a message (header row) or an
error."
  (unless (mm/proc-is-running) (mm/start-proc))
  (when mm/mu-proc
    (process-send-string mm/mu-proc (format "find %s\n" expr))))


(defun mm/proc-move-msg (docid targetdir flags)
  "Move message identified by DOCID to TARGETDIR, setting FLAGS in
the process.

TARGETDIR must be a maildir, that is, the part _without_ cur/ or
new/.

The FLAGS parameter can have the following forms:
  1. a list of flags such as '(passed replied seen)
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mm/string-to-flags' and `mm/flags-to-string'.

The server reports the results for the operation through
`mm/proc-update-func'.

The results are reported through either (:update ... )
or (:error ) sexp, which are handled my `mm/proc-update-func' and
`mm/proc-error-func', respectively."
  (let
    ((flagstr (if (stringp flags) flags (mm/flags-to-string flags))))
    (unless (and (file-directory-p targetdir) (file-writable-p targetdir))
      (error "Not a writable directory: %s" targetdir))

    (unless (mm/proc-is-running) (mm/start-proc))
    (when mm/mu-proc
      (process-send-string mm/mu-proc
	(format "move %d %s %s\n" docid targetdir flagstr)))))


(defun mm/proc-flag-msg (docid flags)
  "Set FLAGS for the message identified by DOCID."
  (let ((flagstr (if (stringp flags) flags (mm/flags-to-string flags))))
    (unless (mm/proc-is-running) (mm/start-proc))
    (when mm/mu-proc
      (process-send-string mm/mu-proc
	(format "flag %d %s\n" docid flagstr)))))


(defun mm/proc-view-msg (docid)
  "Get one particular message based on its DOCID. The result will
be delivered to the function registered as `mm/proc-message-func'."
  (unless (mm/proc-is-running) (mm/start-proc))
  (when mm/mu-proc
    (process-send-string mm/mu-proc
      (format "view %d\n" docid))))


(provide 'mm-proc)
