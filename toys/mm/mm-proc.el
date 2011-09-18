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

(defvar mm/proc-error-func nil
  "*internal* A function called for each error returned from the
server process; the function is passed an error plist as
argument. See `mm/proc-filter' for the format.")

(defvar mm/proc-update-func nil
  "*internal* A function called for each :update sexp returned from
the server process; the function is passed a msg sexp as
argument. See `mm/proc-filter' for the format.")

(defvar mm/proc-remove-func nil
  "*internal* A function called for each :remove sexp returned from
the server process, when some message has been deleted. The
function is passed the docid of the removed message.")

(defvar mm/proc-view-func nil
  "*internal* A function called for each single message sexp
returned from the server process. The function is passed a message
sexp as argument. See `mm/proc-filter' for the
format.")

(defvar mm/proc-header-func nil
  "*internal* A function called for each message returned from the
server process; the function is passed a msg plist as argument. See
`mm/proc-filter' for the format.")

(defvar mm/proc-compose-func nil
  "*internal* A function called for each message returned from the
server process that is used as basis for composing a new
message (ie., either a reply or a forward); the function is passed
msg and a symbol (either reply or forward). See `mm/proc-filter'
for the format of <msg-plist>.")

(defvar mm/proc-info-func nil
  "*internal* A function called for each (:info type ....) sexp
received from the server process.")


(defvar mm/buf nil
  "*internal* Buffer for results data.")

(defun mm/proc-info-handler (info)
  "Handler function for (:info ...) sexps received from the server
process."
  (let ((type (plist-get info :info)))
    (cond
      ;; (:info :version "3.1")
      ((eq type 'version) (setq mm/mu-version (plist-get info :version)))
      ((eq type 'index)
	(if (eq (plist-get info :status) 'running)
	  (message (format "Indexing... processed %d, updated %d"
		     (plist-get info :processed) (plist-get info :updated)))
	  (message
	    (format "Indexing completed; processed %d, updated %d, cleaned-up %d"
	      (plist-get info :processed) (plist-get info :updated)
	      (plist-get info :cleaned-up))))))))


(defun mm/start-proc ()
  "Start the mu server process."
  ;; TODO: add version check
  (unless (file-executable-p mm/mu-binary)
    (error (format "%S is not executable" mm/mu-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
	  (args '("server"))
	  (args (append args (when mm/mu-home
			       (list (concat "--muhome=" mm/mu-home))))))
    (setq mm/buf "")
    (setq mm/mu-proc (apply 'start-process "*mu-server*" "*mu-server*"
		       mm/mu-binary args))
    ;; register a function for (:info ...) sexps
    (setq mm/proc-info-func 'mm/proc-info-handler)
    (when mm/mu-proc
      (set-process-filter mm/mu-proc 'mm/proc-filter)
      (set-process-sentinel mm/mu-proc 'mm/proc-sentinel))))

(defun mm/kill-proc ()
  "Kill the mu server process."
  (when (mm/proc-is-running)
    (let ((delete-exited-processes t))
      (kill-process mm/mu-proc)
      (setq
	mm/mu-proc nil
	mm/buf nil))))

(defun mm/proc-is-running ()
  (and mm/mu-proc (eq (process-status mm/mu-proc) 'run)))

(defun mm/proc-eat-sexp-from-buf ()
  "'Eat' the next s-expression from `mm/buf'. `mm/buf gets its
  contents from the mu-servers in the following form:
       \376<len-of-sexp>\376<sexp>
Function returns this sexp, or nil if there was none. `mm/buf' is
updated as well, with all processed sexp data removed."
  (let* ((b (string-match "\376\\([0-9]+\\)\376" mm/buf))
	  (sexp-len
	    (when b (string-to-number (match-string 1 mm/buf)))))
    ;; does mm/buf contain the full sexp?
    (when (and b (>= (length mm/buf) (+ sexp-len (match-end 0))))
      ;; clear-up start
      (setq mm/buf (substring mm/buf (match-end 0)))
      (let ((objcons (read-from-string mm/buf)))
	(setq mm/buf (substring mm/buf sexp-len))
	(car objcons)))))


(defun mm/proc-filter (proc str)
  "A process-filter for the 'mu server' output; it accumulates the
  strings into valid sexps by checking of the ';;eox' end-of-sexp
  marker, and then evaluating them.

  The server output is as follows:

   1. an error
      (:error 2 :error-message \"unknown command\")
      ;; eox
   => this will be passed to `mm/proc-error-func'.

   2. a message sexp looks something like:
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
   => this will be passed to `mm/proc-header-func'.

  3. a view looks like:
  (:view <msg-sexp>)
  => the <msg-sexp> (see 2.) will be passed to `mm/proc-view-func'.

  4. a database update looks like:
  (:update <msg-sexp> :move <nil-or-t>)

   => the <msg-sexp> (see 2.) will be passed to
   `mm/proc-update-func', :move tells us whether this is a move to
   another maildir, or merely a flag change.

  5. a remove looks like:
  (:remove <docid>)
  => the docid will be passed to `mm/proc-remove-func'

  6. a compose looks like:
  (:compose <msg-sexp> :action <reply|forward>) => the <msg-sexp>
  and either 'reply or 'forward will be passed
  `mm/proc-compose-func'."
  (setq mm/buf (concat mm/buf str)) ;; update our buffer
  (let ((sexp (mm/proc-eat-sexp-from-buf)))
    (while sexp
      (mm/proc-log "%S" sexp)
      (cond
	((eq (plist-get sexp :msgtype) 'header)
	  (funcall mm/proc-header-func sexp))
	((plist-get sexp :view)
	  (funcall mm/proc-view-func (plist-get sexp :view)))
	((plist-get sexp :update)
	  (funcall mm/proc-update-func
	    (plist-get sexp :update) (plist-get sexp :move)))
	((plist-get sexp :remove)
	  (funcall mm/proc-remove-func (plist-get sexp :remove)))
	((plist-get sexp :compose)
	  (funcall mm/proc-compose-func
	    (plist-get sexp :compose)
	    (plist-get sexp :action)))
	((plist-get sexp :info)
	  (funcall mm/proc-info-func sexp))
	((plist-get sexp :error)
	  (funcall mm/proc-error-func sexp))
	(t (message "Unexpected data from server [%S]" sexp)))
      (setq sexp (mm/proc-eat-sexp-from-buf)))))


(defun mm/proc-sentinel (proc msg)
  "Function that will be called when the mu-server process
terminates."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq mm/mu-proc nil)
    (setq mm/buf "") ;; clear any half-received sexps
    (cond
      ((eq status 'signal)
	(cond
	  ((eq code 9) (message "the mu server process has been stopped"))
	  (t (message (format "mu server process received signal %d" code)))))
      ((eq status 'exit)
	(cond
	  ((eq code 11) (message "Database is locked by another process"))
	  (t (message (format "mu server process ended with exit code %d" code)))))
      (t
	(message "something bad happened to the mu server process")))))


(defconst mm/proc-log-buffer-name "*mm-log*"
  "*internal* Name of the logging buffer.")

(defun mm/proc-log (frm &rest args)
  "Write something in the *mm-log* buffer - mainly useful for debugging."
  (with-current-buffer (get-buffer-create mm/proc-log-buffer-name)
    (goto-char (point-max))
    (insert (apply 'format (concat (format-time-string "%Y-%m-%d %T "
				     (current-time)) frm "\n") args))))

(defun mm/proc-send-command (frm &rest args)
  "Send as command to the mu server process; start the process if needed."
  (unless (mm/proc-is-running)
    (mm/start-proc))
  (let ((cmd (apply 'format frm args)))
    (mm/proc-log cmd)
    (process-send-string mm/mu-proc (concat cmd "\n"))))


(defun mm/proc-remove-msg (docid)
  "Remove message identified by DOCID. The results are reporter
through either (:update ... ) or (:error ) sexp, which are handled
my `mm/proc-update-func' and `mm/proc-error-func', respectively."
  (mm/proc-send-command "remove %d" docid))


(defun mm/proc-find (expr)
  "Start a database query for EXPR. For each result found, a
function is called, depending on the kind of result. The variables
`mm/proc-header-func' and `mm/proc-error-func' contain the function
that will be called for, resp., a message (header row) or an
error."
  (mm/proc-send-command "find \"%s\"" expr))


(defun mm/proc-move-msg (docid targetmdir &optional flags)
  "Move message identified by DOCID to TARGETMDIR, optionally
setting FLAGS in the process.

TARGETDIR must be a maildir, that is, the part _without_ cur/ or
new/ or the root-maildir-prefix. E.g. \"/archive\". This directory
must already exist.

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
    ((flagstr (if (stringp flags) flags (mm/flags-to-string flags)))
      (fullpath (concat mm/maildir targetmdir)))
    (unless (and (file-directory-p fullpath) (file-writable-p fullpath))
      (error "Not a writable directory: %s" fullpath))
    (mm/proc-send-command "move %d %s %s" docid targetmdir flagstr)))


(defun mm/proc-flag-msg (docid flags)
  "Set FLAGS for the message identified by DOCID."
  (let ((flagstr (if (stringp flags) flags (mm/flags-to-string flags))))
    (mm/proc-send-command "flag %d %s" docid flagstr)))

(defun mm/proc-index (maildir)
  "Update the message database."
  (mm/proc-send-command "index %s" maildir))

(defun mm/proc-view-msg (docid)
  "Get one particular message based on its DOCID. The result will
be delivered to the function registered as `mm/proc-message-func'."
  (mm/proc-send-command "view %d" docid))

(defun mm/proc-compose-msg (docid reply-or-forward)
  "Start composing a message as either a forward or reply to
message with DOCID. REPLY-OR-FORWARD is either 'reply or 'forward.

The result will be delivered to the function registered as
`mm/proc-compose-func'."
  (let ((action (cond
		  ((eq reply-or-forward 'forward) "forward")
		  ((eq reply-or-forward 'reply) "reply")
		  (t (error "symbol must be eiter 'reply or 'forward")))))
    (mm/proc-send-command "compose %s %d" action docid)))


(provide 'mm-proc)
