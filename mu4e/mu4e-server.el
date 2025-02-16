;;; mu4e-server.el --- Control mu server from mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Dirk-Jan C. Binnema

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

;;; Code:

(require 'mu4e-helpers)


;;; Configuration
(defcustom mu4e-mu-home nil
  "Location of an alternate mu home directory.
If not set, use the defaults, based on the XDG Base Directory
Specification.

Changes to this value only take effect after (re)starting the mu
session."
  :group 'mu4e
  :type '(choice (const :tag "Default location" nil)
                 (directory :tag "Specify location"))
  :safe 'stringp)

(defcustom mu4e-mu-binary (executable-find "mu")
  "Path to the mu-binary to use.

Changes to this value only take effect after (re)starting the mu
session."
  :type '(file :must-match t)
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-mu-debug nil
  "Whether to run the mu binary in debug-mode.
Setting this to t increases the amount of information in the log.

Changes to this value only take effect after (re)starting the mu
session."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-change-filenames-when-moving nil
  "Change message file names when moving them.

When moving messages to different folders, normally mu/mu4e keep
the base filename the same (the flags-part of the filename may
change still). With this option set to non-nil, mu4e instead
changes the filename.

This latter behavior works better with some
IMAP-synchronization programs such as mbsync; the default works
better with e.g. offlineimap."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-mu-allow-temp-file nil
  "Allow using temp-files for optimizing mu <-> mu4e communication.

Some commands - in particular \"find\" and \"contacts\" - return
big s-expressions; and it turns out that reading those is faster
by passing them through a temp file rather than through normal
stdin/stdout channel - esp. on the (common case) where the
file-system for temp-files is in-memory.

To see if the helps, you can benchmark the rendering with
     (setq mu4e-headers-report-render-time t)

and compare the results with `mu4e-mu-allow-temp' set and unset.

Note: for a change to this variable to take effect, you need to
stop/start mu4e."
  :type  'boolean
  :group 'mu4e
  :safe  'booleanp)

;; Cached data
(defvar mu4e-maildir-list)

;; Handlers are not strictly internal, but are not meant
;; for overriding outside mu4e. The are mainly for breaking
;; dependency cycles.

(defvar mu4e-error-func nil
  "Function called for each error received.
The function is passed an error plist as argument. See
`mu4e--server-filter' for the format.")

(defvar mu4e-update-func nil
  "Function called for each :update sexp returned.
The function is passed a msg sexp as argument.
See `mu4e--server-filter' for the format.")

(defvar mu4e-remove-func nil
  "Function called for each :remove sexp returned.
This happens when some message has been deleted. The function is
passed the docid of the removed message.")

(defvar mu4e-view-func  nil
  "Function called for each single-message sexp.
The function is passed a message sexp as argument. See
`mu4e--server-filter' for the format.")

(defvar mu4e-headers-append-func nil
  "Function called with a list of headers to append.
The function is passed a list of message plists as argument. See
See `mu4e--server-filter' for the details.")

(defvar mu4e-found-func nil
  "Function called for when we received a :found sexp.
This happens after the headers have been returned, to report on
the number of matches. See `mu4e--server-filter' for the format.")

(defvar mu4e-erase-func nil
  "Function called we receive an :erase sexp.
This before new headers are displayed, to clear the current
headers buffer. See `mu4e--server-filter' for the format.")

(defvar mu4e-info-func nil
  "Function called for each (:info type ....) sexp received.
from the server process.")

(defvar mu4e-pong-func nil
  "Function called for each (:pong type ....) sexp received.")

(defvar mu4e-queries-func nil
  "Function called for each (:queries type ....) sexp received.")

(defvar mu4e-contacts-func nil
  "A function called for each (:contacts (<list-of-contacts>))
sexp received from the server process.")

;;; Dealing with Server properties
(defvar mu4e--server-props nil
  "Metadata we receive from the mu4e server.")

(defun mu4e-server-properties ()
  "Get the server metadata plist."
  mu4e--server-props)

(defun mu4e-root-maildir()
  "Get the root maildir."
  (or (and mu4e--server-props
           (plist-get mu4e--server-props :root-maildir))
      (mu4e-error "Root maildir unknown; did you start mu4e?")))

(defun mu4e-database-path()
  "Get the root maildir."
  (or (and mu4e--server-props
           (plist-get mu4e--server-props :database-path))
      (mu4e-error "Root maildir unknown; did you start mu4e?")))

(defun mu4e-server-version()
  "Get the root maildir."
  (or (and mu4e--server-props
           (plist-get mu4e--server-props :version))
      (mu4e-error "Version unknown; did you start mu4e?")))

;;; remember query results.
(defvar mu4e--server-query-items nil
  "Query items results we receive from the mu4e server.
Those are the results from the counting-queries
for bookmarks and maildirs.")

(defun mu4e-server-query-items ()
  "Get the latest server query items."
  mu4e--server-query-items)

(defvar mu4e--server-query nil
  "Last query executed by the server.
This is a plist, see `mu4e-server-last-query' for details.")

(defun mu4e-server-last-query ()
  "Get a plist with information about the last server-query.

This has the following fields:
- :query: this is the last query the server executed (a string)
- :query-sexp: this is that last query as processed by the query engine
  (an s-expression as a string)
- :query-sexp-expanded: like :query-sexp, but with combination fields
   expanded (if any)."
  (cl-remf mu4e--server-query :found) ;; there's no plist-delete
  mu4e--server-query)

(defvar mu4e--last-query-buffer-name)
(defun mu4e-analyze-last-query ()
  "Pop-up a buffer with the most recent query as the server saw it.
See `mu4e-server-last-query' for the fields.
It is the mu4e-version of \"mu find <query> --analyze\"."
  (interactive)
  (mu4e--popup-lisp-buffer
   mu4e--last-query-buffer-name
   (mu4e-server-last-query)))

;;; Handling raw server data

(defvar mu4e--server-buf nil
  "Buffer (string) for data received from the backend.")
(defconst mu4e--server-name " *mu4e-server*"
  "Name of the server process, buffer.")
(defvar mu4e--server-process nil
  "The mu-server process.")

;; dealing with the length cookie that precedes expressions
(defconst mu4e--server-cookie-pre "\376"
  "Each expression starts with a length cookie:
<`mu4e--server-cookie-pre'><length-in-hex><`mu4e--server-cookie-post'>.")
(defconst mu4e--server-cookie-post "\377"
  "Each expression starts with a length cookie:
<`mu4e--server-cookie-pre'><length-in-hex><`mu4e--server-cookie-post'>.")
(defconst mu4e--server-cookie-matcher-rx
  (concat mu4e--server-cookie-pre "\\([[:xdigit:]]+\\)"
          mu4e--server-cookie-post)
  "Regular expression matching the length cookie.
Match 1 will be the length (in hex).")

(defvar mu4e--server-indexing nil "Currently indexing?")

(defun mu4e-running-p ()
  "Whether mu4e is running.
Checks whether the server process is live."
  (and mu4e--server-process
       (memq (process-status mu4e--server-process)
             '(run open listen connect stop)) t))

(declare-function mu4e--massage-addresses "mu4e-contacts")

(defsubst mu4e--server-eat-sexp-from-buf ()
  "Eat the next s-expression from `mu4e--server-buf'.
Note: this is a string, not an emacs-buffer. `mu4e--server-buf gets
its contents from the mu-servers in the following form:
   <`mu4e--server-cookie-pre'><length-in-hex><`mu4e--server-cookie-post'>
Function returns this sexp, or nil if there was none.
`mu4e--server-buf' is updated as well, with all processed sexp data
removed."
  (ignore-errors ;; the server may die in the middle...
    (let ((b (string-match mu4e--server-cookie-matcher-rx mu4e--server-buf))
          (sexp-len) (objcons))
      (when b
        (setq sexp-len (string-to-number (match-string 1 mu4e--server-buf) 16))
        ;; does mu4e--server-buf contain the full sexp?
        (when (>= (length mu4e--server-buf) (+ sexp-len (match-end 0)))
          ;; clear-up start
          (setq mu4e--server-buf (substring mu4e--server-buf (match-end 0)))
          ;; note: we read the input in binary mode -- here, we take the part
          ;; that is the sexp, and convert that to utf-8, before we interpret
          ;; it.
          (setq objcons (read-from-string
                         (decode-coding-string
                          (substring mu4e--server-buf 0 sexp-len)
                          'utf-8 t)))
          (when objcons
            (setq mu4e--server-buf (substring mu4e--server-buf sexp-len))
            (car objcons)))))))

(defun mu4e--server-plist-get (plist key)
  "Like `plist-get' but load data from file if it is a string.

PLIST is a property-list, and KEY is the the key to search for.


E.g., (mu4e--server-plist-get (:foo bar) :foo)
  => bar
but
     (mu4e--server-plist-get (:foo \"/tmp/data.eld\") :foo)
  => evaluates the contents of /tmp/data.eld
   (and deletes the file afterward).

This for the few sexps we get from the mu server that support
 this -- headers, contacts, maildirs."
  ;; XXX: perhaps re-use the same buffer?
  (let ((val (plist-get plist key)))
    (if (stringp val)
        (with-temp-buffer
          (insert-file-contents val)
          (goto-char (point-min))
          (delete-file val)
          (read (current-buffer)))
      val)))

(defun mu4e--server-filter (_proc str)
  "Filter string STR from PROC.
This processes the \"mu server\" output. It accumulates the
strings into valid s-expressions and evaluates those.

The server output is as follows:

   1. an error
      (:error 2 :message \"unknown command\")
      ;; eox
   => passed to `mu4e-error-func'.

   2a. a header exp looks something like:
  (:headers
      ( ;; message 1
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
        :path \"/home/mickey/Maildir/inbox/cur/1312_3.32282.pluto,4cd5bd4e9:2,\"
        :priority high
        :flags (new unread)
        :meta <meta-data>
       )
       (  .... more messages  )
)
;; eox
   => this will be passed to `mu4e-headers-append-func'.

  2b. After the list of headers has been returned (see 2a.),
  we'll receive a sexp that looks like
  (:found <n>) with n the number of messages found. The <n> will be
  passed to `mu4e-found-func'.

  3. a view looks like:
  (:view <msg-sexp>)
  => the <msg-sexp> (see 2.) will be passed to `mu4e-view-func'.
     the <msg-sexp> also contains :body

  4. a database update looks like:
  (:update <msg-sexp> :move <nil-or-t>)
    like :header

   => the <msg-sexp> (see 2.) will be passed to
   `mu4e-update-func', :move tells us whether this is a move to
   another maildir, or merely a flag change.

  5. a remove looks like:
  (:remove <docid>)
  => the docid will be passed to `mu4e-remove-func'

  6. a compose looks like:
  (:compose <reply|forward|edit|new> [:original<msg-sexp>] [:include <attach>])
  `mu4e-compose-func'. :original looks like :view."
  (mu4e-log 'misc "* Received %d byte(s)" (length str))
  (setq mu4e--server-buf (concat mu4e--server-buf str)) ;; update our buffer
  (let ((sexp (mu4e--server-eat-sexp-from-buf)))
    (with-local-quit
      (while sexp
        (mu4e-log 'from-server "%s" sexp)
        (cond
         ;; a list of messages (after a find command)
         ((plist-get sexp :headers)
          (funcall mu4e-headers-append-func
                   (mu4e--server-plist-get sexp :headers)))

         ;; the found sexp, we receive after getting all the headers
         ((plist-get sexp :found)
          ;; capture the query-info
          (setq mu4e--server-query sexp)
          (funcall mu4e-found-func (plist-get sexp :found)))

         ;; viewing a specific message
         ((plist-get sexp :view)
          (funcall mu4e-view-func (plist-get sexp :view)))

         ;; receive an erase message
         ((plist-get sexp :erase)
          (funcall mu4e-erase-func))

         ;; received a pong message
         ((plist-get sexp :pong)
          (let ((props (plist-get sexp :props)))
            ;; attempt to translate the regex-style.
            (plist-put props :personal-addresses
                       (mu4e--massage-addresses
                        (plist-get props :personal-addresses)))
            (setq mu4e--server-props props))
          (funcall mu4e-pong-func sexp))

         ;; receive queries info
         ((plist-get sexp :queries)
          (setq mu4e--server-query-items (plist-get sexp :queries))
          (funcall mu4e-queries-func sexp))

         ;; received a contacts message
         ;; note: we use 'member', to match (:contacts nil)
         ((plist-member sexp :contacts)
          (funcall mu4e-contacts-func
                   (mu4e--server-plist-get sexp :contacts)
                   (plist-get sexp :tstamp)))

         ;; something got moved/flags changed
         ((plist-get sexp :update)
          (funcall mu4e-update-func
                   (plist-get sexp :update)
                   (plist-get sexp :move)
                   (plist-get sexp :maybe-view)))

         ;; a message got removed
         ((plist-get sexp :remove)
          (funcall mu4e-remove-func (plist-get sexp :remove)))

         ;; get some info
         ((plist-get sexp :info)
          ;; when indexing is finished, remove the block
          (when (and (eq (plist-get sexp :info) 'index)
                     (eq (plist-get sexp :status) 'complete))
            (setq mu4e--server-indexing nil))

          (funcall mu4e-info-func sexp))

         ;; get some data
         ((plist-get sexp :maildirs)
          (setq mu4e-maildir-list (mu4e--server-plist-get sexp :maildirs)))

         ;; receive an error
         ((plist-get sexp :error)
          (funcall mu4e-error-func
                   (plist-get sexp :error)
                   (plist-get sexp :message)))

         (t (mu4e-message "Unexpected data from server [%S]" sexp)))

        (setq sexp (mu4e--server-eat-sexp-from-buf))))))

(defun mu4e--kill-stale ()
  "Kill stale mu4e server process.
As per issue #2198."
  (seq-each
   (lambda(proc)
     (when (and (process-live-p proc)
                (string-prefix-p mu4e--server-name (process-name proc)))
       (mu4e-message "killing stale mu4e server")
       (ignore-errors
         (signal-process proc 'SIGINT) ;; nicely
         (sit-for 1.0)
         (signal-process proc 'SIGKILL)))) ;; forcefully
   (process-list)))

(defun mu4e--server-args()
  "Return the command line args for the command  to start the mu4e-server."
  ;; [--debug] server [--muhome=..]
  (seq-filter #'identity ;; filter out nil
              `(,(when mu4e-mu-debug "--debug")
                "server"
                ,(when mu4e-mu-allow-temp-file "--allow-temp-file")
                ,(when mu4e-mu-home (format "--muhome=%s" mu4e-mu-home)))))

(defun mu4e--version-check ()
  "Verify that the versions for mu4e and mu are the same."
  ;; sanity-check 1
  (let ((default-directory temporary-file-directory)) ;;ensure it's local.
    (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
      (mu4e-error
       "Cannot find mu, please set `mu4e-mu-binary' to the mu executable path"))
    ;; sanity-check 2
    (let ((version (let ((s (shell-command-to-string
                             (concat mu4e-mu-binary " --version"))))
                     (and (string-match "version \\(.+\\)" s)
                          (match-string 1 s)))))
      (if (not (string= version mu4e-mu-version))
          (mu4e-error
           (concat
            "Found mu version %s, but mu4e needs version %s"
            "; please set `mu4e-mu-binary' "
            "accordingly")
           version mu4e-mu-version)
        (mu4e-message "Found mu version %s" version)))))

(defun mu4e-server-repl ()
  "Start a mu4e-server repl.

This is meant for debugging/testing - the repl is designed for
machines, not for humans.

You cannot run the repl when mu4e is running (or vice-versa)."
  (interactive)
  (if (mu4e-running-p)
      (mu4e-error "Cannot run repl when mu4e is running")
    (progn
      (mu4e--version-check)
      (let ((cmd (string-join (cons mu4e-mu-binary (mu4e--server-args)) " ")))
        (term cmd)
        (rename-buffer "*mu4e-repl*" 'unique)
        (message "invoked: '%s'" cmd)))))

(defun mu4e--server-start ()
  "Start the mu server process."
  (mu4e--version-check)
  ;; kill old/stale servers, if any.
  (mu4e--kill-stale)
  (let* ((process-connection-type nil) ;; use a pipe
         (args (mu4e--server-args)))
    (setq mu4e--server-buf "")
    (mu4e-log 'misc "* invoking '%s' with parameters %s" mu4e-mu-binary
              (mapconcat (lambda (arg) (format "'%s'" arg)) args " "))
    (setq mu4e--server-process (apply 'start-process
                                      mu4e--server-name mu4e--server-name
                                      mu4e-mu-binary args))
    ;; register a function for (:info ...) sexps
    (unless mu4e--server-process
      (mu4e-error "Failed to start the mu4e backend"))
    (set-process-query-on-exit-flag mu4e--server-process nil)
    (set-process-coding-system mu4e--server-process 'binary 'utf-8-unix)
    (set-process-filter mu4e--server-process 'mu4e--server-filter)
    (set-process-sentinel mu4e--server-process 'mu4e--server-sentinel)))

(defun mu4e--server-kill ()
  "Kill the mu server process."
  (let* ((buf (get-buffer mu4e--server-name))
         (proc (and (buffer-live-p buf) (get-buffer-process buf))))
    (when proc
      (mu4e-message "shutting down")
      (setq mu4e--server-indexing nil)
      (set-process-filter mu4e--server-process nil)
      (set-process-sentinel mu4e--server-process nil)
      (let ((delete-exited-processes t))
        (mu4e--server-call-mu '(quit)))
      ;; try sending SIGINT (C-c) to process, so it can exit gracefully
      (ignore-errors
        (signal-process proc 'SIGINT))))
  (setq
   mu4e--server-process nil
   mu4e--server-buf nil))

;; error codes are defined in src/mu-util
;;(defconst mu4e-xapian-empty 19 "Error code: xapian is empty/non-existent")

(defun mu4e--server-sentinel (proc _msg)
  "Function called when the server process PROC terminates with MSG."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (mu4e-log 'misc "* famous last words from server: '%s'" mu4e--server-buf)
    (setq mu4e--server-process nil)
    (setq mu4e--server-buf "") ;; clear any half-received sexps
    (cond
     ((eq status 'signal)
      (cond
       ((or(eq code 9) (eq code 2)) (message nil))
       ;;(message "the mu server process has been stopped"))
       (t (mu4e-error (format "server process received signal %d" code)))))
     ((eq status 'exit)
      (cond
       ((eq code 0)
        (message nil)) ;; don't do anything
       ((eq code 11)
        (error "Schema mismatch; please re-init mu from command-line"))
       ((eq code 19)
        (error "Mu database is locked by another process"))
       (t (error "Mu server process ended with exit code %d" code))))
     (t
      (error "Something bad happened to the mu server process")))))

(declare-function mu4e "mu4e")
(defvar mu4e--initialized)

(defun mu4e--server-call-mu (form)
  "Call the mu server with some command FORM."
  (unless mu4e--initialized
    (mu4e 'background))
  ;; ensure the server is running as well
  (unless (mu4e-running-p)
    (mu4e--server-start))
  ;; in single-threaded mode, mu can't accept our command right now.
  (when mu4e--server-indexing
    (mu4e-message "Cannot handle command while indexing, please retry later."))
  (let* ((print-length nil) (print-level nil)
         (cmd (format "%S" form)))
    (mu4e-log 'to-server "%s" cmd)
    (process-send-string mu4e--server-process (concat cmd "\n"))))

(defun mu4e--server-add (path)
  "Add the message at PATH to the database.
On success, we receive `'(:info add :path <path> :docid <docid>)'
as well as `'(:update <msg-sexp>)`'; otherwise, we receive an error."
  (mu4e--server-call-mu `(add :path ,path)))

(defun mu4e--server-contacts (personal after maxnum tstamp)
  "Ask for contacts with PERSONAL AFTER MAXNUM TSTAMP.

S-expression (:contacts (<list>) :tstamp \"<tstamp>\")
is expected in response.

If PERSONAL is non-nil, only get personal contacts, if AFTER is
non-nil, get only contacts seen AFTER (the time_t value). If MAX is non-nil,
get at most MAX contacts."
  (mu4e--server-call-mu
   `(contacts
     :personal ,(and personal t)
     :after    ,(or after nil)
     :tstamp   ,(or tstamp nil)
     :maxnum   ,(or maxnum nil))))

(defun mu4e--server-data (kind)
  "Request data of some KIND.
KIND is a symbol. Currently supported kinds: maildirs."
  (mu4e--server-call-mu
   `(data :kind ,kind)))

(defun mu4e--server-find (query threads sortfield sortdir maxnum skip-dups
                                include-related)
  "Run QUERY with THREADS SORTFIELD SORTDIR MAXNUM SKIP-DUPS INCLUDE-RELATED.

If THREADS is non-nil, show results in threaded fashion,
SORTFIELD is a symbol describing the field to sort by (or nil);
see `mu4e~headers-sortfield-choices'. If SORT is `descending',
sort Z->A, if it's `ascending', sort A->Z. MAXNUM determines the
maximum number of results to return, or nil for unlimited. If
SKIP-DUPS is non-nil, show only one of duplicate messages (see
`mu4e-headers-skip-duplicates'). If INCLUDE-RELATED is non-nil,
include messages related to the messages matching the search
query (see `mu4e-headers-include-related').

For each result found, a function is called, depending on the
kind of result. The variables `mu4e-error-func' contain the
function that to be be called for, resp., a message (header)
or an error."
  (mu4e--server-call-mu
   `(find
     :query ,query
     :threads ,(and threads t)
     :sortfield ,sortfield
     :descending ,(if (eq sortdir 'descending) t nil)
     :maxnum ,maxnum
     :skip-dups ,(and skip-dups t)
     :include-related ,(and include-related t))))

(defun mu4e--server-index (&optional cleanup lazy-check)
  "Index messages.
If CLEANUP is non-nil, remove messages which are in the database
but no longer in the file system. If LAZY-CHECK is non-nil, only
consider messages for which the time stamp (ctime) of the
directory they reside in has not changed since the previous
indexing run. This is much faster than the non-lazy check, but
won't update messages that have change (rather than having been
added or removed), since merely editing a message does not update
the directory time stamp."
  (mu4e--server-call-mu
   `(index :cleanup ,(and cleanup t)
           :lazy-check ,(and lazy-check t)))
  (setq mu4e--server-indexing t)) ;; remember we're indexing.

(defun mu4e--server-mkdir (path &optional update)
  "Create a new maildir-directory at file system PATH.
When UPDATE is non-nil, send a update when completed.
PATH must be below the root-maildir."
  ;; handle maildir cache
  (if (not (string-prefix-p (mu4e-root-maildir) path))
      (mu4e-error "Cannot create maildir outside root-maildir")
    (add-to-list 'mu4e-maildir-list ;; update cache
                 (substring path (length (mu4e-root-maildir)))))
  (mu4e--server-call-mu `(mkdir
                          :path ,path
                          :update ,(or update nil))))

(defun mu4e--server-move (docid-or-msgid &optional maildir flags no-view)
  "Move message identified by DOCID-OR-MSGID.
Optionally to MAILDIR and optionally setting FLAGS. If MAILDIR is
nil, message will be moved within the same maildir.

At least one of MAILDIR and FLAGS must be specified. Note that
even when MAILDIR is nil, this is still a filesystem move, since
a change in flags implies a change in message filename.

MAILDIR must be a maildir, that is, the part _without_ cur/ or new/
or the root-maildir-prefix. E.g. \"/archive\". This directory must
already exist.

The FLAGS parameter can have the following forms:
  1. a list of flags such as `(passed replied seen)'
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mu4e-string-to-flags' and `mu4e-flags-to-string'.
The server reports the results for the operation through
`mu4e-update-func'.

If the variable `mu4e-change-filenames-when-moving' is
non-nil, moving to a different maildir generates new names for
the target files; this helps certain tools (such as mbsync).

If NO-VIEW is non-nil, do not update the view.

Returns either (:update ... ) or (:error ) sexp, which are handled by
`mu4e-update-func' and `mu4e-error-func', respectively."
  (unless (or maildir flags)
    (mu4e-error "At least one of maildir and flags must be specified"))
  (unless (or (not maildir)
              (file-exists-p
               (mu4e-join-paths (mu4e-root-maildir) maildir)))
    (mu4e-error "Target directory does not exist"))
  (mu4e--server-call-mu
   `(move
     :docid ,(if (stringp docid-or-msgid) nil docid-or-msgid)
     :msgid ,(if (stringp docid-or-msgid) docid-or-msgid nil)
     :flags ,(or flags nil)
     :maildir ,(or maildir nil)
     :rename  ,(and maildir mu4e-change-filenames-when-moving t)
     :no-view ,(and no-view t))))

(defun mu4e--server-ping ()
  "Sends a ping to the mu server, expecting a (:pong ...) in response."
  (mu4e--server-call-mu `(ping)))

(defun mu4e--server-queries (queries)
  "Sends queries to the mu server, expecting a (:queries ...) sexp in response.
QUERIES is a list of queries for the number of results with
read/unread status are returned in the pong-response."
  (mu4e--server-call-mu `(queries :queries ,queries)))

(defun mu4e--server-remove (docid-or-path)
  "Remove message with either DOCID-OR-PATH.
The results are reported through either (:update ... )
or (:error) sexps."
  (if (stringp docid-or-path)
      (mu4e--server-call-mu `(remove :path ,docid-or-path))
      (mu4e--server-call-mu `(remove :docid ,docid-or-path))))

(defun mu4e--server-view (docid-or-msgid &optional mark-as-read)
  "View a message referred to by DOCID-OR-MSGID.
Optionally, if MARK-AS-READ is non-nil, the backend marks the
message as \"read\" before returning, if not already. The result
will be delivered to the function registered as `mu4e-view-func'."
  (mu4e--server-call-mu
   `(view
     :docid ,(if (stringp docid-or-msgid) nil docid-or-msgid)
     :msgid ,(if (stringp docid-or-msgid) docid-or-msgid nil)
     :mark-as-read ,(and mark-as-read t)
     ;; when moving (due to mark-as-read), change filenames
     ;; if so configured. Note: currently this *ignored*
     ;; because mbsync seems to get confused.
     :rename  ,(and mu4e-change-filenames-when-moving t))))

(provide 'mu4e-server)
;;; mu4e-server.el ends here
