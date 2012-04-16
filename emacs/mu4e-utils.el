;;; mu4e-utils.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

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

;; Utility functions used in the mu4e

;;; Code:
(require 'cl)
(require 'html2text)
(require 'mu4e-vars)

(defun mu4e-create-maildir-maybe (dir)
  "Offer to create DIR if it does not exist yet. Return t if the
dir already existed, or has been created, nil otherwise."
  (if (and (file-exists-p dir) (not (file-directory-p dir)))
    (error "%s exists, but is not a directory." dir))
  (cond
    ((file-directory-p dir) t)
    ((yes-or-no-p (format "%s does not exist yes. Create now?" dir))
      (mu4e-proc-mkdir dir))
    (t nil)))

(defun mu4e-check-requirements ()
  "Check for the settings required for running mu4e."
  (unless (and mu4e-mu-binary (file-executable-p mu4e-mu-binary))
    (error "Please set `mu4e-mu-binary' to the full path to the mu
    binary."))
  (unless mu4e-maildir
    (error "Please set `mu4e-maildir' to the full path to your
    Maildir directory."))
  ;; expand mu4e-maildir, mu4e-attachment-dir
  (setq
    mu4e-maildir (expand-file-name mu4e-maildir)
    mu4e-attachment-dir (expand-file-name mu4e-attachment-dir))
  (unless (mu4e-create-maildir-maybe mu4e-maildir)
    (error "%s is not a valid maildir directory" mu4e-maildir))
  (dolist (var '( mu4e-sent-folder
		  mu4e-drafts-folder
		  mu4e-trash-folder))
    (unless (and (boundp var) (symbol-value var))
      (error "Please set %S" var))
    (let* ((dir (symbol-value var)) (path (concat mu4e-maildir dir)))
      (unless (string= (substring dir 0 1) "/")
	(error "%S must start with a '/'" dir))
      (unless (mu4e-create-maildir-maybe path)
	(error "%s (%S) does not exist" path var)))))

(defun mu4e-read-option (prompt options)
  "Ask user for an option from a list on the input area. PROMPT
describes a multiple-choice question to the user, OPTIONS describe
the options, and is a list of cells describing particular
options. Cells have the following structure:
   (OPTIONSTRING CHAR) where CHAR is a short-cut character for the
option, and OPTIONSTRING is a non-empty string describing the
option. If CHAR is nil or not-specified, the first character of the
optionstring is used.
The options are provided as a list for the user to choose from;
user can then choose by typing CHAR.
Example:
  (mu4e-read-option \"Choose an animal: \"
              '((\"Monkey\" ?m) (\"Gnu\" ?g) (\"platipus\")))
User now will be presented with a list:
   \"Choose an animal: [m]Monkey, [g]Gnu, [p]latipus\"
Function returns the CHAR typed."
  (let* ((optionkars)
	  (optionsstr
	    (mapconcat
	    (lambda (option)
	      (let* ((descr (car option)) (kar (and (cdr option) (cadr option))))
		;; handle the empty kar case
		(unless kar
		  (setq ;; eat first kar from descr; use it as kar
		    kar   (string-to-char descr)
		    descr (substring descr 1)))
		(add-to-list 'optionkars kar)
		(concat
		  "[" (propertize (make-string 1 kar)
			'face 'mu4e-view-link-face) "]"
		  descr))) options ", "))
	  (inhibit-quit nil) ;; allow C-g from read-char, not sure why this is needed
	  (okchar)
	  (response))
    (while (not okchar)
      (message nil) ;; we need to clear the echo area first... why?!
      (setq response
	  (read-char-exclusive
	    (concat prompt optionsstr
	      " [" (propertize "C-g" 'face 'highlight) " to quit]")))
      (setq okchar (member response optionkars)))
    response))


(defun mu4e-get-maildirs (parentdir)
  "List the maildirs under PARENTDIR." ;; TODO: recursive?
  (let* ((files (directory-files parentdir))
	  (maildirs ;;
	    (remove-if
	      (lambda (file)
		(let ((path (concat parentdir "/" file)))
		  (cond
		    ((string-match "^\\.\\{1,2\\}$" file)  t) ;; remove '..' and '.'
		    ((not (file-directory-p path)) t)   ;; remove non-dirs
		    ((not ;; remove non-maildirs
		       (and (file-directory-p (concat path "/cur"))
			 (file-directory-p (concat path "/new"))
			 (file-directory-p (concat path "/tmp")))) t)
		    (t nil) ;; otherwise, it's probably maildir
		    )))
	      files)))
    (map 'list (lambda(dir) (concat "/" dir)) maildirs)))

(defun mu4e-ask-maildir (prompt)
  "Ask the user for a shortcut (using PROMPT) as defined in
`mu4e-maildir-shortcuts', then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`mu4e-maildir-shortcuts is not defined, let user choose from all
maildirs under `mu4e-maildir."
  (unless mu4e-maildir (error "`mu4e-maildir' is not defined"))
  (if (not mu4e-maildir-shortcuts)
    (ido-completing-read prompt (mu4e-get-maildirs mu4e-maildir))
    (let* ((mlist (append mu4e-maildir-shortcuts '(("ther" . ?o))))
	    (fnames
	      (mapconcat
		(lambda (item)
		  (concat
		    "["
		    (propertize (make-string 1 (cdr item))
		      'face 'mu4e-view-link-face)
		    "]"
		    (car item)))
		mlist ", "))
	    (kar (read-char (concat prompt fnames))))
      (if (= kar ?o) ;; user chose 'other'?
	(ido-completing-read prompt (mu4e-get-maildirs mu4e-maildir))
	(or
	  (car-safe (find-if
		      (lambda (item)
			(= kar (cdr item)))
		      mu4e-maildir-shortcuts))
	  (error "Invalid shortcut '%c'" kar))))))


(defun mu4e-ask-bookmark (prompt &optional kar)
  "Ask the user for a bookmark (using PROMPT) as defined in
`mu4e-bookmarks', then return the corresponding query."
  (unless mu4e-bookmarks (error "`mu4e-bookmarks' is not defined"))
  (let* ((bmarks
	   (mapconcat
	     (lambda (bm)
	       (let ((query (nth 0 bm)) (title (nth 1 bm)) (key (nth 2 bm)))
		 (concat
		   "[" (propertize (make-string 1 key)
			 'face 'mu4e-view-link-face)
		   "]"
		   title))) mu4e-bookmarks ", "))
	  (kar (read-char (concat prompt bmarks))))
    (mu4e-get-bookmark-query kar)))

(defun mu4e-get-bookmark-query (kar)
  "Get the corresponding bookmarked query for shortcut character
KAR, or raise an error if none is found."
 (let ((chosen-bm
	 (find-if
	   (lambda (bm)
	     (= kar (nth 2 bm)))
	   mu4e-bookmarks)))
   (if chosen-bm
     (nth 0 chosen-bm)
     (error "Invalid shortcut '%c'" kar))))

(defun mu4e-new-buffer (bufname)
  "Return a new buffer BUFNAME; if such already exists, kill the
old one first."
  (when (get-buffer bufname)
    (progn
      (message (format "Killing %s" bufname))
      (kill-buffer bufname)))
  (get-buffer-create bufname))



;;; converting flags->string and vice-versa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-flags-to-string (flags)
  "Remove duplicates and sort the output of `mu4e-flags-to-string-raw'."
  (concat
    (sort (remove-duplicates
	    (append (mu4e-flags-to-string-raw flags) nil)) '>)))

(defun mu4e-flags-to-string-raw (flags)
  "Convert a list of flags into a string as seen in Maildir
message files; flags are symbols draft, flagged, new, passed,
replied, seen, trashed and the string is the concatenation of the
uppercased first letters of these flags, as per [1]. Other flags
than the ones listed here are ignored.
Also see `mu4e-flags-to-string'.
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
		 ('attach    ?a)
		 ('encrypted ?x)
		 ('signed    ?s)
		 ('unread    ?u))))
      (concat (and kar (string kar))
	(mu4e-flags-to-string-raw (cdr flags))))))


(defun mu4e-string-to-flags (str)
  "Remove duplicates from the output of `mu4e-string-to-flags-1'"
  (remove-duplicates (mu4e-string-to-flags-1 str)))

(defun mu4e-string-to-flags-1 (str)
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
	(mu4e-string-to-flags-1 (substring str 1))))))


(defun mu4e-display-size (size)
  "Get a string representation of SIZE (in bytes)."
  (cond
    ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
    ((and (>= size 1000) (< size 1000000))
      (format "%2.1fK" (/ size 1000.0)))
    ((< size 1000) (format "%d" size))
    (t (propertize "?" 'face 'mu4e-system-face))))


(defun mu4e-body-text (msg)
  "Get the body in text form for this message, which is either :body-txt,
or if not available, :body-html converted to text. By default, it
uses the emacs built-in `html2text'. Alternatively, if
`mu4e-html2text-command' is non-nil, it will use that. Normally,
function prefers the text part, but this can be changed by setting
`mu4e-view-prefer-html'."
  (let* ((txt (plist-get msg :body-txt))
	 (html (plist-get msg :body-html))
	  (body))
    ;; is there an appropriate text body?
    (when (and txt
	    (not (and mu4e-view-prefer-html html))
	    (> (* 10 (length txt))
	      (if html (length html) 0))) ;; real text part?
      (setq body txt))
    ;; no body yet? try html
    (unless body
      (when html
	(setq body
	  (with-temp-buffer
	    (insert html)
	    ;; if defined, use the external tool
	    (if mu4e-html2text-command
	      (shell-command-on-region (point-min) (point-max)
		mu4e-html2text-command nil t)
	      ;; otherwise...
	      (html2text))
	    (buffer-string)))))
    ;; still no body?
    (unless body
      (setq body ""))
    ;; and finally, remove some crap from the remaining string.
    (replace-regexp-in-string "[ ]" " " body nil nil nil)))

(defconst mu4e-update-mail-name "*mu4e-update-mail*"
  "*internal* Name of the process to update mail")

(defun mu4e-update-mail (&optional buf)
  "Update mail (retrieve using `mu4e-get-mail-command' and update
the database afterwards), with output going to BUF if not nil, or
discarded if nil. After retrieving mail, update the database. Note,
function is asynchronous, returns (almost) immediately, and all the
processing takes part in the background, unless buf is non-nil."
  (unless mu4e-get-mail-command
    (error "`mu4e-get-mail-command' is not defined"))
  (let* ((process-connection-type t)
	  (proc (start-process-shell-command
		 mu4e-update-mail-name buf mu4e-get-mail-command)))
    (message "Retrieving mail...")
    (set-process-sentinel proc
      (lambda (proc msg)
	(message nil)
	(mu4e-proc-index mu4e-maildir)
	(let ((buf (process-buffer proc)))
	  (when (buffer-live-p buf)
	    (kill-buffer buf)))))
    (set-process-query-on-exit-flag proc t)))


(defun mu4e-display-manual ()
  "Display the mu4e manual page for the current mode, or go to the
top level if there is none."
  (interactive)
  (info (case major-mode
	  ('mu4e-main-mode "(mu4e)Main view")
	  ('mu4e-hdrs-mode "(mu4e)Headers view")
	  ('mu4e-view-mode "(mu4e)Message view")
	  (t               "mu4e"))))

(defun mu4e-user-agent ()
  "Return the User-Agent string for mu4e. This is either the value
of `mu4e-user-agent', or, if not set, a string based on the versions
of mu4e and emacs."
  (or mu4e-user-agent
    (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version)))


(defun mu4e-message-at-point ()
  "Get the message s-expression for the message at point in either
the headers buffer or the view buffer."
  (let ((msg
	 (cond
	   ((eq major-mode 'mu4e-hdrs-mode)
	     (get-text-property (point) 'msg))
	   ((eq major-mode 'mu4e-view-mode)
	     mu4e-current-msg))))
    (unless msg (error "No message at point"))
    msg))

(defun mu4e-field-at-point (field)
  "Get FIELD (a symbol, see `mu4e-header-names') for the message at
point in eiter the headers buffer or the view buffer."
  (plist-get (mu4e-message-at-point) field))

(defun mu4e-capture-message ()
  "Capture the path of the message at point."
  (interactive)
  (setq mu4e-captured-message (mu4e-message-at-point))
  (message "Message has been captured"))

(defun mu4e-kill-buffer-and-window (buf)
  "Kill buffer BUF and any of its windows. Like
`kill-buffer-and-window', but can be called from any buffer, and
simply does not attempt to delete the window if there is none,
instead of erroring out."
  (when (buffer-live-p buf)
    (bury-buffer)
    (delete-windows-on buf) ;; destroy all windows for this buffer
    (kill-buffer buf)))

(defun mu4e-select-other-view ()
  "When the headers view is selected, select the message view (if
that has a live window), and vice versa."
  (interactive)
  (let* ((other-buf
	   (cond
	     ((eq major-mode 'mu4e-hdrs-mode)
	       mu4e-view-buffer)
	     ((eq major-mode 'mu4e-view-mode)
	       mu4e-hdrs-buffer)))
	  (other-win (and other-buf (get-buffer-window other-buf))))
    (if (window-live-p other-win)
      (select-window other-win)
      (message "No window to switch to"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some handler functions for server messages
;;
(defun mu4e-info-handler (info)
  "Handler function for (:info ...) sexps received from the server
process."
  (let ((type (plist-get info :info)))
    (cond
      ((eq type 'add)
	;; update our path=>docid map; we use this when composing messages to
	;; add draft messages to the db, so when we're sending them, we can move
	;; to the sent folder using the `mu4e-proc-move'.
	(puthash (plist-get info :path) (plist-get info :docid) mu4e-path-docid-map))
      ((eq type 'index)
	(if (eq (plist-get info :status) 'running)
	  (message (format "Indexing... processed %d, updated %d"
		     (plist-get info :processed) (plist-get info :updated)))
	  (message
	    (format "Indexing completed; processed %d, updated %d, cleaned-up %d"
	      (plist-get info :processed) (plist-get info :updated)
	      (plist-get info :cleaned-up)))))
      ((plist-get info :message) (message "%s" (plist-get info :message))))))


(defun mu4e-error-handler (errcode errmsg)
  "Handler function for showing an error."
  (case errcode
      (4 (message "No matches for this search query."))
      (t (message (format "Error %d: %s" errcode errmsg)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-update-timer nil
  "*internal* The mu4e update timer.")

(defun mu4e ()
  "Start mu4e. We do this by sending a 'ping' to the mu server
process, and start the main view if the 'pong' we receive from the
server has the expected values."
  (interactive)
  (if (buffer-live-p (get-buffer mu4e-main-buffer-name))
    (switch-to-buffer mu4e-main-buffer-name)
    (mu4e-check-requirements)
    ;; explicit version checks are a bit questionable,
    ;; better to check for specific features
    (if (< emacs-major-version 23)
	(error "Emacs >= 23.x is required for mu4e")
	(progn
	  (setq mu4e-pong-func
	    (lambda (version doccount)
	      (unless (string= version mu4e-mu-version)
		(error "mu server has version %s, but we need %s"
		  version mu4e-mu-version))
	      (mu4e-main-view)
	      (when (and mu4e-update-interval (null mu4e-update-timer))
		(setq mu4e-update-timer
		  (run-at-time
		    0 mu4e-update-interval
		    'mu4e-update-mail)))
	      (message "Started mu4e with %d message%s in store"
		doccount (if (= doccount 1) "" "s"))))
	  (mu4e-proc-ping)))))

(defun mu4e-quit()
  "Quit the mu4e session."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit? ")
    (message nil)
    (when mu4e-update-timer
      (cancel-timer mu4e-update-timer)
      (setq mu4e-update-timer nil))
    (mu4e-kill-proc)
    (kill-buffer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging / debugging
(defun mu4e-log (type frm &rest args)
  "Write a message of TYPE with format-string FRM and ARGS in
*mu4e-log* buffer, if the variable mu4e-debug is non-nil. Type is
either 'to-server, 'from-server or 'misc. This function is meant for debugging."
  (when mu4e-debug
    (with-current-buffer (get-buffer-create mu4e-log-buffer-name)
      (view-mode)
      (setq buffer-undo-list t)
      (let* ((inhibit-read-only t)
	      (tstamp (propertize (format-time-string "%Y-%m-%d %T" (current-time))
			'face 'font-lock-string-face))
	      (msg-face
		(case type
		  (from-server 'font-lock-type-face)
		  (to-server   'font-lock-function-name-face)
		  (misc        'font-lock-variable-name-face)
		  (otherwise   (error "Unsupported log type"))))
	      (msg (propertize (apply 'format frm args) 'face msg-face)))
	(goto-char (point-max))
	(insert tstamp 
	  (case type
	    (from-server " <- ")
	    (to-server   " -> " )
	    (otherwise   " "))
	  msg "\n")
	
	;; if `mu4e-log-max-lines is specified and exceeded, clearest the oldest
	;; lines
	(when (numberp mu4e-log-max-lines)
	  (let ((lines (count-lines (point-min) (point-max))))
	    (when (> lines mu4e-log-max-lines)
	      (goto-char (point-max))
	      (forward-line (- mu4e-log-max-lines lines))
	      (beginning-of-line)
	      (delete-region (point-min) (point)))))))))
       
(defun mu4e-toggle-logging ()
  "Toggle between enabling/disabling debug-mode (in debug-mode,
mu4e logs some of its internal workings to a log-buffer. See
`mu4e-visit-log'."
  (interactive)
  (setq mu4e-debug (not mu4e-debug))
  (message "mu4e debug logging has been %s"
    (if mu4e-debug "enabled" "disabled")))

(defun mu4e-show-log ()
  "Visit the mu4e debug log."
  (interactive)
  (let ((buf (get-buffer mu4e-log-buffer-name)))
    (unless (buffer-live-p buf)
      (error "No debug log available"))
    (switch-to-buffer buf)))


(provide 'mu4e-utils)
;;; End of mu4e-utils.el
