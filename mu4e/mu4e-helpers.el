;;; mu4e-helpers.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2022 Dirk-Jan C. Binnema

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

;; Helper functions used in the mu4e. This is slowly usurp all the code from
;; mu4e-utils.el that does not depend on other parts of mu4e.

;;; Code:

(require 'seq)
(require 'ido)
(require 'cl-lib)
(require 'bookmark)

(require 'mu4e-config)

;;; Customization

(defcustom mu4e-debug nil
  "When set to non-nil, log debug information to the mu4e log  buffer."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-modeline-max-width 42
  "Determines the maximum length of the modeline string.
If the string exceeds this limit, it will be truncated to fit."
  :type 'integer
  :group 'mu4e)

(defcustom mu4e-completing-read-function 'ido-completing-read
  "Function to be used to receive user-input during completion.
Suggested possible values are:
 * `completing-read':      built-in completion method
 * `ido-completing-read':  dynamic completion within the minibuffer."
  :type 'function
  :options '(completing-read ido-completing-read)
  :group 'mu4e)

(defcustom mu4e-use-fancy-chars nil
  "When set, allow fancy (Unicode) characters for marks/threads.
You can customize the exact fancy characters used with
`mu4e-marks' and various `mu4e-headers-..-mark' and
`mu4e-headers..-prefix' variables."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-display-update-status-in-modeline nil
  "Non-nil value will display the update status in the modeline."
  :group 'mu4e
  :type 'boolean)

;; maybe move the next ones... but they're convenient
;; here because they're needed in multiple buffers.

(defcustom mu4e-view-auto-mark-as-read t
  "Automatically mark messages as read when you read them.
This is the default behavior, but can be turned off, for example
when using a read-only file-system.

This can also be set to a function; if so, receives a message
plist which should evaluate to nil if the message should *not* be
marked as read-only, or non-nil otherwise."
  :type '(choice
          boolean
          function)
  :group 'mu4e-view)


(defcustom mu4e-split-view 'horizontal
  "How to show messages / headers.
A symbol which is either:
 * `horizontal':    split horizontally (headers on top)
 * `vertical':      split vertically (headers on the left).
 * `single-window': view and headers in one window (mu4e will try not to
        touch your window layout), main view in minibuffer
 * a function:      the function is responsible to return some window for
        the view.
 * anything else:   don't split (show either headers or messages,
        not both).
Also see `mu4e-headers-visible-lines'
and `mu4e-headers-visible-columns'."
  :type '(choice (const :tag "Split horizontally" horizontal)
                 (const :tag "Split vertically" vertical)
                 (const :tag "Single window" single-window)
                 (const :tag "Don't split" nil))
  :group 'mu4e-headers)

;;; Buffers

(defconst mu4e-main-buffer-name " *mu4e-main*"
  "Name of the mu4e main buffer.
The default name starts with SPC and therefore is not visible in
buffer list.")
(defconst mu4e-headers-buffer-name "*mu4e-headers*"
  "Name of the buffer for message headers.")
(defconst mu4e-embedded-buffer-name " *mu4e-embedded*"
  "Name for the embedded message view buffer.")
(defconst mu4e-view-buffer-name "*Article*"
  "Name of the view buffer.")

(defun mu4e-get-headers-buffer ()
  "Get the buffer object from `mu4e-headers-buffer-name'."
  (get-buffer mu4e-headers-buffer-name))

(defun mu4e-get-view-buffer ()
  "Get the buffer object from `mu4e-view-buffer-name'."
  (get-buffer mu4e-view-buffer-name))

(defun mu4e-select-other-view ()
  "Switch between headers view and message view."
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


;;; Windows
(defun mu4e-hide-other-mu4e-buffers ()
  "Bury mu4e buffers.
Hide (main, headers, view) (and delete all windows displaying
it). Do _not_ bury the current buffer, though."
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

;;; Modeline

(defun mu4e-quote-for-modeline (str)
  "Quote STR to be used literally in the modeline.
The string will be shortened to fit if its length exceeds
`mu4e-modeline-max-width'."
  (replace-regexp-in-string
   "%" "%%"
   (truncate-string-to-width str mu4e-modeline-max-width 0 nil t)))



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
  "Display FRM with ARGS like `message' in mu4e style.
If we're waiting for user-input or if there's some message in the
echo area, don't show anything."
  (unless (or (active-minibuffer-window))
    (message "%s" (apply 'mu4e-format frm args))))

(declare-function mu4e~loading-close "mu4e-headers")

(defun mu4e-error (frm &rest args)
  "Display an error with FRM and ARGS like `mu4e-message'.

Create [mu4e]-prefixed error based on format FRM and ARGS. Does a
local-exit and does not return, and raises a
debuggable (backtrace) error."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  ;; opportunistically close the "loading" window.
  (mu4e~loading-close)
  (error "%s" (apply 'mu4e-format frm args)))

(defun mu4e-warn (frm &rest args)
  "Create [mu4e]-prefixed warning based on format FRM and ARGS.
Does a local-exit and does not return."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (user-error "%s" (apply 'mu4e-format frm args)))

;;; Reading user input

(defun mu4e--read-char-choice (prompt choices)
  "Read and return one of CHOICES, prompting for PROMPT.
Any input that is not one of CHOICES is ignored. This is mu4e's
version of `read-char-choice' which becomes case-insentive after
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
PROMPT describes a multiple-choice question to the user. OPTIONS
describe the options, and is a list of cells describing
particular options. Cells have the following structure:

   (OPTIONSTRING . RESULT)

where OPTIONSTRING is a non-empty string describing the
option. The first character of OPTIONSTRING is used as the
shortcut, and obviously all shortcuts must be different, so you
can prefix the string with an uniquifying character.

The options are provided as a list for the user to choose from;
user can then choose by typing CHAR.  Example:
  (mu4e-read-option \"Choose an animal: \"
              \='((\"Monkey\" . monkey) (\"Gnu\" . gnu) (\"xMoose\" . moose)))

User now will be presented with a list: \"Choose an animal:
   [M]onkey, [G]nu, [x]Moose\".

Function returns the cdr of the list element."
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
          (mu4e--read-char-choice
           (concat prompt optionsstr
                   " [" (propertize "C-g" 'face 'mu4e-highlight-face)
                   " to cancel]")
           ;; the allowable chars
           (seq-map (lambda(elm) (string-to-char (car elm))) options)))
         (chosen
          (seq-find
           (lambda (option) (eq response (string-to-char (car option))))
           options)))
    (if chosen
        (cdr chosen)
      (mu4e-warn "Unknown shortcut '%c'" response))))



;;; Server properties
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
  (plist-get mu4e--server-props :queries))

(defun mu4e-last-query-result (query)
  "Get the last result for some QUERY or nil if not found."
  (seq-find
   (lambda (elm) (string= (plist-get elm :query) query))
   (mu4e-last-query-results)))


;;; Logging / debugging

(defconst mu4e--log-max-size 1000000
  "Max number of characters to keep around in the log buffer.")
(defconst mu4e--log-buffer-name "*mu4e-log*"
  "Name of the logging buffer.")

(defun mu4e--get-log-buffer ()
  "Fetch (and maybe create) the log buffer."
  (unless (get-buffer mu4e--log-buffer-name)
    (with-current-buffer (get-buffer-create mu4e--log-buffer-name)
      (view-mode)
      (when (fboundp 'so-long-mode)
        (unless (eq major-mode 'so-long-mode)
          (eval '(so-long-mode))))
      (setq buffer-undo-list t)))
  mu4e--log-buffer-name)

(defun mu4e-log (type frm &rest args)
  "Log a message of TYPE with format-string FRM and ARGS.
Use the mu4e log buffer for this. If the variable mu4e-debug is
non-nil. Type is a symbol, either `to-server', `from-server' or
`misc'.

This function is meant for debugging."
  (when mu4e-debug
    (with-current-buffer (mu4e--get-log-buffer)
      (let* ((inhibit-read-only t)
             (tstamp (propertize (format-time-string "%Y-%m-%d %T.%3N"
                                                     (current-time))
                                 'face 'font-lock-string-face))
             (msg-face
              (pcase type
                ('from-server 'font-lock-type-face)
                ('to-server   'font-lock-function-name-face)
                ('misc        'font-lock-variable-name-face)
                ('error       'font-lock-warning-face)
                (_            (mu4e-error "Unsupported log type"))))
             (msg (propertize (apply 'format frm args) 'face msg-face)))
        (save-excursion
          (goto-char (point-max))
          (insert tstamp
                  (pcase type
                    ('from-server " <- ")
                    ('to-server   " -> ")
                    ('error       " !! ")
                    (_            " "))
                  msg "\n")
          ;; if `mu4e-log-max-lines is specified and exceeded, clearest the
          ;; oldest lines
          (when (> (buffer-size) mu4e--log-max-size)
            (goto-char (- (buffer-size) mu4e--log-max-size))
            (beginning-of-line)
            (delete-region (point-min) (point))))))))

(defun mu4e-toggle-logging ()
  "Toggle `mu4e-debug'.
In debug-mode, mu4e logs some of its internal workings to a
log-buffer. See `mu4e-show-log'."
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
  (let ((buf (get-buffer mu4e--log-buffer-name)))
    (unless (buffer-live-p buf)
      (mu4e-warn "No debug log available"))
    (switch-to-buffer buf)))



;;; Flags
;; Converting flags->string and vice-versa

(defun mu4e-flags-to-string (flags)
  "Convert a list of Maildir[1] FLAGS into a string.

See `mu4e-string-to-flags'. \[1\]:
http://cr.yp.to/proto/maildir.html."
  (seq-sort
   '<
   (seq-mapcat
    (lambda (flag)
      (pcase flag
	(`draft     "D")
	(`flagged   "F")
	(`new       "N")
	(`passed    "P")
	(`replied   "R")
	(`seen      "S")
	(`trashed   "T")
	(`attach    "a")
	(`encrypted "x")
	(`signed    "s")
	(`unread    "u")
	(_          "")))
    (seq-uniq flags) 'string)))

(defun mu4e-string-to-flags (str)
  "Convert a STR with Maildir[1] flags into a list of flags.

See `mu4e-string-to-flags'. \[1\]:
http://cr.yp.to/proto/maildir.html."
  (seq-uniq
   (seq-filter
    'identity
    (seq-mapcat
     (lambda (kar)
       (list
	(pcase kar
	  ('?D   'draft)
	  ('?F   'flagged)
	  ('?P   'passed)
	  ('?R   'replied)
	  ('?S   'seen)
	  ('?T   'trashed)
	  (_     nil))))
     str))))


;;; Misc
(defun mu4e-copy-thing-at-point ()
  "Copy e-mail address or URL at point to the kill ring.
If there is not e-mail address at point, do nothing."
  (interactive)
  (let* ((thing (and (thing-at-point 'email)
		     (string-trim (thing-at-point 'email 'no-props) "<" ">")))
	 (thing (or thing (thing-at-point 'url 'no-props))))
    (when thing
      (kill-new thing)
      (mu4e-message "Copied '%s' to kill-ring" thing))))

(defun mu4e-display-size (size)
  "Get a human-friendly string representation of SIZE (in bytes)."
  (cond
   ((>= size 1000000)
    (format "%2.1fM" (/ size 1000000.0)))
   ((and (>= size 1000) (< size 1000000))
    (format "%2.1fK" (/ size 1000.0)))
   ((< size 1000)
    (format "%d" size))
   (t "?")))


(defun mu4e-split-ranges-to-numbers (str n)
  "Convert STR containing attachment numbers into a list of numbers.

STR is a string; N is the highest possible number in the list.
This includes expanding e.g. 3-5 into 3,4,5. If the letter
\"a\" ('all')) is given, that is expanded to a list with numbers
[1..n]."
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

(defun mu4e-make-temp-file (ext)
  "Create a self-destructing temporary file with extension EXT.
The file will self-destruct in a short while, enough to open it
in an external program."
  (let ((tmpfile (make-temp-file "mu4e-" nil (concat "." ext))))
    (run-at-time "30 sec" nil
		 (lambda () (ignore-errors (delete-file tmpfile))))
    tmpfile))

(defsubst mu4e-is-mode-or-derived-p (mode)
  "Is the current mode equal to MODE or derived from it?"
  (or (eq major-mode mode) (derived-mode-p mode)))

(defun mu4e-display-manual ()
  "Display the mu4e manual page for the current mode.
Or go to the top level if there is none."
  (interactive)
  (info (pcase major-mode
          ('mu4e-main-mode    "(mu4e)Main view")
          ('mu4e-headers-mode "(mu4e)Headers view")
          ('mu4e-view-mode    "(mu4e)Message view")
          (_                  "mu4e"))))


;;; bookmarks
(defun mu4e--make-bookmark-record ()
  "Create a bookmark for the message at point."
  (let* ((msg     (mu4e-message-at-point))
	 (subject (or (plist-get msg :subject) "No subject"))
	 (date	  (plist-get msg :date))
	 (date	  (if date (format-time-string "%F: " date) ""))
	 (title	  (format "%s%s" date subject))
	 (msgid	  (or (plist-get msg :message-id)
		      (mu4e-error "Cannot bookmark message without message-id"))))
    `(,title
      ,@(bookmark-make-record-default 'no-file 'no-context)
      (message-id . ,msgid)
      (handler    . mu4e--jump-to-bookmark))))

(declare-function mu4e-view-message-with-message-id "mu4e-view")
(declare-function mu4e-message-at-point             "mu4e-message")

(defun mu4e--jump-to-bookmark (bookmark)
  "View the message referred to by BOOKMARK."
  (when-let ((msgid (bookmark-prop-get bookmark 'message-id)))
    (mu4e-view-message-with-message-id msgid)))

;;; Macros

(defmacro mu4e-setq-if-nil (var val)
  "Set VAR to VAL if VAR is nil."
  `(unless ,var (setq ,var ,val)))

(provide 'mu4e-helpers)
;;; mu4e-helpers.el ends here
