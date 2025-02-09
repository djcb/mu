;;; mu4e-helpers.el --- Helper functions -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Dirk-Jan C. Binnema

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
(require 'message)

(require 'mu4e-window)
(require 'mu4e-config)

;;; Customization

(defcustom mu4e-debug nil
  "When set to non-nil, log debug information to the mu4e log buffer."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-completing-read-function #'ido-completing-read
  "Function to be used to receive user-input during completion.

Suggested possible values are:
 * `completing-read':      Emacs' built-in completion method
 * `ido-completing-read':  dynamic completion within the minibuffer.

The function is used in two contexts -
1) directly - for instance in when listing _other_ maildirs
   in `mu4e-ask-maildir'
2) if `mu4e-read-option-use-builtin' is nil, it is used
   as part of `mu4e-read-option' in many places.

Set it to `completing-read' when you want to use completion
frameworks such as Helm, Ivy or Vertico. In that case, you
might want to add something like the following in your configuration.

   (setq mu4e-read-option-use-builtin nil
         mu4e-completing-read-function \\='completing-read)
."
  :type 'function
  :options '(completing-read ido-completing-read)
  :group 'mu4e)

(defcustom mu4e-read-option-use-builtin t
  "Whether to use mu4e's traditional completion for `mu4e-read-option'.

If nil, use the value of `mu4e-completing-read-function', integrated
into mu4e.

Many of the third-party completion frameworks - such as Helm, Ivy
and Vertico - influence `completion-read', so to have mu4e follow
your overall settings, try the equivalent of

   (setq mu4e-read-option-use-builtin nil
         mu4e-completing-read-function \\='completing-read)

Tastes differ, but without any such frameworks, the unaugmented
Emacs `completing-read' is rather Spartan."
  :type 'boolean
  :group 'mu4e)

(defcustom mu4e-use-fancy-chars nil
  "When set, allow fancy (Unicode) characters for marks/threads.
You can customize the exact fancy characters used with
`mu4e-marks' and various `mu4e-headers-..-mark' and
`mu4e-headers..-prefix' variables."
  :type 'boolean
  :group 'mu4e)

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

(defun mu4e-select-other-view ()
  "Switch between headers view and message view."
  (interactive)
  (let* ((other-buf
          (cond
           ((mu4e-current-buffer-type-p 'view)
            (mu4e-get-headers-buffer))
           ((mu4e-current-buffer-type-p 'headers)
            (mu4e-get-view-buffer))
           (t (mu4e-error
               "This window is neither the headers nor the view window"))))
         (other-win (and other-buf (get-buffer-window other-buf))))
    (if (window-live-p other-win)
        (select-window other-win)
      (mu4e-message "No window to switch to"))))

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
  (error "%s" (apply 'mu4e-format frm args)))

(defun mu4e-warn (frm &rest args)
  "Create [mu4e]-prefixed warning based on format FRM and ARGS.
Does a local-exit and does not return."
  (mu4e-log 'error (apply 'mu4e-format frm args))
  (user-error "%s" (apply 'mu4e-format frm args)))

;;; Reading user input

(defun mu4e--plist-get (lst prop)
  "Get PROP from plist LST and raise an error if not present."
  (or (plist-get lst prop)
      (if (plist-member lst prop)
          nil
        (mu4e-error "Missing property %s in %s" prop lst))))

(defun mu4e--matching-choice (choices kar)
  "Does KAR match any of the  CHOICES?

KAR is a character and CHOICES is an alist as described in
`mu4e--read-choice-builtin'.

First try an exact match, but if there isn't, try
case-insensitive.

Return the cdr (value) of the matching cell, if any."
  (let* ((match) (match-ci))
    (catch 'found
      (seq-do
       (lambda (choice)
         ;; first try an exact match
         (let ((case-fold-search nil))
           (if (char-equal kar (caadr choice))
               (progn
                 (setq match choice)
                 (throw 'found choice)) ;; found it - quit.
             ;; perhaps case-insensitive?
             (let ((case-fold-search t))
               (when (and (not match-ci) (char-equal kar (caadr choice)))
                 (setq match-ci choice))))))
       choices))
    (if match (cdadr match)
      (when match-ci (cdadr match-ci)))))

(defun mu4e--read-choice-completing-read (prompt choices)
  "Read and return one of CHOICES, prompting for PROMPT.

PROMPT describes a multiple-choice question to the user. CHOICES
is an alist of the form
  ( ( <display-string>  ( <shortcut> . <value> ))
     ... )
Any input that is not one of CHOICES is ignored. This is mu4e's
version of `read-char-choice' which becomes case-insensitive
after trying an exact match.

Return the matching choice value (cdr of the cell)."
  (let* ((metadata `(metadata
                     (display-sort-function . ,#'identity)
                     (cycle-sort-function   . ,#'identity)))
         (quick-result)
         (result
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook
                          (lambda ()
                            ;; Exit directly if a quick key is pressed
                            (let ((prefix (minibuffer-contents-no-properties)))
                              (unless (string-empty-p prefix)
                                (setq quick-result
                                      (mu4e--matching-choice
                                       choices (string-to-char prefix)))
                                (when quick-result
                                  (exit-minibuffer)))))
                          -1 'local))
            (funcall mu4e-completing-read-function
             prompt
             ;; Use function with metadata to disable sorting.
             (lambda (input predicate action)
               (if (eq action 'metadata)
                   metadata
                 (complete-with-action action choices input predicate)))
             ;; Require confirmation, if the input does not match a suggestion
             nil t nil nil nil))))
    (or quick-result
        (cdadr (assoc result choices)))))

(defun mu4e--read-choice-builtin (prompt choices)
  "Read and return one of CHOICES, prompting for PROMPT.

PROMPT describes a multiple-choice question to the user. CHOICES
is an alist of the form
  ( ( <display-string>  ( <shortcut> . <value> ))
     ... )
Any input that is not one of CHOICES is ignored. This is mu4e's
version of `read-char-choice' which becomes case-insensitive
after trying an exact match.

Return the matching choice value (cdr of the cell)."
  (let ((chosen) (inhibit-quit nil)
        (prompt (format "%s%s"
                        (mu4e-format prompt)
                        (mapconcat #'car choices ", "))))
    (while (not chosen)
      (message nil) ;; this seems needed...
      (when-let* ((kar (read-char-exclusive prompt)))
        (when (eq kar ?\e) (keyboard-quit)) ;; `read-char-exclusive' is a C
                                            ;; function and doesn't check for
                                            ;; `keyboard-quit', there we need to
                                            ;; check if ESC is pressed
        (setq chosen (mu4e--matching-choice choices kar))))
    chosen))

(defun mu4e-read-option (prompt options)
  "Ask user for an option from a list on the input area.

PROMPT describes a multiple-choice question to the user. OPTIONS
describe the options, and is a list of cells describing
particular options. Cells have the following structure:

   (OPTION . RESULT)

where OPTIONS is a non-empty string describing the option. The
first character of OPTION is used as the shortcut, and obviously
all shortcuts must be different, so you can prefix the string
with an uniquifying character.

The options are provided as a list for the user to choose from;
user can then choose by typing CHAR.  Example:
  (mu4e-read-option \"Choose an animal: \"
              \\='((\"Monkey\" . monkey) (\"Gnu\" . gnu) (\"xMoose\" . moose)))

User now will be presented with a list: \"Choose an animal:
   [M]onkey, [G]nu, [x]Moose\".

If optional character KEY is provied, use that instead of asking
the user.

Function returns the value (cdr) of the matching cell."
  (let* ((choices ;; ((<display> ( <key> . <value> ) ...)
          (seq-map
           (lambda (option)
             (list
              (concat ;; <display>
               "[" (propertize (substring (car option) 0 1)
                               'face 'mu4e-highlight-face)
               "]"
               (substring (car option) 1))
              (cons
               (string-to-char (car option)) ;; <key>
               (cdr option))))               ;; <value>
           options))
         (response (funcall
                    (if mu4e-read-option-use-builtin
                        #'mu4e--read-choice-builtin
                      #'mu4e--read-choice-completing-read)
                    prompt choices)))
    (or response
        (mu4e-warn "invalid input"))))

(defun mu4e-filter-single-key (lst)
  "Return a list consisting of LST items with a `characterp' :key prop."
  ;; This works for bookmarks and maildirs.
  (seq-filter (lambda (item)
                (characterp (plist-get item :key)))
              lst))

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
    (display-buffer buf)))

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
         (thing (or thing (get-text-property (point) 'shr-url)))
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
                      (mu4e-error
                       "Cannot bookmark message without message-id"))))
    `(,title
      ,@(bookmark-make-record-default 'no-file 'no-context)
      (message-id . ,msgid)
      (handler    . mu4e--jump-to-bookmark))))

(declare-function mu4e-view-message-with-message-id "mu4e-view")
(declare-function mu4e-message-at-point             "mu4e-message")

(defun mu4e--jump-to-bookmark (bookmark)
  "View the message referred to by BOOKMARK."
  (when-let* ((msgid (bookmark-prop-get bookmark 'message-id)))
    (mu4e-view-message-with-message-id msgid)))

(defun mu4e--popup-lisp-buffer (bufname data)
  "Show or hide an s-expression string in a popup-buffer.
BUFNAME is the name of the buffer, and DATA is lisp-data, if any."
  (if-let* ((win (get-buffer-window bufname)))
      (delete-window win)
    (when data
      (when (buffer-live-p bufname)
        (kill-buffer bufname))
      (with-current-buffer-window
          (get-buffer-create bufname) nil nil
        ;; sadly, the default indentation for plists
        ;; is not very nice, and I failed to overwrite it
        (lisp-mode)
        (insert (pp-to-string data))
        ;; add basic `quit-window' bindings
        (view-mode 1)))))

;;; Macros

(defmacro mu4e-setq-if-nil (var val)
  "Set VAR to VAL if VAR is nil."
  `(unless ,var (setq ,var ,val)))

;;; Misc
(defun mu4e-join-paths (directory &rest components)
  "Append COMPONENTS to DIRECTORY and return the resulting string.

This is mu4e's version of Emacs 28's `file-name-concat' with the
difference it also handles slashes at the beginning of
COMPONENTS."
  (replace-regexp-in-string
   "//+" "/"
   (mapconcat (lambda (part) (if (stringp part) part ""))
              (cons directory components) "/")))

(defun mu4e-string-replace (from-string to-string in-string)
  "Replace FROM-STRING with TO-STRING in IN-STRING each time it occurs.
Mu4e's version of Emacs 28's `string-replace'."
  (replace-regexp-in-string (regexp-quote from-string)
                            to-string in-string nil 'literal))

(defun mu4e-plistp (object)
  "Non-nil if and only if OBJECT is a valid plist.

This is mu4e's version of Emacs 29's `plistp'."
  (let ((len (proper-list-p object)))
    (and len (zerop (% len 2)))))

(defun mu4e--message-hide-headers ()
  "Hide headers based on the `message-hidden-headers' variable.
This is mu4e's version of the post-emacs-28 `message-hide-headers',
which we need to avoid #2661."
  (let ((regexps (if (stringp message-hidden-headers)
                     (list message-hidden-headers)
                   message-hidden-headers))
        end-of-headers)
    (when regexps
      (save-excursion
        (save-restriction
          (message-narrow-to-headers)
          (setq end-of-headers (point-min-marker))
          (goto-char (point-min))
          (while (not (eobp))
            (if (not (message-hide-header-p regexps))
                (message-next-header)
              (let ((begin (point)))
                (message-next-header)
                (let ((header (delete-and-extract-region begin (point))))
                  (save-excursion
                    (goto-char end-of-headers)
                    (insert-before-markers header))))))))
      (narrow-to-region end-of-headers (point-max)))))

(defun mu4e-key-description (cmd)
  "Get the textual form of current binding to interactive function CMD.
If it is unbound, return nil. If there are multiple bindings,
return the shortest.

Roughly does what `substitute-command-keys' does, but picks
shorter keys in some cases where there are multiple bindings."
  ;; not a perfect heuristic: e.g. '<up>' is longer that 'C-p'
  (car-safe
   (seq-sort (lambda (b1 b2)
               (< (length b1) (length b2)))
             (seq-map #'key-description
                      (where-is-internal cmd)))))

(provide 'mu4e-helpers)
;;; mu4e-helpers.el ends here
