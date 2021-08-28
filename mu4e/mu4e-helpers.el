;;; mu4e-helpers.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dirk-Jan C. Binnema

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
(require 'cl-lib)


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

(defun mu4e--read-char-choice (prompt choices)
  "Read and return one of CHOICES, prompting for PROMPT.
Any input that is not one of CHOICES is ignored. This mu4e's
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
non-nil. Type is either 'to-server, 'from-server or 'misc. This
function is meant for debugging."
  (when mu4e-debug
    (with-current-buffer (mu4e--get-log-buffer)
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


;;; Misc

(defun mu4e-quote-for-modeline (str)
  "Quote STR to be used literally in the modeline.
The string will be shortened to fit if its length exceeds
`mu4e-modeline-max-width'."
  (replace-regexp-in-string
   "%" "%%"
   (truncate-string-to-width str mu4e-modeline-max-width 0 nil t)))

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

(provide 'mu4e-helpers)
;;; mu4e-helpers.el ends here

