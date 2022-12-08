;;; mu4e-headers.el -- part of mu4e -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright (C) 2011-2022 Dirk-Jan C. Binnema

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

;; In this file are function related mu4e-headers-mode, to creating the list of
;; one-line descriptions of emails, aka 'headers' (not to be confused with
;; headers like 'To:' or 'Subject:')

;;; Code:

(require 'cl-lib)
(require 'fringe)
(require 'hl-line)
(require 'mailcap)
(require 'mule-util) ;; seems _some_ people need this for truncate-string-ellipsis

(require 'mu4e-update)

    ;; utility functions
(require 'mu4e-server)
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-context)
(require 'mu4e-contacts)
(require 'mu4e-search)
(require 'mu4e-compose)
(require 'mu4e-actions)
(require 'mu4e-message)
(require 'mu4e-lists)
(require 'mu4e-update)
(require 'mu4e-folders)

(declare-function mu4e-view       "mu4e-view")
(declare-function mu4e--main-view  "mu4e-main")



;;; Configuration

(defgroup mu4e-headers nil
  "Settings for the headers view."
  :group 'mu4e)

(defcustom mu4e-headers-fields
  '( (:human-date    .   12)
     (:flags         .    6)
     (:mailing-list  .   10)
     (:from          .   22)
     (:subject       .   nil))
  "A list of header fields to show in the headers buffer.
Each element has the form (HEADER . WIDTH), where HEADER is one of
the available headers (see `mu4e-header-info') and WIDTH is the
respective width in characters.

A width of nil means \"unrestricted\", and this is best reserved
for the rightmost (last) field. Note that emacs may become very
slow with excessively long lines (1000s of characters), so if you
regularly get such messages, you want to avoid fields with nil
altogether."
  :type `(repeat (cons (choice ,@(mapcar (lambda (h)
                                           (list 'const :tag
                                                 (plist-get (cdr h) :help)
                                                 (car h)))
                                         mu4e-header-info))
                       (choice (integer :tag "width")
                               (const :tag "unrestricted width" nil))))
  :group 'mu4e-headers)

(defcustom mu4e-headers-date-format "%x"
  "Date format to use in the headers view.
In the format of `format-time-string'."
  :type  'string
  :group 'mu4e-headers)

(defcustom mu4e-headers-time-format "%X"
  "Time format to use in the headers view.
In the format of `format-time-string'."
  :type  'string
  :group 'mu4e-headers)

(defcustom mu4e-headers-long-date-format "%c"
  "Date format to use in the headers view tooltip.
In the format of `format-time-string'."
  :type  'string
  :group 'mu4e-headers)

(defcustom mu4e-headers-visible-lines 10
  "Number of lines to display in the header view when using the
horizontal split-view. This includes the header-line at the top,
and the mode-line."
  :type 'integer
  :group 'mu4e-headers)

(defcustom mu4e-headers-visible-columns 30
  "Number of columns to display for the header view when using the
vertical split-view."
  :type 'integer
  :group 'mu4e-headers)

(defcustom mu4e-headers-precise-alignment nil
  "When set, use precise (but relatively slow) alignment for columns.
By default, do it in a slightly inaccurate but faster way. To get
an idea about the difference, In some tests, the rendering time
was around 5.8 ms per messages for precise alignment, versus 3.3
for non-precise aligment (for 445 messages)."
  :type 'boolean
  :group 'mu4e-headers)

(defcustom mu4e-headers-auto-update t
  "Whether to automatically update the current headers buffer if an
indexing operation showed changes."
  :type 'boolean
  :group 'mu4e-headers)

(defcustom mu4e-headers-advance-after-mark t
  "With this option set to non-nil, automatically advance to the
next mail after marking a message in header view."
  :type 'boolean
  :group 'mu4e-headers)


(defvar mu4e-headers-hide-predicate nil
  "Predicate function to hide matching heasders.
If the function evaluates to non-nil when applied a a message
plist, do not show the corresponding header. The function takes
one parameter MSG, which is the message plist for the message to
be hidden or not.

Example that hides all trashed messages:

  (setq mu4e-headers-hide-predicate
     (lambda (msg)
       (member \='trashed (mu4e-message-field msg :flags)))).")

(defcustom mu4e-headers-visible-flags
  '(draft flagged new passed replied trashed attach encrypted signed list personal)
  "An ordered list of flags to show in the headers buffer.
Each element is a symbol in the list.

By default, we leave out `unread' and `seen', since those are
mostly covered by `new', and the display gets cluttered otherwise."
  :type '(set
          (const :tag "Draft" draft)
          (const :tag "Flagged" flagged)
          (const :tag "New" new)
          (const :tag "Passed" passed)
          (const :tag "Replied" replied)
          (const :tag "Seen" seen)
          (const :tag "Trashed" trashed)
          (const :tag "Attach" attach)
          (const :tag "Encrypted" encrypted)
          (const :tag "Signed" signed)
	  (const :tag "List" list)
	  (const :tag "Personal" personal)
	  (const :tag "Calendar" calendar))
  :group 'mu4e-headers)

(defcustom mu4e-headers-found-hook nil
  "Hook run just *after* all of the headers for the last search
query have been received and are displayed."
  :type 'hook
  :group 'mu4e-headers)

;;; Public variables

(defvar mu4e-headers-sort-field :date
  "Field to sort the headers by. A symbol:
one of: `:date', `:subject', `:size', `:prio', `:from', `:to.',
`:list'.

Note that when threading is enabled (through
`mu4e-search-threads'), the headers are exclusively sorted
chronologically (`:date') by the newest message in the thread.")

(defvar mu4e-headers-sort-direction 'descending
  "Direction to sort by; a symbol either `descending' (sorting
  Z->A) or `ascending' (sorting A->Z).")

(defcustom mu4e-headers-from-or-to-prefix '("" . "To ")
  "Prefix for the :from-or-to field when it is showing,
  respectively, From: or To:.  It is a cons cell with the car
  element being the From: prefix, the cdr element the To: prefix."
  :type '(cons string string)
  :group 'mu4e-headers)

;;;; Fancy marks

;; marks for headers of the form; each is a cons-cell (basic . fancy)
;; each of which is basic ascii char and something fancy, respectively
;; by default, we some conservative marks, even when 'fancy'
;; so they're less likely to break if people don't have certain fonts.
;; However, if you want to be really 'fancy', you could use something like
;; the following; esp. with a newer Emacs with color-icon support.
;; (setq
;;  mu4e-headers-draft-mark     '("D" . "💈")
;;  mu4e-headers-flagged-mark   '("F" . "📍")
;;  mu4e-headers-new-mark       '("N" . "🔥")
;;  mu4e-headers-passed-mark    '("P" . "❯")
;;  mu4e-headers-replied-mark   '("R" . "❮")
;;  mu4e-headers-seen-mark      '("S" . "☑")
;;  mu4e-headers-trashed-mark   '("T" . "💀")
;;  mu4e-headers-attach-mark    '("a" . "📎")
;;  mu4e-headers-encrypted-mark '("x" . "🔒")
;;  mu4e-headers-signed-mark    '("s" . "🔑")
;;  mu4e-headers-unread-mark    '("u" . "⎕")
;;  mu4e-headers-list-mark      '("s" . "🔈")
;;  mu4e-headers-personal-mark  '("p" . "👨")
;;  mu4e-headers-calendar-mark  '("c" . "📅"))


(defvar mu4e-headers-draft-mark     '("D" . "⚒") "Draft.")
(defvar mu4e-headers-flagged-mark   '("F" . "✚") "Flagged.")
(defvar mu4e-headers-new-mark       '("N" . "✱") "New.")
(defvar mu4e-headers-passed-mark    '("P" . "❯") "Passed (fwd).")
(defvar mu4e-headers-replied-mark   '("R" . "❮") "Replied.")
(defvar mu4e-headers-seen-mark      '("S" . "✔") "Seen.")
(defvar mu4e-headers-trashed-mark   '("T" . "⏚") "Trashed.")
(defvar mu4e-headers-attach-mark    '("a" . "⚓") "W/ attachments.")
(defvar mu4e-headers-encrypted-mark '("x" . "⚴") "Encrypted.")
(defvar mu4e-headers-signed-mark    '("s" . "☡") "Signed.")
(defvar mu4e-headers-unread-mark    '("u" . "⎕") "Unread.")
(defvar mu4e-headers-list-mark      '("s" . "Ⓛ") "Mailing list.")
(defvar mu4e-headers-personal-mark  '("p" . "Ⓟ") "Personal.")
(defvar mu4e-headers-calendar-mark  '("c" . "Ⓒ") "Calendar invitation.")


;;;; Graph drawing

(defvar mu4e-headers-thread-mark-as-orphan 'first
  "Define which messages should be prefixed with the orphan mark.
`all' marks all the messages without a parent as orphan, `first'
only marks the first message in the thread.")

(defvar mu4e-headers-thread-root-prefix '("* " . "□ ")
  "Prefix for root messages.")
(defvar mu4e-headers-thread-child-prefix '("|>" . "│ ")
  "Prefix for messages in sub threads that do have a following sibling.")
(defvar mu4e-headers-thread-first-child-prefix '("o " . "⚬ ")
  "Prefix for the first child messages in a sub thread.")
(defvar mu4e-headers-thread-last-child-prefix '("L" . "└ ")
  "Prefix for messages in sub threads that do not have a following sibling.")
(defvar mu4e-headers-thread-connection-prefix '("|" . "│ ")
  "Prefix to connect sibling messages that do not follow each other.
Must have the same length as `mu4e-headers-thread-blank-prefix'.")
(defvar mu4e-headers-thread-blank-prefix '(" " . "  ")
  "Prefix to separate non connected messages.
Must have the same length as `mu4e-headers-thread-connection-prefix'.")
(defvar mu4e-headers-thread-orphan-prefix '("<>" . "♢ ")
  "Prefix for orphan messages with siblings.")
(defvar mu4e-headers-thread-single-orphan-prefix '("<>" . "♢ ")
  "Prefix for orphan messages with no siblings.")
(defvar mu4e-headers-thread-duplicate-prefix '("=" . "≡ ")
  "Prefix for duplicate messages.")

(defvar mu4e-headers-threaded-label   '("T" . "Ⓣ")
  "Non-fancy and fancy labels to indicate threaded search in the mode-line.")
(defvar mu4e-headers-full-label       '("F" . "Ⓕ")
  "Non-fancy and fancy labels to indicate full search in the mode-line.")
(defvar mu4e-headers-related-label    '("R" . "Ⓡ")
  "Non-fancy and fancy labels to indicate related search in the mode-line.")
(defvar mu4e-headers-skip-duplicates-label '("U" . "Ⓤ") ;; 'U' for 'unique'
  "Non-fancy and fancy labels for include-related search in the mode-line.")

;;;; Various

(defvar mu4e-headers-actions
  '( ("capture message"  . mu4e-action-capture-message)
     ("show this thread" . mu4e-action-show-thread))
  "List of actions to perform on messages in the headers list.
The actions are cons-cells of the form (NAME . FUNC) where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first character of NAME is used as the shortcut.")

(defvar mu4e-headers-custom-markers
  '(("Older than"
     (lambda (msg date) (time-less-p (mu4e-msg-field msg :date) date))
     (lambda () (mu4e-get-time-date "Match messages before: ")))
    ("Newer than"
     (lambda (msg date) (time-less-p date (mu4e-msg-field msg :date)))
     (lambda () (mu4e-get-time-date "Match messages after: ")))
    ("Bigger than"
     (lambda (msg bytes) (> (mu4e-msg-field msg :size) (* 1024 bytes)))
     (lambda () (read-number "Match messages bigger than (Kbytes): "))))
  "List of custom markers -- functions to mark message that match
some custom function. Each of the list members has the following format:
  (NAME PREDICATE-FUNC PARAM-FUNC)
* NAME is the name of the predicate function, and the first character
is the shortcut (so keep those unique).
* PREDICATE-FUNC is a function that takes two parameters, MSG
and (optionally) PARAM, and should return non-nil when there's a
match.
* PARAM-FUNC is function that is evaluated once, and its value is then passed to
PREDICATE-FUNC as PARAM. This is useful for getting user-input.")
;;; Internal variables/constants

;; docid cookies
(defconst mu4e~headers-docid-pre "\376"
  "Each header starts (invisibly) with the `mu4e~headers-docid-pre',
followed by the docid, followed by `mu4e~headers-docid-post'.")
(defconst mu4e~headers-docid-post "\377"
  "Each header starts (invisibly) with the `mu4e~headers-docid-pre',
followed by the docid, followed by `mu4e~headers-docid-post'.")

(defvar mu4e~headers-sort-field-choices
  '( ("date"    . :date)
     ("from"    . :from)
     ("list"    . :list)
     ("maildir" . :maildir)
     ("prio"    . :prio)
     ("zsize"   . :size)
     ("subject" . :subject)
     ("to"      . :to))
  "List of cells describing the various sort-options.
In the format needed for `mu4e-read-option'.")


(defvar mu4e~headers-search-start nil)
(defvar mu4e~headers-render-start nil)
(defvar mu4e~headers-render-time  nil)

(defvar mu4e-headers-report-render-time nil
  "If non-nil, report on the time it took to render the messages.
This is mostly useful for profiling.")



;;; Clear

(defun mu4e~headers-clear (&optional text)
  "Clear the headers buffer and related data structures.
Optionally, show TEXT."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (setq mu4e~headers-render-start (float-time))
    (let ((inhibit-read-only t))
      (with-current-buffer (mu4e-get-headers-buffer)
        (mu4e--mark-clear)
        (erase-buffer)
	(when text
          (goto-char (point-min))
          (insert (propertize text 'face 'mu4e-system-face 'intangible t)))))))


;;; Misc

(defun mu4e~headers-contact-str (contacts)
  "Turn the list of contacts CONTACTS (with elements (NAME . EMAIL)
into a string."
  (mapconcat
   (lambda (contact)
     (let ((name (mu4e-contact-name contact))
	   (email (mu4e-contact-email contact)))
       (or name email "?"))) contacts ", "))

(defun mu4e~headers-thread-prefix-map (type)
  "Return the thread prefix based on the symbol TYPE."
  (let ((get-prefix
         (lambda (cell)
           (if mu4e-use-fancy-chars (cdr cell) (car cell)))))
    (cl-case type
      (child         (funcall get-prefix mu4e-headers-thread-child-prefix))
      (first-child   (funcall get-prefix mu4e-headers-thread-first-child-prefix))
      (last-child    (funcall get-prefix mu4e-headers-thread-last-child-prefix))
      (connection    (funcall get-prefix mu4e-headers-thread-connection-prefix))
      (blank         (funcall get-prefix mu4e-headers-thread-blank-prefix))
      (orphan        (funcall get-prefix mu4e-headers-thread-orphan-prefix))
      (single-orphan (funcall get-prefix mu4e-headers-thread-single-orphan-prefix))
      (duplicate     (funcall get-prefix mu4e-headers-thread-duplicate-prefix))
      (t              "?"))))


;; headers in the buffer are prefixed by an invisible string with the docid
;; followed by an EOT ('end-of-transmission', \004, ^D) non-printable ascii
;; character. this string also has a text-property with the docid. the former
;; is used for quickly finding a certain header, the latter for retrieving the
;; docid at point without string matching etc.

(defun mu4e~headers-docid-pos (docid)
  "Return the pos of the beginning of the line with the header with
docid DOCID, or nil if it cannot be found."
  (let ((pos))
    (save-excursion
      (setq pos (mu4e~headers-goto-docid docid)))
    pos))

(defun mu4e~headers-docid-cookie (docid)
  "Create an invisible string containing DOCID; this is to be used
at the beginning of lines to identify headers."
  (propertize (format "%s%d%s"
                      mu4e~headers-docid-pre docid mu4e~headers-docid-post)
              'docid docid 'invisible t));;

(defun mu4e~headers-docid-at-point (&optional point)
  "Get the docid for the header at POINT, or at current (point) if
nil. Returns the docid, or nil if there is none."
  (save-excursion
    (when point
      (goto-char point))
    (get-text-property (line-beginning-position) 'docid)))



(defun mu4e~headers-goto-docid (docid &optional to-mark)
  "Go to the beginning of the line with the header with docid
DOCID, or nil if it cannot be found. If the optional TO-MARK is
non-nil, go to the point directly *after* the docid-cookie instead
of the beginning of the line."
  (let ((oldpoint (point)) (newpoint))
    (goto-char (point-min))
    (setq newpoint
          (search-forward (mu4e~headers-docid-cookie docid) nil t))
    (unless to-mark
      (if (null newpoint)
          (goto-char oldpoint) ;; not found; restore old pos
        (progn
          (beginning-of-line) ;; found, move to beginning of line
          (setq newpoint (point)))))
    newpoint)) ;; return the point, or nil if not found

(defun mu4e~headers-field-for-docid (docid field)
  "Get FIELD (a symbol, see `mu4e-headers-names') for the message
with DOCID which must be present in the headers buffer."
  (save-excursion
    (when (mu4e~headers-goto-docid docid)
      (mu4e-message-field (mu4e-message-at-point) field))))


;; In order to print a thread tree with all the message connections,
;; it's necessary to keep track of all sub levels that still have
;; following messages. For each level, mu4e~headers-thread-state keeps
;; the value t for a connection or nil otherwise.
(defvar-local mu4e~headers-thread-state '())

(defun mu4e~headers-thread-prefix (thread)
  "Calculate the thread prefix based on thread info THREAD."
  (when thread
    (let* ((prefix       "")
          (level        (plist-get thread :level))
          (has-child    (plist-get thread :has-child))
          (first-child  (plist-get thread :first-child))
          (last-child   (plist-get thread :last-child))
          (orphan       (plist-get thread :orphan))
          (single-orphan(and orphan first-child last-child))
          (duplicate    (plist-get thread :duplicate)))
      ;; Do not prefix root messages.
      (if (= level 0)
          (setq mu4e~headers-thread-state '()))
      (if (> level 0)
          (let* ((length (length mu4e~headers-thread-state))
                 (padding (make-list (max 0 (- level length)) nil)))
            ;; Trim and pad the state to ensure a message will
            ;; always be shown with the correct indentation, even if
            ;; a broken thread is returned. It's trimmed to level-1
            ;; because the current level has always an connection
            ;; and it used a special formatting.
            (setq mu4e~headers-thread-state
                  (cl-subseq (append mu4e~headers-thread-state padding)
                             0 (- level 1)))
            ;; Prepare the thread prefix.
            (setq prefix
                  (concat
                   ;; Current mu4e~headers-thread-state, composed by
                   ;; connections or blanks.
                   (mapconcat
                    (lambda (s)
                      (mu4e~headers-thread-prefix-map
                       (if s 'connection 'blank)))
                    mu4e~headers-thread-state "")
                   ;; Current entry.
                   (mu4e~headers-thread-prefix-map
                    (if single-orphan 'single-orphan
                      (if (and orphan
                               (or first-child
                                   (not (eq mu4e-headers-thread-mark-as-orphan 'first))))
                               'orphan
                        (if last-child 'last-child
                          (if first-child 'first-child
                            'child)))))))))
      ;; If a new sub-thread will follow (has-child) and the current
      ;; one is still not done (not last-child), then a new
      ;; connection needs to be added to the tree-state.  It's not
      ;; necessary to a blank (nil), because padding will handle
      ;; that.
      (if (and has-child (not last-child))
          (setq mu4e~headers-thread-state
                (append mu4e~headers-thread-state '(t))))
      ;; Return the thread prefix.
      (format "%s%s"
              prefix
              (if duplicate
                  (mu4e~headers-thread-prefix-map 'duplicate) "")))))

(defun mu4e~headers-flags-str (flags)
  "Get a display string for FLAGS.
Note that `mu4e-flags-to-string' is for internal use only; this
function is for display. (This difference is significant, since
internally, the Maildir spec determines what the flags look like,
while our display may be different)."
  (or  (mapconcat
	(lambda (flag)
	  (when (member flag mu4e-headers-visible-flags)
	    (if-let* ((mark (intern-soft
			     (format "mu4e-headers-%s-mark" (symbol-name flag))))
		      (cell (symbol-value mark)))
		(if mu4e-use-fancy-chars (cdr cell) (car cell))
	      "")))
	flags "")
       ""))

;;; Special headers

(defun mu4e~headers-from-or-to (msg)
  "Get the From: address from MSG if not one of user's; otherwise get To:.
When the from address for message MSG is one of the the user's
addresses, (as per `mu4e-personal-address-p'), show the To
address. Otherwise, show the From address, prefixed with the
appropriate `mu4e-headers-from-or-to-prefix'."
  (let* ((from1 (car-safe (mu4e-message-field msg :from)))
	 (from1-addr (and from1 (mu4e-contact-email from1)))
	 (is-user (and from1-addr (mu4e-personal-address-p from1-addr))))
    (if is-user
        (concat (cdr mu4e-headers-from-or-to-prefix)
                (mu4e~headers-contact-str (mu4e-message-field msg :to)))
      (concat (car mu4e-headers-from-or-to-prefix)
              (mu4e~headers-contact-str (mu4e-message-field msg :from))))))

(defun mu4e~headers-human-date (msg)
  "Show a \"human\" date for MSG.
If the date is today, show the time, otherwise, show the date.
The formats used for date and time are `mu4e-headers-date-format'
and `mu4e-headers-time-format'."
  (let ((date (mu4e-msg-field msg :date)))
    (if (equal date '(0 0 0))
        "None"
      (let ((day1 (decode-time date))
            (day2 (decode-time (current-time))))
        (if (and
             (eq (nth 3 day1) (nth 3 day2))     ;; day
             (eq (nth 4 day1) (nth 4 day2))     ;; month
             (eq (nth 5 day1) (nth 5 day2)))    ;; year
            (format-time-string mu4e-headers-time-format date)
          (format-time-string mu4e-headers-date-format date))))))

(defun mu4e~headers-thread-subject (msg)
  "Get the subject for MSG if it is the first one in a thread.
Otherwise, return the thread-prefix without the subject-text. In
other words, show the subject of a thread only once, similar to
e.g. \"mutt\"."
  (let* ((tinfo (mu4e-message-field msg :meta))
         (subj (mu4e-msg-field msg :subject)))
    (concat ;; prefix subject with a thread indicator
     (mu4e~headers-thread-prefix tinfo)
     (if (plist-get tinfo :thread-subject)
         (truncate-string-to-width subj 600) ""))))

(defun mu4e~headers-mailing-list (list)
  "Get some identifier for the mailing list."
  (if list
      (propertize (mu4e-get-mailing-list-shortname list) 'help-echo list)
    ""))

(defsubst mu4e~headers-custom-field-value (msg field)
  "Show some custom header field, or raise an error if it is not
found."
  (let* ((item (or (assoc field mu4e-header-info-custom)
                   (mu4e-error "field %S not found" field)))
         (func (or (plist-get (cdr-safe item) :function)
                   (mu4e-error "no :function defined for field %S %S"
                               field (cdr item)))))
    (funcall func msg)))

(defun mu4e~headers-field-value (msg field)
  (let ((val (mu4e-message-field msg field)))
    (cl-case field
      (:subject
       (concat ;; prefix subject with a thread indicator
        (mu4e~headers-thread-prefix (mu4e-message-field msg :meta))
        ;;  "["(plist-get (mu4e-message-field msg :meta) :path) "] "
        ;; work-around: emacs' display gets really slow when lines are too long;
        ;; so limit subject length to 600
        (truncate-string-to-width val 600)))
      (:thread-subject ;; if not searching threads, fall back to :subject
       (if mu4e-search-threads
	   (mu4e~headers-thread-subject msg)
	 (mu4e~headers-field-value msg :subject)))
      ((:maildir :path :message-id) val)
      ((:to :from :cc :bcc) (mu4e~headers-contact-str val))
      ;; if we (ie. `user-mail-address' is the 'From', show
      ;; 'To', otherwise show From
      (:from-or-to (mu4e~headers-from-or-to msg))
      (:date (format-time-string mu4e-headers-date-format val))
      (:list (or val ""))
      (:mailing-list (mu4e~headers-mailing-list (mu4e-msg-field msg :list)))
      (:human-date (propertize (mu4e~headers-human-date msg)
                               'help-echo (format-time-string
                                           mu4e-headers-long-date-format
                                           (mu4e-msg-field msg :date))))
      (:flags (propertize (mu4e~headers-flags-str val)
                          'help-echo (format "%S" val)))
      (:tags (propertize (mapconcat 'identity val ", ")))
      (:size (mu4e-display-size val))
      (t (mu4e~headers-custom-field-value msg field)))))

(defsubst mu4e~headers-truncate-field-fast (val width)
  "Truncate VAL to WIDTH. Fast and somewhat inaccurate."
  (if width
      (truncate-string-to-width val width 0 ?\s truncate-string-ellipsis)
    val))

(defun mu4e~headers-truncate-field-precise (field val width)
  "Return VAL truncated to one less than WIDTH, with a trailing
space propertized with a `display' text property which expands to
 the correct column for display."
  (when width
    (let ((end-col (cl-loop for (f . w) in mu4e-headers-fields
                            sum w
                            until (equal f field))))
      (setq val (string-trim-right val))
      (if (> width (length val))
          (setq val (concat val " "))
        (setq val
              (concat
               (truncate-string-to-width val (1- width) 0 ?\s t)
               " ")))
      (put-text-property (1- (length val))
                         (length val)
                         'display
                         `(space . (:align-to ,end-col))
                         val)))
  val)

(defsubst mu4e~headers-truncate-field (field val width)
  "Truncate VAL to WIDTH."
  (if mu4e-headers-precise-alignment
      (mu4e~headers-truncate-field-precise field val width)
    (mu4e~headers-truncate-field-fast val width)))

(make-obsolete-variable 'mu4e-headers-field-properties-function
                        "not used" "1.6.1")

(defsubst mu4e~headers-field-handler (f-w msg)
  "Create a description of the field of MSG described by F-W."
  (let* ((field (car f-w))
         (width (cdr f-w))
         (val (mu4e~headers-field-value msg field))
         (val (and val (if width (mu4e~headers-truncate-field field val width) val))))
    val))

(defsubst mu4e~headers-apply-flags (msg fieldval)
  "Adjust FIELDVAL's face property based on flags in MSG."
  (let* ((flags (plist-get msg :flags))
	 (meta (plist-get msg :meta))
	 (face (cond
		((memq 'trashed flags) 'mu4e-trashed-face)
                ((memq 'draft flags)   'mu4e-draft-face)
                ((or (memq 'unread flags) (memq 'new flags))
                 'mu4e-unread-face)
                ((memq 'flagged flags) 'mu4e-flagged-face)
		((plist-get meta :related) 'mu4e-related-face)
                ((memq 'replied flags) 'mu4e-replied-face)
                ((memq 'passed flags)  'mu4e-forwarded-face)
		(t                     'mu4e-header-face))))
    (add-face-text-property 0 (length fieldval) face t fieldval)
    fieldval))

(defsubst mu4e~message-header-line (msg)
  "Return a propertized description of MSG suitable for
displaying in the header view."
  (unless (and mu4e-headers-hide-predicate
               (funcall mu4e-headers-hide-predicate msg))
    (mu4e~headers-apply-flags
     msg
     (mapconcat (lambda (f-w) (mu4e~headers-field-handler f-w msg))
                mu4e-headers-fields " "))))


(defsubst mu4e~headers-insert-header (msg pos)
  "Insert a header for MSG at point POS."
  (when-let ((line (mu4e~message-header-line msg))
	     (docid (plist-get msg :docid)))
    (goto-char pos)
    (insert
     (propertize
      (concat
       (mu4e~headers-docid-cookie docid)
       mu4e--mark-fringe line "\n")
      'docid docid 'msg msg))))

(defun mu4e~headers-remove-header (docid &optional ignore-missing)
  "Remove header with DOCID at point.
When IGNORE-MISSING is non-nill, don't raise an error when the
docid is not found."
  (with-current-buffer (mu4e-get-headers-buffer)
    (if (mu4e~headers-goto-docid docid)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position) (line-beginning-position 2)))
      (unless ignore-missing
        (mu4e-error "Cannot find message with docid %S" docid)))))


;;; Handler functions

;; next are a bunch of handler functions; those will be called from mu4e~proc in
;; response to output from the server process

(defun mu4e~headers-view-handler (msg)
  "Handler function for displaying a message."
  (mu4e-view msg))

(defun mu4e~headers-view-this-message-p (docid)
  "Is DOCID currently being viewed?"
  (when (buffer-live-p (mu4e-get-view-buffer))
    (with-current-buffer (mu4e-get-view-buffer)
      (eq docid (plist-get mu4e~view-message :docid)))))

;; note: this function is very performance-sensitive
(defun mu4e~headers-append-handler (msglst)
  "Append one-line descriptions of messages in MSGLIST.
Do this at the end of the headers-buffer."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (save-excursion
	(let ((inhibit-read-only t))
	  (seq-do
	   (lambda (msg)
	     (mu4e~headers-insert-header msg (point-max)))
	   msglst))))))


(defun mu4e~headers-update-handler (msg is-move maybe-view)
  "Update handler, will be called when a message has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (let* ((docid (mu4e-message-field msg :docid))
             (initial-message-at-point (mu4e~headers-docid-at-point))
             (initial-column (current-column))
	     (inhibit-read-only t)
             (point (mu4e~headers-docid-pos docid))
             (markinfo (gethash docid mu4e--mark-map)))
        (when point ;; is the message present in this list?

          ;; if it's marked, unmark it now
          (when (mu4e-mark-docid-marked-p docid)
            (mu4e-mark-set 'unmark))

          ;; re-use the thread info from the old one; this is needed because
          ;; *update* messages don't have thread info by themselves (unlike
          ;; search results)
          ;; since we still have the search results, re-use
          ;; those
          (plist-put msg :meta
                     (mu4e~headers-field-for-docid docid :meta))

          ;; first, remove the old one (otherwise, we'd have two headers with
          ;; the same docid...
          (mu4e~headers-remove-header docid t)

          ;; if we're actually viewing this message (in mu4e-view mode), we
          ;; update it; that way, the flags can be updated, as well as the path
          ;; (which is useful for viewing the raw message)
          (when (and maybe-view (mu4e~headers-view-this-message-p docid))
            (save-excursion (mu4e-view msg)))
          ;; now, if this update was about *moving* a message, we don't show it
          ;; anymore (of course, we cannot be sure if the message really no
          ;; longer matches the query, but this seem a good heuristic.  if it
          ;; was only a flag-change, show the message with its updated flags.
          (unless is-move
	    (save-excursion
	      (mu4e~headers-insert-header msg point)))

          ;; restore the mark, if any. See #2076.
          (when (and markinfo (mu4e~headers-goto-docid docid))
            (mu4e-mark-at-point (car markinfo) (cdr markinfo)))

          (if (and initial-message-at-point
                   (mu4e~headers-goto-docid initial-message-at-point))
              (progn
                (move-to-column initial-column)
                (mu4e~headers-highlight initial-message-at-point))
            ;; attempt to highlight the corresponding line and make it visible
            (mu4e~headers-highlight docid))
          (run-hooks 'mu4e-message-changed-hook))))))

(defun mu4e~headers-remove-handler (docid)
  "Remove handler, will be called when a message with DOCID has
been removed from the database. This function will hide the removed
message from the current list of headers. If the message is not
present, don't do anything."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (mu4e~headers-remove-header docid t))
  ;; if we were viewing this message, close it now.
  (when (and (mu4e~headers-view-this-message-p docid)
             (buffer-live-p (mu4e-get-view-buffer)))
    (unless (eq mu4e-split-view 'single-window)
      (mapc #'delete-window (get-buffer-window-list
                             (mu4e-get-view-buffer) nil t)))
    (kill-buffer (mu4e-get-view-buffer))))



;;; Performing queries (internal)
(defconst mu4e~search-message "Searching...")
(defconst mu4e~no-matches     "No matching messages found")
(defconst mu4e~end-of-results "End of search results")

(defun mu4e--search-execute (expr ignore-history)
  "Search for query EXPR.

Switch to the output buffer for the results. If IGNORE-HISTORY is
true, do *not* update the query history stack."
  (let* ((buf (get-buffer-create mu4e-headers-buffer-name))
         (inhibit-read-only t)
         (rewritten-expr (funcall mu4e-query-rewrite-function expr))
         (maxnum (unless mu4e-search-full mu4e-search-results-limit)))
    (with-current-buffer buf
      (mu4e-headers-mode)
      (unless ignore-history
        ;; save the old present query to the history list
        (when mu4e--search-last-query
          (mu4e--search-push-query mu4e--search-last-query 'past)))
      (setq mu4e--search-last-query rewritten-expr)
      (setq list-buffers-directory rewritten-expr)
      (mu4e~headers-update-mode-line))

    ;; when the buffer is already visible, select it; otherwise,
    ;; switch to it.
    (unless (get-buffer-window buf 0)
      (switch-to-buffer buf))
    (run-hook-with-args 'mu4e-search-hook expr)
    (mu4e~headers-clear mu4e~search-message)
    (setq mu4e~headers-search-start (float-time))
    (mu4e--server-find
     rewritten-expr
     mu4e-search-threads
     mu4e-headers-sort-field
     mu4e-headers-sort-direction
     maxnum
     mu4e-headers-skip-duplicates
     mu4e-headers-include-related)))

(defun mu4e~headers-benchmark-message (count)
  "Get some report message for messaging search and rendering speed."
  (if (and mu4e-headers-report-render-time
	   mu4e~headers-search-start
	   mu4e~headers-render-start
	   (> count 0))
      (let ((render-time-ms (* 1000(- (float-time) mu4e~headers-render-start)))
	    (search-time-ms (* 1000(- (float-time) mu4e~headers-search-start))))
	(format (concat
		 "; search: %0.1f ms (%0.2f ms/msg)"
		 "; render: %0.1f ms (%0.2f ms/msg)")
		search-time-ms (/ search-time-ms count)
		render-time-ms (/ render-time-ms count)))
    ""))

(defun mu4e~headers-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (str (if (zerop count) mu4e~no-matches mu4e~end-of-results))
              (msg (format "Found %d matching message%s%s"
                           count (if (= 1 count) "" "s")
			   (mu4e~headers-benchmark-message count))))

          (insert (propertize str 'face 'mu4e-system-face 'intangible t))
          (unless (zerop count)
            (mu4e-message "%s" msg))))

          ;; if we need to jump to some specific message, do so now
          (goto-char (point-min))
          (when mu4e--search-msgid-target
            (if (eq (current-buffer) (window-buffer))
                (mu4e-headers-goto-message-id mu4e--search-msgid-target)
              (let* ((pos (mu4e-headers-goto-message-id mu4e--search-msgid-target)))
                (when pos
                  (set-window-point (get-buffer-window nil t) pos)))))
          (when (and mu4e--search-view-target (mu4e-message-at-point 'noerror))
            ;; view the message at point when there is one.
            (mu4e-headers-view-message))
          (setq mu4e--search-view-target nil
                mu4e--search-msgid-target nil)
          (when (mu4e~headers-docid-at-point)
            (mu4e~headers-highlight (mu4e~headers-docid-at-point)))))
    ;; run-hooks
    (run-hooks 'mu4e-headers-found-hook))


;;; Marking

(defmacro mu4e~headers-defun-mark-for (mark)
  "Define a function mu4e~headers-mark-MARK."
  (let ((funcname (intern (format "mu4e-headers-mark-for-%s" mark)))
        (docstring (format "Mark header at point with %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e-headers-mark-and-next ',mark))
       (put ',funcname 'definition-name ',mark))))

(mu4e~headers-defun-mark-for refile)
(mu4e~headers-defun-mark-for something)
(mu4e~headers-defun-mark-for delete)
(mu4e~headers-defun-mark-for trash)
(mu4e~headers-defun-mark-for flag)
(mu4e~headers-defun-mark-for move)
(mu4e~headers-defun-mark-for read)
(mu4e~headers-defun-mark-for unflag)
(mu4e~headers-defun-mark-for untrash)
(mu4e~headers-defun-mark-for unmark)
(mu4e~headers-defun-mark-for unread)
(mu4e~headers-defun-mark-for action)

;;; Headers-mode and mode-map

(defvar mu4e-headers-mode-map nil
  "Keymap for *mu4e-headers* buffers.")
(unless mu4e-headers-mode-map
  (setq mu4e-headers-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "j" 'mu4e~headers-jump-to-maildir)
          (define-key map "O" 'mu4e-headers-change-sorting)
	  (define-key map "M" 'mu4e-headers-toggle-setting)

	  ;; these are impossible to remember; use mu4e-headers-toggle-setting
	  ;; instead :)
          (define-key map "P" 'mu4e-headers-toggle-threading)
          (define-key map "Q" 'mu4e-headers-toggle-full-search)
          (define-key map "W" 'mu4e-headers-toggle-include-related)
          (define-key map "V" 'mu4e-headers-toggle-skip-duplicates)

          (define-key map "q" 'mu4e~headers-quit-buffer)
          (define-key map "g" 'mu4e-search-rerun) ;; for compatibility

          (define-key map "%" 'mu4e-headers-mark-pattern)
          (define-key map "t" 'mu4e-headers-mark-subthread)
          (define-key map "T" 'mu4e-headers-mark-thread)

	  (define-key map "," #'mu4e-sexp-at-point)

          ;; navigation between messages
          (define-key map "p" 'mu4e-headers-prev)
          (define-key map "n" 'mu4e-headers-next)
          (define-key map (kbd "<M-up>") 'mu4e-headers-prev)
          (define-key map (kbd "<M-down>") 'mu4e-headers-next)

          (define-key map (kbd "[") 'mu4e-headers-prev-unread)
          (define-key map (kbd "]") 'mu4e-headers-next-unread)

          ;; change the number of headers
          (define-key map (kbd "C-+") 'mu4e-headers-split-view-grow)
          (define-key map (kbd "C--") 'mu4e-headers-split-view-shrink)
          (define-key map (kbd "<C-kp-add>") 'mu4e-headers-split-view-grow)
          (define-key map (kbd "<C-kp-subtract>") 'mu4e-headers-split-view-shrink)

          ;; switching to view mode (if it's visible)
          (define-key map "y" 'mu4e-select-other-view)

          ;; marking/unmarking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (define-key map (kbd "<backspace>")  'mu4e-headers-mark-for-trash)
          (define-key map (kbd "d")            'mu4e-headers-mark-for-trash)
          (define-key map (kbd "<delete>")     'mu4e-headers-mark-for-delete)
          (define-key map (kbd "<deletechar>") 'mu4e-headers-mark-for-delete)
          (define-key map (kbd "D")            'mu4e-headers-mark-for-delete)
          (define-key map (kbd "m")            'mu4e-headers-mark-for-move)
          (define-key map (kbd "r")            'mu4e-headers-mark-for-refile)

          (define-key map (kbd "?")            'mu4e-headers-mark-for-unread)
          (define-key map (kbd "!")            'mu4e-headers-mark-for-read)
          (define-key map (kbd "A")            'mu4e-headers-mark-for-action)

          (define-key map (kbd "u")            'mu4e-headers-mark-for-unmark)
          (define-key map (kbd "+")            'mu4e-headers-mark-for-flag)
          (define-key map (kbd "-")            'mu4e-headers-mark-for-unflag)
          (define-key map (kbd "=")            'mu4e-headers-mark-for-untrash)
          (define-key map (kbd "&")            'mu4e-headers-mark-custom)

          (define-key map (kbd "*")              'mu4e-headers-mark-for-something)
          (define-key map (kbd "<kp-multiply>")  'mu4e-headers-mark-for-something)
          (define-key map (kbd "<insertchar>")   'mu4e-headers-mark-for-something)
          (define-key map (kbd "<insert>")       'mu4e-headers-mark-for-something)

          (define-key map (kbd "#")   'mu4e-mark-resolve-deferred-marks)

          (define-key map "U" 'mu4e-mark-unmark-all)
          (define-key map "x" 'mu4e-mark-execute-all)

          (define-key map "a" 'mu4e-headers-action)

          ;; message composition
          (define-key map "R" 'mu4e-compose-reply)
          (define-key map "F" 'mu4e-compose-forward)
          (define-key map "C" 'mu4e-compose-new)
          (define-key map "E" 'mu4e-compose-edit)

          (define-key map (kbd "RET") 'mu4e-headers-view-message)
          (define-key map [mouse-2]   'mu4e-headers-view-message)

          (define-key map "$" 'mu4e-show-log)
          (define-key map "H" 'mu4e-display-manual)

          (define-key map "|" 'mu4e-view-pipe)

          ;; menu
          ;;(define-key map [menu-bar] (make-sparse-keymap))
          (let ((menumap (make-sparse-keymap)))
            (define-key map [menu-bar headers] (cons "Mu4e" menumap))

            (define-key menumap [mu4e~headers-quit-buffer]
              '("Quit view" . mu4e~headers-quit-buffer))
            (define-key menumap [display-help] '("Help" . mu4e-display-manual))

            (define-key menumap [sepa0] '("--"))

            (define-key menumap [toggle-include-related]
              '(menu-item "Toggle related messages"
                          mu4e-headers-toggle-include-related
                          :button (:toggle .
                                           (and (boundp 'mu4e-headers-include-related)
                                                mu4e-headers-include-related))))
            (define-key menumap [toggle-threading]
              '(menu-item "Toggle threading" mu4e-headers-toggle-threading
                          :button (:toggle .
                                           (and (boundp 'mu4e-search-threads)
                                                mu4e-search-threads))))

            (define-key menumap "|" '("Pipe through shell" . mu4e-view-pipe))
            (define-key menumap [sepa1] '("--"))

            (define-key menumap [execute-marks]  '("Execute marks"
                                                   . mu4e-mark-execute-all))
            (define-key menumap [unmark-all]  '("Unmark all" . mu4e-mark-unmark-all))
            (define-key menumap [unmark]
              '("Unmark" . mu4e-headers-mark-for-unmark))

            (define-key menumap [mark-pattern]  '("Mark pattern" .
                                                  mu4e-headers-mark-pattern))
            (define-key menumap [mark-as-read]  '("Mark as read" .
                                                  mu4e-headers-mark-for-read))
            (define-key menumap [mark-as-unread]
              '("Mark as unread" .  mu4e-headers-mark-for-unread))

            (define-key menumap [mark-delete]
              '("Mark for deletion" . mu4e-headers-mark-for-delete))
            (define-key menumap [mark-untrash]
              '("Mark for untrash" .  mu4e-headers-mark-for-untrash))
            (define-key menumap [mark-trash]
              '("Mark for trash" .  mu4e-headers-mark-for-trash))
            (define-key menumap [mark-move]
              '("Mark for move" . mu4e-headers-mark-for-move))
            (define-key menumap [sepa2] '("--"))

            (define-key menumap [resend]  '("Resend" . mu4e-compose-resend))
            (define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
            (define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
            (define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))

            (define-key menumap [sepa3] '("--"))

            (define-key menumap [query-next]
              '("Next query" . mu4e-headers-query-next))
            (define-key menumap [query-prev]  '("Previous query" .
                                                mu4e-headers-query-prev))
            (define-key menumap [narrow-search] '("Narrow search" .
                                                  mu4e-headers-search-narrow))
            (define-key menumap [bookmark]  '("Search bookmark" .
                                              mu4e-headers-search-bookmark))
            (define-key menumap [jump]  '("Jump to maildir" .
                                          mu4e~headers-jump-to-maildir))
            (define-key menumap [refresh]  '("Refresh" . mu4e-search-rerun))
            (define-key menumap [search]  '("Search" . mu4e-headers-search))

            (define-key menumap [sepa4] '("--"))

            (define-key menumap [view]  '("View" . mu4e-headers-view-message))
            (define-key menumap [next]  '("Next" . mu4e-headers-next))
            (define-key menumap [previous]  '("Previous" . mu4e-headers-prev))
            (define-key menumap [sepa5] '("--")))
          map)))
(fset 'mu4e-headers-mode-map mu4e-headers-mode-map)

(defun mu4e~header-line-format ()
  "Get the format for the header line."
  (let ((uparrow   (if mu4e-use-fancy-chars " ▲" " ^"))
        (downarrow (if mu4e-use-fancy-chars " ▼" " V")))
    (cons
     (make-string
      (+ mu4e--mark-fringe-len (floor (fringe-columns 'left t))) ?\s)
     (mapcar
      (lambda (item)
        (let* ( ;; with threading enabled, we're necessarily sorting by date.
               (sort-field (if mu4e-search-threads :date mu4e-headers-sort-field))
               (field (car item)) (width (cdr item))
               (info (cdr (assoc field
                                 (append mu4e-header-info mu4e-header-info-custom))))
	       (sortable (plist-get info :sortable))
               ;; if sortable, it is either t (when field is sortable itself)
               ;; or a symbol (if another field is used for sorting)
               (this-field (when sortable (if (booleanp sortable) field sortable)))
               (help (plist-get info :help))
               ;; triangle to mark the sorted-by column
               (arrow
                (when (and sortable (eq this-field sort-field))
                  (if (eq mu4e-headers-sort-direction 'descending) downarrow uparrow)))
               (name (concat (plist-get info :shortname) arrow))
               (map (make-sparse-keymap)))
          (when sortable
            (define-key map [header-line mouse-1]
              (lambda (&optional e)
                ;; getting the field, inspired by `tabulated-list-col-sort'
                (interactive "e")
                (let* ((obj (posn-object (event-start e)))
                       (field
                        (and obj (get-text-property 0 'field (car obj)))))
                  ;; "t": if we're already sorted by field, the sort-order is
                  ;; changed
                  (mu4e-headers-change-sorting field t)))))
          (concat
           (propertize
            (if width
                (truncate-string-to-width name width 0 ?\s truncate-string-ellipsis)
              name)
            'face (when arrow 'bold)
            'help-echo help
            'mouse-face (when sortable 'highlight)
            'keymap (when sortable map)
            'field field) " ")))
      mu4e-headers-fields))))

(defun mu4e~headers-maybe-auto-update ()
  "Update the current headers buffer after indexing has brought
some changes, `mu4e-headers-auto-update' is non-nil and there is
no user-interaction ongoing."
  (when (and mu4e-headers-auto-update          ;; must be set
	     mu4e-index-update-status
	     (not (zerop (plist-get mu4e-index-update-status :updated)))
             (zerop (mu4e-mark-marks-num))     ;; non active marks
             (not (active-minibuffer-window))) ;; no user input only
    ;; rerun search if there's a live window with search results;
    ;; otherwise we'd trigger a headers view from out of nowhere.
    (when (and (buffer-live-p (mu4e-get-headers-buffer))
               (window-live-p (get-buffer-window (mu4e-get-headers-buffer) t)))
      (mu4e-search-rerun))))

(defcustom mu4e-headers-eldoc-format "“%s” from %f on %d"
  "Format for the `eldoc' string for the current message in the headers buffer.
The following specs are supported:
- %s: the message Subject
- %f: the message From
- %t: the message To
- %c: the message Cc
- %d: the message Date
- %p: the message priority
- %m: the maildir containing the message
- %F: the message’s flags
- %M: the Message-Id"
  :type 'string
  :group 'mu4e-headers)

(defun mu4e-headers-eldoc-function (&rest _args)
  (let ((msg (get-text-property (point) 'msg)))
    (when msg
      (format-spec
       mu4e-headers-eldoc-format
       `((?s . ,(mu4e-message-field msg :subject))
         (?f . ,(mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (?t . ,(mu4e~headers-contact-str (mu4e-message-field msg :to)))
         (?c . ,(mu4e~headers-contact-str (mu4e-message-field msg :cc)))
         (?d . ,(mu4e~headers-human-date msg))
         (?p . ,(mu4e-message-field msg :priority))
         (?m . ,(mu4e-message-field msg :maildir))
         (?F . ,(mu4e-message-field msg :flags))
         (?M . ,(mu4e-message-field msg :message-id)))))))

(define-derived-mode mu4e-headers-mode special-mode
  "mu4e:headers"
  "Major mode for displaying mu4e search results.
\\{mu4e-headers-mode-map}."
  (use-local-map mu4e-headers-mode-map)
  (make-local-variable 'mu4e~headers-proc)
  (make-local-variable 'mu4e~highlighted-docid)
  (set (make-local-variable 'hl-line-face) 'mu4e-header-highlight-face)

  ;; Eldoc support
  (when (featurep 'eldoc)
    (if (boundp 'eldoc-documentation-functions)
        ;; Emacs 28 or newer
        (add-hook 'eldoc-documentation-functions
                  #'mu4e-headers-eldoc-function nil t)
      ;; Emacs 27 or older
      (when (fboundp 'add-function) ;; add-function was added in 24.4.
        (add-function :before-until (local 'eldoc-documentation-function)
                      #'mu4e-headers-eldoc-function))))

  ;; support bookmarks.
  (set (make-local-variable 'bookmark-make-record-function)
       'mu4e--make-bookmark-record)
  ;; maybe update the current headers upon indexing changes
  (add-hook 'mu4e-index-updated-hook #'mu4e~headers-maybe-auto-update)
  (setq
   truncate-lines t
   buffer-undo-list t ;; don't record undo information
   overwrite-mode nil
   header-line-format (mu4e~header-line-format))

  (mu4e--mark-initialize) ;; initialize the marking subsystem
  (mu4e-context-minor-mode)
  (mu4e-update-minor-mode)
  (mu4e-search-minor-mode)
  (hl-line-mode 1))

;;; Highlighting

(defvar mu4e~highlighted-docid nil
  "The highlighted docid")

(defun mu4e~headers-highlight (docid)
  "Highlight the header with DOCID, or do nothing if it's not found.
Also, unhighlight any previously highlighted headers."
  (with-current-buffer (mu4e-get-headers-buffer)
    (save-excursion
      ;; first, unhighlight the previously highlighted docid, if any
      (when (and docid mu4e~highlighted-docid
                 (mu4e~headers-goto-docid mu4e~highlighted-docid))
        (hl-line-unhighlight))
      ;; now, highlight the new one
      (when (mu4e~headers-goto-docid docid)
        (hl-line-highlight)))
    (setq mu4e~highlighted-docid docid)))

;;; Misc 2

(defun mu4e~headers-select-window ()
  "When there is a visible window for the headers buffer, make sure
to select it. This is needed when adding new headers, otherwise
adding a lot of new headers looks really choppy."
  (let ((win (get-buffer-window (mu4e-get-headers-buffer))))
    (when win (select-window win))))

(defun mu4e-headers-goto-message-id (msgid)
  "Go to the next message with message-id MSGID. Return the
message plist, or nil if not found."
  (mu4e-headers-find-if
   (lambda (msg)
     (let ((this-msgid (mu4e-message-field msg :message-id)))
       (when (and this-msgid (string= msgid this-msgid))
         msg)))))

;;; Marking 2

(defun mu4e~headers-mark (docid mark)
  "(Visually) mark the header for DOCID with character MARK."
  (with-current-buffer (mu4e-get-headers-buffer)
    (let ((inhibit-read-only t) (oldpoint (point)))
      (unless (mu4e~headers-goto-docid docid)
        (mu4e-error "Cannot find message with docid %S" docid))
      ;; now, we're at the beginning of the header, looking at
      ;; <docid>\004
      ;; (which is invisible). jump past that…
      (unless (re-search-forward mu4e~headers-docid-post nil t)
        (mu4e-error "Cannot find the `mu4e~headers-docid-post' separator"))

      ;; clear old marks, and add the new ones.
      (let ((msg (get-text-property (point) 'msg)))
        (delete-char mu4e--mark-fringe-len)
        (insert (propertize
                 (format mu4e--mark-fringe-format mark)
                 'face 'mu4e-header-marks-face
                 'docid docid
                 'msg msg)))
      (goto-char oldpoint))))


;;; Queries & searching
(defvar mu4e~headers-mode-line-label "")
(defun mu4e~headers-update-mode-line ()
  "Update mode-line settings."
    (let* ((flagstr
            (mapconcat
	     (lambda (flag-cell)
               (if (car flag-cell)
                   (if mu4e-use-fancy-chars
                       (cddr flag-cell) (cadr flag-cell) ) ""))
             `((,mu4e-search-full             . ,mu4e-headers-full-label)
               (,mu4e-headers-include-related . ,mu4e-headers-related-label)
               (,mu4e-search-threads          . ,mu4e-headers-threaded-label)
	       (,mu4e-headers-skip-duplicates . ,mu4e-headers-skip-duplicates-label))
             ""))
           (name "mu4e-headers"))

      (setq mode-name name)
      (setq mu4e~headers-mode-line-label (concat flagstr " " mu4e--search-last-query))

      (make-local-variable 'global-mode-string)

      (add-to-list 'global-mode-string
                   `(:eval
                     (concat
                      (propertize
                       (mu4e-quote-for-modeline ,mu4e~headers-mode-line-label)
                       'face 'mu4e-modeline-face)
                      " "
                      (if (and mu4e-display-update-status-in-modeline
                               (buffer-live-p mu4e--update-buffer)
                               (process-live-p (get-buffer-process
                                                mu4e--update-buffer)))
                          (propertize " (updating)" 'face 'mu4e-modeline-face)
                        ""))))))


(defun mu4e~headers-redraw-get-view-window ()
  "Close all windows, redraw the headers buffer based on the value
of `mu4e-split-view', and return a window for the message view."
  (if (eq mu4e-split-view 'single-window)
      (or (and (buffer-live-p (mu4e-get-view-buffer))
               (get-buffer-window (mu4e-get-view-buffer)))
          (selected-window))
    (mu4e-hide-other-mu4e-buffers)
    (unless (buffer-live-p (mu4e-get-headers-buffer))
      (mu4e-error "No headers buffer available"))
    (switch-to-buffer (mu4e-get-headers-buffer))
    ;; kill the existing view buffer
    (when (buffer-live-p (mu4e-get-view-buffer))
      (kill-buffer (mu4e-get-view-buffer)))
    ;; get a new view window
    (setq mu4e~headers-view-win
          (with-demoted-errors "Unable to split window: %S"
            (cond
             ((eq mu4e-split-view 'horizontal) ;; split horizontally
              (split-window-vertically mu4e-headers-visible-lines))
             ((eq mu4e-split-view 'vertical) ;; split vertically
              (split-window-horizontally mu4e-headers-visible-columns))
             ((functionp mu4e-split-view)
              (funcall mu4e-split-view))
             (t ;; no splitting; just use the currently selected one
              (selected-window)))))))

;;; Search-based marking

(defun mu4e-headers-for-each (func)
  "Call FUNC for each header, moving point to the header.
FUNC receives one argument, the message s-expression for the
corresponding header."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward mu4e~headers-docid-pre nil t)
      ;; not really sure why we need to jump to bol; we do need to, otherwise we
      ;; miss lines sometimes...
      (let ((msg (get-text-property (line-beginning-position) 'msg)))
        (when msg
          (funcall func msg))))))

(defun mu4e-headers-find-if (func &optional backward)
  "Move to the next header for which FUNC returns non-`nil',
starting from the current position. FUNC receives one argument, the
message s-expression for the corresponding header. If BACKWARD is
non-`nil', search backwards. Returns the new position, or `nil' if
nothing was found. If you want to exclude matches for the current
message, you can use `mu4e-headers-find-if-next'."
  (let ((pos)
        (search-func (if backward 'search-backward 'search-forward)))
    (save-excursion
      (while (and (null pos)
                  (funcall search-func mu4e~headers-docid-pre nil t))
        ;; not really sure why we need to jump to bol; we do need to, otherwise
        ;; we miss lines sometimes...
        (let ((msg (get-text-property (line-beginning-position) 'msg)))
          (when (and msg (funcall func msg))
            (setq pos (point))))))
    (when pos
      (goto-char pos))))

(defun mu4e-headers-find-if-next (func &optional backwards)
  "Like `mu4e-headers-find-if', but do not match the current header.
Move to the next or (if BACKWARDS is non-`nil') header for which FUNC
returns non-`nil', starting from the current position."
  (let ((pos))
    (save-excursion
      (if backwards
          (beginning-of-line)
        (end-of-line))
      (setq pos (mu4e-headers-find-if func backwards)))
    (when pos (goto-char pos))))

(defvar mu4e~headers-regexp-hist nil
  "History list of regexps used.")

(defun mu4e-headers-mark-for-each-if (markpair mark-pred &optional param)
  "Mark all headers for which predicate function MARK-PRED returns
non-nil with MARKPAIR. MARK-PRED is function that receives two
arguments, MSG (the message at point) and PARAM (a user-specified
parameter). MARKPAIR is a cell (MARK . TARGET); see
`mu4e-mark-at-point' for details about marks."
  (mu4e-headers-for-each
   (lambda (msg)
     (when (funcall mark-pred msg param)
       (mu4e-mark-at-point (car markpair) (cdr markpair))))))

(defun mu4e-headers-mark-pattern ()
  "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (let ((markpair (mu4e--mark-get-markpair "Mark matched messages with: " t))
        (field (mu4e-read-option "Field to match: "
                                 '( ("subject" . :subject)
                                    ("from"    . :from)
                                    ("to"      . :to)
                                    ("cc"      . :cc)
                                    ("bcc"     . :bcc)
                                    ("list"    . :list))))
        (pattern (read-string
                  (mu4e-format "Regexp:")
                  nil 'mu4e~headers-regexp-hist)))
    (mu4e-headers-mark-for-each-if
     markpair
     (lambda (msg _param)
       (let* ((value (mu4e-msg-field msg field)))
         (if (member field '(:to :from :cc :bcc :reply-to))
             (cl-find-if (lambda (contact)
                           (let ((name (mu4e-contact-name contact))
				 (email (mu4e-contact-email contact)))
                             (or (and name (string-match pattern name))
                                 (and email (string-match pattern email))))) value)
           (string-match pattern (or value ""))))))))

(defun mu4e-headers-mark-custom ()
  "Mark messages based on a user-provided predicate function."
  (interactive)
  (let* ((pred (mu4e-read-option "Match function: "
                                 mu4e-headers-custom-markers))
         (param (when (cdr pred) (eval (cdr pred))))
         (markpair (mu4e--mark-get-markpair "Mark matched messages with: " t)))
    (mu4e-headers-mark-for-each-if markpair (car pred) param)))

(defun mu4e~headers-get-thread-info (msg what)
  "Get WHAT (a symbol, either path or thread-id) for MSG."
  (let* ((meta (or (mu4e-message-field msg :meta)
                     (mu4e-error "No thread info found")))
         (path  (or (plist-get meta :path)
                    (mu4e-error "No threadpath found"))))
    (cl-case what
      (path path)
      (thread-id
       (save-match-data
         ;; the thread id is the first segment of the thread path
         (when (string-match "^\\([[:xdigit:]]+\\):?" path)
           (match-string 1 path))))
      (otherwise (mu4e-error "Not supported")))))

(defun mu4e-headers-mark-thread-using-markpair (markpair &optional subthread)
  "Mark the thread at point using the given markpair. If SUBTHREAD is
non-nil, marking is limited to the message at point and its
descendants."
  (let* ((mark (car markpair))
         (allowed-marks (mapcar 'car mu4e-marks)))
    (unless (memq mark allowed-marks)
      (mu4e-error "The mark (%s) has to be one of: %s"
                  mark allowed-marks)))
  ;; note: the thread id is shared by all messages in a thread
  (let* ((msg (mu4e-message-at-point))
         (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
         (path      (mu4e~headers-get-thread-info msg 'path))
         ;; the thread path may have a ':z' suffix for sorting;
         ;; remove it for subthread matching.
         (match-path (replace-regexp-in-string ":z$" "" path))
         (last-marked-point))
    (mu4e-headers-for-each
     (lambda (cur-msg)
       (let ((cur-thread-id   (mu4e~headers-get-thread-info cur-msg 'thread-id))
             (cur-thread-path (mu4e~headers-get-thread-info cur-msg 'path)))
         (if subthread
             ;; subthread matching; mymsg's thread path should have path as its
             ;; prefix
             (when (string-match (concat "^" match-path) cur-thread-path)
               (mu4e-mark-at-point (car markpair) (cdr markpair))
               (setq last-marked-point (point)))
           ;; nope; not looking for the subthread; looking for the whole thread
           (when (string= thread-id cur-thread-id)
             (mu4e-mark-at-point (car markpair) (cdr markpair))
             (setq last-marked-point (point)))))))
    (when last-marked-point
      (goto-char last-marked-point)
      (mu4e-headers-next))))

(defun mu4e-headers-mark-thread (&optional subthread markpair)
  "Like `mu4e-headers-mark-thread-using-markpair' but prompt for the markpair."
  (interactive
   (let* ((subthread current-prefix-arg))
     (list current-prefix-arg
           ;; FIXME: e.g., for refiling we should evaluate this
           ;; for each line separately
           (mu4e--mark-get-markpair
            (if subthread "Mark subthread with: " "Mark whole thread with: ")
            t))))
  (mu4e-headers-mark-thread-using-markpair markpair subthread))

(defun mu4e-headers-mark-subthread (&optional markpair)
  "Like `mu4e-mark-thread', but only for a sub-thread."
  (interactive)
  (if markpair (mu4e-headers-mark-thread t markpair)
    (let ((current-prefix-arg t))
      (call-interactively 'mu4e-headers-mark-thread))))


;;; Interactive functions
(defun mu4e-headers-change-sorting (&optional field dir)
  "Change the sorting/threading parameters.
FIELD is the field to sort by; DIR is a symbol: either
`ascending', `descending', t (meaning: if FIELD is the same as
the current sortfield, change the sort-order) or nil (ask the
user)."
  (interactive)
  (let* ((field
          (or field
              (mu4e-read-option "Sortfield: " mu4e~headers-sort-field-choices)))
         ;; note: 'sortable' is either a boolean (meaning: if non-nil, this is
         ;; sortable field), _or_ another field (meaning: sort by this other field).
         (sortable (plist-get (cdr (assoc field mu4e-header-info)) :sortable))
         ;; error check
         (sortable
          (if sortable
              sortable
            (mu4e-error "Not a sortable field")))
         (sortfield (if (booleanp sortable) field sortable))
         (dir
          (cl-case dir
            ((ascending descending) dir)
            ;; change the sort order if field = curfield
            (t
             (if (eq sortfield mu4e-headers-sort-field)
                 (if (eq mu4e-headers-sort-direction 'ascending)
                     'descending 'ascending)
               'descending)))))
    (setq
     mu4e-headers-sort-field sortfield
     mu4e-headers-sort-direction dir)
    (mu4e-message "Sorting by %s (%s)"
                  (symbol-name sortfield)
                  (symbol-name mu4e-headers-sort-direction))
    (mu4e-search-rerun)))


(defun mu4e-headers-toggle-setting (&optional dont-refresh)
  "Toggle some aspect of headers display.
When prefix-argument DONT-REFRESH is non-nill, do not refresh the
last search with the new setting."
  (interactive "P")
  (let* ((toggles '(("fFull-search"      . mu4e-search-full)
		    ("rInclude-related"  . mu4e-headers-include-related)
		    ("tShow threads"     . mu4e-search-threads)
		    ("uSkip duplicates"  . mu4e-headers-skip-duplicates)))
	 (toggles (seq-map
		   (lambda (cell)
		     (cons
		      (concat (car cell) (format" (%s)"
						(if (symbol-value (cdr cell)) "on" "off")))
		      (cdr cell))) toggles))
	 (choice (mu4e-read-option "Toggle setting " toggles)))
    (when choice
      (set choice (not (symbol-value choice)))
      (mu4e-message "Set `%s' to %s" (symbol-name choice) (symbol-value choice))
      (unless dont-refresh
	(mu4e-search-rerun)))))


(defun mu4e~headers-toggle (name togglevar dont-refresh)
  "Toggle variable TOGGLEVAR for feature NAME. Unless DONT-REFRESH is non-nil,
re-run the last search."
  (set togglevar (not (symbol-value togglevar)))
  (mu4e-message "%s turned %s%s"
                name
                (if (symbol-value togglevar) "on" "off")
                (if dont-refresh
                    " (press 'g' to refresh)" ""))
  (unless dont-refresh
    (mu4e-search-rerun)))

(defun mu4e-headers-toggle-threading (&optional dont-refresh)
  "Toggle `mu4e-search-threads'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (mu4e~headers-toggle "Threading" 'mu4e-search-threads dont-refresh))

(defun mu4e-headers-toggle-full-search (&optional dont-refresh)
  "Toggle `mu4e-search-full'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (mu4e~headers-toggle "Full-search"
                       'mu4e-search-full dont-refresh))

(defun mu4e-headers-toggle-include-related (&optional dont-refresh)
  "Toggle `mu4e-headers-include-related'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (mu4e~headers-toggle "Include-related"
                       'mu4e-headers-include-related dont-refresh))

(defun mu4e-headers-toggle-skip-duplicates (&optional dont-refresh)
  "Toggle `mu4e-headers-skip-duplicates'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (mu4e~headers-toggle "Skip-duplicates"
                       'mu4e-headers-skip-duplicates dont-refresh))

(defvar mu4e~headers-loading-buf nil
  "A buffer for loading a message view.")

(defun mu4e-headers-view-message ()
  "View message at point                                    .
If there's an existing window for the view, re-use that one . If
not, create a new one, depending on the value of
`mu4e-split-view': if it's a symbol `horizontal' or `vertical',
split the window accordingly; if it is nil, replace the current
window                                                      . "
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((msg (mu4e-message-at-point))
	 (path (mu4e-message-field msg :path))
	 (_exists (or (file-readable-p  path)
		     (mu4e-warn "No message at %s" path)))
         (docid (or (mu4e-message-field msg :docid)
                    (mu4e-warn "No message at point")))
         (mark-as-read
          (if (functionp mu4e-view-auto-mark-as-read)
              (funcall mu4e-view-auto-mark-as-read msg)
            mu4e-view-auto-mark-as-read))
         (viewwin (mu4e~headers-redraw-get-view-window)))
    (unless (window-live-p viewwin)
      (mu4e-error "Cannot get a message view"))
    (select-window viewwin)

    ;; show some 'loading...' buffer
    (unless (buffer-live-p mu4e~headers-loading-buf)
      (setq mu4e~headers-loading-buf (get-buffer-create " *mu4e-loading*"))
      (with-current-buffer mu4e~headers-loading-buf
        (mu4e-loading-mode)))

    (switch-to-buffer mu4e~headers-loading-buf)
    (mu4e--server-view docid mark-as-read)))


(defun mu4e~headers-move (lines)
  "Move point LINES lines.
Move foward if LINES is positive or backwards if LINES is
negative. If this succeeds, return the new docid. Otherwise,
return nil."
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (cl-flet ((goto-next-line
              (arg)
              (condition-case _err
                  (prog1
                      (let (line-move-visual)
			(and (line-move arg) 0))
                    ;; Skip invisible text at BOL possibly hidden by
                    ;; the end of another invisible overlay covering
                    ;; previous EOL.
                    (move-to-column 2))
		((beginning-of-buffer end-of-buffer)
                 1))))
    (let* ((succeeded (zerop (goto-next-line lines)))
           (docid (mu4e~headers-docid-at-point)))
      ;; move point, even if this function is called when this window is not
      ;; visible
      (when docid
        ;; update all windows showing the headers buffer
        (walk-windows
         (lambda (win)
           (when (eq (window-buffer win) (mu4e-get-headers-buffer))
             (set-window-point win (point))))
         nil t)
        (if (eq mu4e-split-view 'single-window)
            (when (eq (window-buffer) (mu4e-get-view-buffer))
              (mu4e-headers-view-message))
          ;; update message view if it was already showing
          (when (and mu4e-split-view (window-live-p mu4e~headers-view-win))
            (mu4e-headers-view-message)))
        ;; attempt to highlight the new line, display the message
        (mu4e~headers-highlight docid)
        (if succeeded
	    docid
	  nil)))))

(defun mu4e-headers-next (&optional n)
  "Move point to the next message header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth next
header."
  (interactive "P")
  (mu4e~headers-move (or n 1)))

(defun mu4e-headers-prev (&optional n)
  "Move point to the previous message header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth
previous header."
  (interactive "P")
  (mu4e~headers-move (- (or n 1))))

(defun mu4e~headers-prev-or-next-unread (backwards)
  "Move point to the next message that is unread (and
untrashed). If BACKWARDS is non-`nil', move backwards."
  (interactive)
  (or (mu4e-headers-find-if-next
       (lambda (msg)
         (let ((flags (mu4e-message-field msg :flags)))
           (and (member 'unread flags) (not (member 'trashed flags)))))
       backwards)
      (mu4e-message (format "No %s unread message found"
                            (if backwards "previous" "next")))))

(defun mu4e-headers-prev-unread ()
  "Move point to the previous message that is unread (and
untrashed)."
  (interactive)
  (mu4e~headers-prev-or-next-unread t))

(defun mu4e-headers-next-unread ()
  "Move point to the next message that is unread (and
untrashed)."
  (interactive)
  (mu4e~headers-prev-or-next-unread nil))

(defun mu4e~headers-jump-to-maildir (maildir &optional edit)
  "Show the messages in maildir.
The user is prompted to ask what maildir.  If prefix arg EDIT is
given, offer to edit the search query before executing it."
  (interactive
   (let ((maildir (mu4e-ask-maildir "Jump to maildir: ")))
     (list maildir current-prefix-arg)))
  (when maildir
    (let* ((query (format "maildir:\"%s\"" maildir))
           (query (if edit (mu4e-search-read-query "Refine query: " query) query)))
      (mu4e-mark-handle-when-leaving)
      (mu4e-search query))))

(defun mu4e-headers-split-view-grow (&optional n)
  "In split-view, grow the headers window.
In horizontal split-view, increase the number of lines shown by N.
In vertical split-view, increase the number of columns shown by N.
If N is negative shrink the headers window.  When not in split-view
do nothing."
  (interactive "P")
  (let ((n (or n 1))
        (hwin (get-buffer-window (mu4e-get-headers-buffer))))
    (when (and (buffer-live-p (mu4e-get-view-buffer)) (window-live-p hwin))
      (let ((n (or n 1)))
        (cl-case mu4e-split-view
          ;; emacs has weird ideas about what horizontal, vertical means...
          (horizontal
           (window-resize hwin n nil)
           (cl-incf mu4e-headers-visible-lines n))
          (vertical
           (window-resize hwin n t)
           (cl-incf mu4e-headers-visible-columns n)))))))

(defun mu4e-headers-split-view-shrink (&optional n)
  "In split-view, shrink the headers window.
In horizontal split-view, decrease the number of lines shown by N.
In vertical split-view, decrease the number of columns shown by N.
If N is negative grow the headers window.  When not in split-view
do nothing."
  (interactive "P")
  (mu4e-headers-split-view-grow (- (or n 1))))

(defun mu4e-headers-action (&optional actionfunc)
  "Ask user what to do with message-at-point, then do it.
The actions are specified in `mu4e-headers-actions'. Optionally,
pass ACTIONFUNC, which is a function that takes a msg-plist
argument."
  (interactive)
  (let ((msg (mu4e-message-at-point))
        (afunc (or actionfunc (mu4e-read-option "Action: " mu4e-headers-actions))))
    (funcall afunc msg)))

(defun mu4e-headers-mark-and-next (mark)
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
  (interactive)
  (mu4e-mark-set mark)
  (when mu4e-headers-advance-after-mark (mu4e-headers-next)))

(defun mu4e~headers-quit-buffer ()
  "Quit the mu4e-headers buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (if (eq mu4e-split-view 'single-window)
      (progn (mu4e-mark-handle-when-leaving)
             (kill-buffer))
    (unless (eq major-mode 'mu4e-headers-mode)
      (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
    (mu4e-mark-handle-when-leaving)
    (let ((curbuf (current-buffer))
          (curwin (selected-window)))
      (walk-windows
       (lambda (win)
         (with-selected-window win
           ;; if we the view window connected to this one, kill it
           (when (and (not (one-window-p win)) (eq mu4e~headers-view-win win))
             (delete-window win)
             (setq mu4e~headers-view-win nil)))
         ;; and kill any _other_ (non-selected) window that shows the current
         ;; buffer
         (when (and
                (eq curbuf (window-buffer win)) ;; does win show curbuf?
                (not (eq curwin win))             ;; it's not the curwin?
                (not (one-window-p)))           ;; and not the last one?
           (delete-window win))))  ;; delete it!
      ;; now, all *other* windows should be gone. kill ourselves, and return
      ;; to the main view
      (kill-buffer)
      (mu4e--main-view 'refresh))))


;;; Loading messages
;;
(defvar mu4e-loading-mode-map
  (let ((map (make-sparse-keymap)))
          (define-key map "n" #'ignore)
          (define-key map "p" #'ignore)
          (define-key map "q" #'bury-buffer)
          map)
  "Keymap for *mu4e-loading* buffers.")

(define-derived-mode mu4e-loading-mode special-mode
  "mu4e:loading"
  (use-local-map mu4e-loading-mode-map)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Loading message..."
                        'face 'mu4e-system-face 'intangible t))))

(defun mu4e~loading-close ()
  "Bury the mu4e Loading... buffer, if any."
  (let* ((buf mu4e~headers-loading-buf)
	 (win (and (buffer-live-p buf) (get-buffer-window buf t))))
    (when (window-live-p win)
      (delete-window win))))

(provide 'mu4e-headers)
;;; mu4e-headers.el ends here
