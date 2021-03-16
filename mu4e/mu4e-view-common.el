;;; mu4e-view-utils.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

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

;; In this file we define common utils for 'old' and 'gnus' view mode.

;;; Code:

(require 'cl-lib)
(require 'mu4e-utils) ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-headers)
(require 'mu4e-mark)
(require 'mu4e-proc)
(require 'mu4e-compose)
(require 'mu4e-actions)
(require 'mu4e-message)

(require 'comint)
(require 'browse-url)
(require 'button)
(require 'epa)
(require 'epg)
(require 'thingatpt)

;;; Options

(defcustom mu4e-view-scroll-to-next t
  "Move to the next message when calling
`mu4e-view-scroll-up-or-next' (typically bound to SPC) when at
the end of a message. Otherwise, don't move to the next message."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-fields
  '(:from :to  :cc :subject :flags :date :maildir :mailing-list :tags
          :attachments :signature :decryption)
  "Header fields to display in the message view buffer.
For the complete list of available headers, see `mu4e-header-info'."
  :type (list 'symbol)
  :group 'mu4e-view)

(defcustom mu4e-view-actions
  '( ("capture message"  . mu4e-action-capture-message)
     ("view as pdf"      . mu4e-action-view-as-pdf)
     ("show this thread" . mu4e-action-show-thread))
  "List of actions to perform on messages in view mode.
The actions are cons-cells of the form:
  (NAME . FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first letter of NAME is used as a shortcut character."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))


;;; Old options

;; These don't do anything useful when in "gnus" mode, except for avoid errors
;; for people that have these in their config.

(defcustom mu4e-view-show-addresses nil
  "Whether to initially show full e-mail addresses for contacts.
Otherwise, just show their names. Ignored when using the gnus-based view."
  :type 'boolean
  :group 'mu4e-view)

(make-obsolete-variable 'mu4e-view-wrap-lines nil "0.9.9-dev7")
(make-obsolete-variable 'mu4e-view-hide-cited nil "0.9.9-dev7")

(defcustom mu4e-view-date-format "%c"
  "Date format to use in the message view.
In the format of `format-time-string'. Ignored when using the gnus-based view."
  :type 'string
  :group 'mu4e-view)

(defcustom mu4e-view-image-max-width 800
  "The maximum width for images to display.
This is only effective if you're using an Emacs with Imagemagick
support, and `mu4e-view-show-images' is non-nil. Ignored when
using the gnus-based view."
  :type 'integer
  :group 'mu4e-view)

(defcustom mu4e-view-image-max-height 600
  "The maximum height for images to display.
This is only effective if you're using an Emacs with Imagemagick
support, and `mu4e-view-show-images' is non-nil.  Ignored when
using the gnus-based view."
  :type 'integer
  :group 'mu4e-view)


(defcustom mu4e-save-multiple-attachments-without-asking nil
  "If non-nil, saving multiple attachments asks once for a
directory and saves all attachments in the chosen directory.
Ignored when using the gnus-based view."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-attachment-assoc nil
  "Alist of (EXTENSION . PROGRAM).
Specify which PROGRAM to use to open attachment with EXTENSION.
Args EXTENSION and PROGRAM should be specified as strings.
Ignored when using the gnus-based view."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type string))

(defcustom mu4e-view-attachment-actions
  '( ("ssave" . mu4e-view-save-attachment-single)
     ("Ssave multi" . mu4e-view-save-attachment-multi)
     ("wopen-with" . mu4e-view-open-attachment-with)
     ("ein-emacs"  . mu4e-view-open-attachment-emacs)
     ("dimport-in-diary"  . mu4e-view-import-attachment-diary)
     ("kimport-public-key" . mu4e-view-import-public-key)
     ("|pipe"      . mu4e-view-pipe-attachment))
  "List of actions to perform on message attachments.
The actions are cons-cells of the form:
 (NAME . FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives two arguments: the message
  plist and the attachment number.
The first letter of NAME is used as a shortcut character.
Ignored when using the gnus-based view."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))

;;; Keymaps

(defvar mu4e-view-header-field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'mu4e~view-header-field-fold)
    (define-key map (kbd "TAB") 'mu4e~view-header-field-fold)
    map)
  "Keymap used for header fields. Ignored when using the
gnus-based view.")

(defvar mu4e-view-contacts-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'mu4e~view-compose-contact)
    (define-key map "C"  'mu4e~view-compose-contact)
    (define-key map "c"  'mu4e~view-copy-contact)
    map)
  "Keymap used for the contacts in the header fields.
Ignored when using the gnus-based view.")

(defvar mu4e-view-attachments-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'mu4e~view-open-attach-from-binding)
    (define-key map  [?\M-\r] 'mu4e~view-open-attach-from-binding)
    (define-key map [mouse-2] 'mu4e~view-save-attach-from-binding)
    (define-key map (kbd "<S-return>") 'mu4e~view-save-attach-from-binding)
    map)
  "Keymap used in the \"Attachments\" header field. Ignored when
using the gnus-based view.")

;; Helpers

(defun mu4e~view-quit-buffer ()
  "Quit the mu4e-view buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (if (eq mu4e-split-view 'single-window)
      (when (buffer-live-p (mu4e-get-view-buffer))
        (kill-buffer (mu4e-get-view-buffer)))
    (unless (eq major-mode 'mu4e-view-mode)
      (mu4e-error "Must be in mu4e-view-mode (%S)" major-mode))
    (let ((curbuf (current-buffer))
          (curwin (selected-window))
          (headers-win))
      (walk-windows
       (lambda (win)
         ;; check whether the headers buffer window is visible
         (when (eq (mu4e-get-headers-buffer) (window-buffer win))
           (setq headers-win win))
         ;; and kill any _other_ (non-selected) window that shows the current
         ;; buffer
         (when
             (and
              (eq curbuf (window-buffer win)) ;; does win show curbuf?
              (not (eq curwin win))         ;; but it's not the curwin?
              (not (one-window-p))) ;; and not the last one on the frame?
           (delete-window win))))  ;; delete it!
      ;; now, all *other* windows should be gone.
      ;; if the headers view is also visible, kill ourselves + window; otherwise
      ;; switch to the headers view
      (if (window-live-p headers-win)
          ;; headers are visible
          (progn
            (kill-buffer-and-window) ;; kill the view win
            (setq mu4e~headers-view-win nil)
            (select-window headers-win)) ;; and switch to the headers win...
        ;; headers are not visible...
        (progn
          (kill-buffer)
          (setq mu4e~headers-view-win nil)
          (when (buffer-live-p (mu4e-get-headers-buffer))
            (switch-to-buffer (mu4e-get-headers-buffer))))))))


(defconst mu4e~view-raw-buffer-name " *mu4e-raw-view*"
  "Name for the raw message view buffer.")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path))
        (buf (get-buffer-create mu4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (mu4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (view-mode)
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD.
Then, display the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-message-field (mu4e-message-at-point) :path)))
    (mu4e-process-file-through-pipe path cmd)))


(defmacro mu4e~view-in-headers-context (&rest body)
  "Evaluate BODY in the context of the headers buffer connected to
this view."
  `(progn
     (unless (buffer-live-p (mu4e-get-headers-buffer))
       (mu4e-error "no headers buffer connected"))
     (let* ((msg (mu4e-message-at-point))
            (docid (mu4e-message-field msg :docid)))
       (unless docid
         (mu4e-error "message without docid: action is not possible."))
       (with-current-buffer (mu4e-get-headers-buffer)
         (unless (eq mu4e-split-view 'single-window)
           (when (get-buffer-window)
             (select-window (get-buffer-window))))
         (if (mu4e~headers-goto-docid docid)
             ,@body
           (mu4e-error "cannot find message in headers buffer."))))))

(defun mu4e-view-headers-next (&optional n)
  "Move point to the next message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (mu4e~view-in-headers-context
   (mu4e~headers-move (or n 1))))

(defun mu4e-view-headers-prev (&optional n)
  "Move point to the previous message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (mu4e~view-in-headers-context
   (mu4e~headers-move (- (or n 1)))))

(defun mu4e~view-prev-or-next-unread (backwards)
  "Move point to the next or previous (when BACKWARDS is non-`nil')
unread message header in the headers buffer connected with this
message view. If this succeeds, return the new docid. Otherwise,
return nil."
  (mu4e~view-in-headers-context
   (mu4e~headers-prev-or-next-unread backwards))
  (if (eq mu4e-split-view 'single-window)
      (when (eq (window-buffer) (mu4e-get-view-buffer))
        (with-current-buffer (mu4e-get-headers-buffer)
          (mu4e-headers-view-message)))
    (mu4e-select-other-view)
    (mu4e-headers-view-message)))

(defun mu4e-view-headers-prev-unread ()
  "Move point to the previous unread message header in the headers
buffer connected with this message view. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~view-prev-or-next-unread t))

(defun mu4e-view-headers-next-unread ()
  "Move point to the next unread message header in the headers
buffer connected with this message view. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~view-prev-or-next-unread nil))


;;; Interactive functions
(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG, then do it.
If MSG is nil apply action to message returned
bymessage-at-point.  The actions are specified in
`mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (actionfunc (mu4e-read-option "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))

(defun mu4e-view-mark-pattern ()
  "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-mark-pattern)))

(defun mu4e-view-mark-thread (&optional markpair)
  "Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the thread at point in the headers view. The
optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e~view-in-headers-context
   (if markpair (mu4e-headers-mark-thread nil markpair)
     (call-interactively 'mu4e-headers-mark-thread))))

(defun mu4e-view-mark-subthread (&optional markpair)
  "Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the subthread at point in the headers view.
The optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e~view-in-headers-context
   (if markpair (mu4e-headers-mark-subthread markpair)
     (mu4e-headers-mark-subthread))))

(defun mu4e-view-search-narrow ()
  "Run `mu4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context
   (call-interactively 'mu4e-headers-search-narrow)))

(defun mu4e-view-search-edit ()
  "Run `mu4e-headers-search-edit' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-search-edit)))

(defun mu4e-mark-region-code ()
  "Highlight region marked with `message-mark-inserted-region'.
Add this function to `mu4e-view-mode-hook' to enable this feature."
  (require 'message)
  (let (beg end ov-beg ov-end ov-inv)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" message-mark-insert-begin) nil t)
        (setq ov-beg (match-beginning 0)
              ov-end (match-end 0)
              ov-inv (make-overlay ov-beg ov-end)
              beg    ov-end)
        (overlay-put ov-inv 'invisible t)
        (when (re-search-forward
               (concat "^" message-mark-insert-end) nil t)
          (setq ov-beg (match-beginning 0)
                ov-end (match-end 0)
                ov-inv (make-overlay ov-beg ov-end)
                end    ov-beg)
          (overlay-put ov-inv 'invisible t))
        (when (and beg end)
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'mu4e-region-code))
          (setq beg nil end nil))))))

;;; View Utilities

(defun mu4e-view-mark-custom ()
  "Run some custom mark function."
  (mu4e~view-in-headers-context
   (mu4e-headers-mark-custom)))

(defun mu4e~view-split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))

;;; Scroll commands

(defun mu4e-view-scroll-up-or-next ()
  "Scroll-up the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we can't scroll-up
anymore, go the next message."
  (interactive)
  (condition-case nil
      (scroll-up)
    (error
     (when mu4e-view-scroll-to-next
       (mu4e-view-headers-next)))))

(defun mu4e-scroll-up ()
  "Scroll text of selected window up one line."
  (interactive)
  (scroll-up 1))

(defun mu4e-scroll-down ()
  "Scroll text of selected window down one line."
  (interactive)
  (scroll-down 1))

;;; Mark commands

(defun mu4e-view-unmark-all ()
  "If we're in split-view, unmark all messages.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
      (mu4e~view-in-headers-context (mu4e-mark-unmark-all))
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
      (mu4e-view-mark-for-unmark)
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defmacro mu4e~view-defun-mark-for (mark)
  "Define a function mu4e-view-mark-for-MARK."
  (let ((funcname (intern (format "mu4e-view-mark-for-%s" mark)))
        (docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e~view-in-headers-context
               (mu4e-headers-mark-and-next ',mark)))
       (put ',funcname 'definition-name ',mark))))

(mu4e~view-defun-mark-for move)
(mu4e~view-defun-mark-for refile)
(mu4e~view-defun-mark-for delete)
(mu4e~view-defun-mark-for flag)
(mu4e~view-defun-mark-for unflag)
(mu4e~view-defun-mark-for unmark)
(mu4e~view-defun-mark-for something)
(mu4e~view-defun-mark-for read)
(mu4e~view-defun-mark-for unread)
(mu4e~view-defun-mark-for trash)
(mu4e~view-defun-mark-for untrash)

(defun mu4e-view-marked-execute ()
  "Execute the marked actions."
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e-mark-execute-all)))


;;; URL handling

(defvar mu4e~view-link-map nil
  "A map of some number->url so we can jump to url by number.")
(put 'mu4e~view-link-map 'permanent-local t)

(defvar mu4e-view-active-urls-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'mu4e~view-browse-url-from-binding)
    (define-key map (kbd "M-<return>") 'mu4e~view-browse-url-from-binding)
    map)
  "Keymap used for the urls inside the body.")

(defvar mu4e~view-beginning-of-url-regexp
  "https?\\://\\|mailto:"
  "Regexp that matches the beginning of http:/https:/mailto:
URLs; match-string 1 will contain the matched URL, if any.")


(defun mu4e~view-browse-url-from-binding (&optional url)
  "View in browser the url at point, or click location.
If the optional argument URL is provided, browse that instead.
If the url is mailto link, start writing an email to that address."
  (interactive)
  (let* (( url (or url (mu4e~view-get-property-from-event 'mu4e-url))))
    (when url
      (if (string-match-p "^mailto:" url)
          (browse-url-mail url)
        (browse-url url)))))


(defun mu4e~view-get-property-from-event (prop)
  "Get the property PROP at point, or the location of the mouse.
The action is chosen based on the `last-command-event'.
Meant to be evoked from interactive commands."
  (if (and (eventp last-command-event)
           (mouse-event-p last-command-event))
      (let ((posn (event-end last-command-event)))
        (when (numberp (posn-point posn))
          (get-text-property
           (posn-point posn)
           prop
           (window-buffer (posn-window posn)))))
    (get-text-property (point) prop)))

;; this is fairly simplistic...
(defun mu4e~view-activate-urls ()
  "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e~view-link-map ;; buffer local
            (make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
        (let ((bounds (thing-at-point-bounds-of-url-at-point)))
          (when bounds
            (let* ((url (thing-at-point-url-at-point))
                   (ov (make-overlay (car bounds) (cdr bounds))))
              (puthash (cl-incf num) url mu4e~view-link-map)
              (add-text-properties
               (car bounds)
               (cdr bounds)
               `(face mu4e-link-face
                      mouse-face highlight
                      mu4e-url ,url
                      keymap ,mu4e-view-active-urls-keymap
                      help-echo
                      "[mouse-1] or [M-RET] to open the link"))
              (overlay-put ov 'after-string
                           (propertize (format "\u200B[%d]" num)
                                       'face 'mu4e-url-number-face)))))))))


(defun mu4e~view-get-urls-num (prompt &optional multi)
  "Ask the user with PROMPT for an URL number for MSG, and ensure
it is valid. The number is [1..n] for URLs \[0..(n-1)] in the
message. If MULTI is nil, return the number for the URL;
otherwise (MULTI is non-nil), accept ranges of URL numbers, as
per `mu4e-split-ranges-to-numbers', and return the corresponding
string."
  (let* ((count (hash-table-count mu4e~view-link-map)) (def))
    (when (zerop count) (mu4e-error "No links for this message"))
    (if (not multi)
        (if (= count 1)
            (read-number (mu4e-format "%s: " prompt) 1)
          (read-number (mu4e-format "%s (1-%d): " prompt count)))
      (progn
        (setq def (if (= count 1) "1" (format "1-%d" count)))
        (read-string (mu4e-format "%s (default %s): " prompt def)
                     nil nil def)))))

(defun mu4e-view-go-to-url (&optional multi)
  "Offer to go to url(s). If MULTI (prefix-argument) is nil, go to
a single one, otherwise, offer to go to a range of urls."
  (interactive "P")
  (mu4e~view-handle-urls "URL to visit"
                         multi
                         (lambda (url) (mu4e~view-browse-url-from-binding url))))

(defun mu4e-view-save-url (&optional multi)
  "Offer to save urls(s) to the kill-ring. If
MULTI (prefix-argument) is nil, save a single one, otherwise, offer
to save a range of URLs."
  (interactive "P")
  (mu4e~view-handle-urls "URL to save" multi
                         (lambda (url)
                           (kill-new url)
                           (mu4e-message "Saved %s to the kill-ring" url))))

(defun mu4e-view-fetch-url (&optional multi)
  "Offer to fetch (download) urls(s). If MULTI (prefix-argument) is nil,
download a single one, otherwise, offer to fetch a range of
URLs. The urls are fetched to `mu4e-attachment-dir'."
  (interactive "P")
  (mu4e~view-handle-urls "URL to fetch" multi
                         (lambda (url)
                           (let ((target (concat (mu4e~get-attachment-dir url) "/"
                                                 (file-name-nondirectory url))))
                             (url-copy-file url target)
                             (mu4e-message "Fetched %s -> %s" url target)))))

(defun mu4e~view-handle-urls (prompt multi urlfunc)
  "If MULTI is nil, apply URLFUNC to a single uri, otherwise, apply
it to a range of uris. PROMPT is the query to present to the user."
  (if multi
      (mu4e~view-handle-multi-urls prompt urlfunc)
    (mu4e~view-handle-single-url prompt urlfunc)))

(defun mu4e~view-handle-single-url (prompt urlfunc &optional num)
  "Apply URLFUNC to url NUM in the current message, prompting the
user with PROMPT."
  (let* ((num (or num (mu4e~view-get-urls-num prompt)))
         (url (gethash num mu4e~view-link-map)))
    (unless url (mu4e-warn "Invalid number for URL"))
    (funcall urlfunc url)))

(defun mu4e~view-handle-multi-urls (prompt urlfunc)
  "Apply URLFUNC to a a range of urls in the current message,
prompting the user with PROMPT.

Default is to apply it to all URLs, [1..n], where n is the number
of urls. You can type multiple values separated by space, e.g.  1
3-6 8 will visit urls 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which means all urls, but as
this is the default, you may not need it."
  (let* ((linkstr (mu4e~view-get-urls-num
                   "URL number range (or 'a' for 'all')" t))
         (count (hash-table-count mu4e~view-link-map))
         (linknums (mu4e-split-ranges-to-numbers linkstr count)))
    (dolist (num linknums)
      (mu4e~view-handle-single-url prompt urlfunc num))))

(defun mu4e-view-for-each-uri (func)
  "Evaluate FUNC(uri) for each uri in the current message."
  (maphash (lambda (_num uri) (funcall func uri)) mu4e~view-link-map))


(provide 'mu4e-view-common)
