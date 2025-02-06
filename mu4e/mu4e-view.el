;;; mu4e-view.el --- Mode for viewing e-mail messages -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Dirk-Jan C. Binnema

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

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'gnus-art)
(require 'comint)
(require 'browse-url)
(require 'button)
(require 'epa)
(require 'epg)
(require 'thingatpt)

(require 'mu4e-actions)
(require 'mu4e-compose)
(require 'mu4e-context)
(require 'mu4e-headers)
(require 'mu4e-mark)
(require 'mu4e-message)
(require 'mu4e-server)
(require 'mu4e-search)
(require 'mu4e-mime-parts)

;; utility functions
(require 'mu4e-contacts)
(require 'mu4e-vars)

;;; Options

(defcustom mu4e-view-scroll-to-next t
  "Move to the next message with `mu4e-view-scroll-up-or-next'.
When at the end of a message, move to the next one, if any.
Otherwise, don't move to the next message."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-fields
  '(:from :to  :cc :subject :flags :date :maildir :mailing-list :tags)
  "Header fields to display in the message view buffer.

For the complete list of available headers, see
`mu4e-header-info'.

Note, you can use this to add fields that are not otherwise
shown; you can further tweak the other fields using e.g.,
`gnus-visible-headers' and `gnus-ignored-headers' - see the gnus
documentation for details."
  :type '(repeat symbol)
  :group 'mu4e-view)

(defcustom mu4e-view-actions
  (delq nil `(("capture message" . mu4e-action-capture-message)
              ("view in browser" . mu4e-action-view-in-browser)
              ("browse online archive" . mu4e-action-browse-list-archive)
              ,(when (fboundp 'xwidget-webkit-browse-url)
                 '("xview in xwidget" . mu4e-action-view-in-xwidget))
              ("show this thread" . mu4e-action-show-thread)))
  "List of actions to perform on messages in view mode.
The actions are cons-cells of the form:
  (NAME . FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first letter of NAME is used as a shortcut character."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))

(defcustom mu4e-view-max-specpdl-size 4096
  "The value of `max-specpdl-size' for displaying messages with Gnus."
  :type 'integer
  :group 'mu4e-view)

(defconst mu4e--view-raw-buffer-name " *mu4e-raw-view*"
  "Name for the raw message view buffer.")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-message-readable-path))
        (buf (get-buffer-create mu4e--view-raw-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mu4e-raw-view-mode)
        (insert-file-contents path)
        (goto-char (point-min))))
    (mu4e-display-buffer buf t)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD.
Then, display the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-message-readable-path)))
    (mu4e-process-file-through-pipe path cmd)))

(defmacro mu4e--view-in-headers-context (&rest body)
  "Evaluate BODY in the context of the headers buffer."
  `(progn
     (let* ((msg (mu4e-message-at-point))
            (buffer (cond
                     ;; are we already inside a headers buffer?
                     ((mu4e-current-buffer-type-p 'headers) (current-buffer))
                     ;; if not, are we inside a view buffer, and does
                     ;; it have linked headers buffer?
                     ((mu4e-current-buffer-type-p 'view)
                      (when (mu4e--view-detached-p (current-buffer))
                        (mu4e-error
                         "Cannot navigate in a detached view buffer"))
                      (mu4e-get-headers-buffer))
                     ;; fallback; but what would trigger this?
                     (t (mu4e-get-headers-buffer))))
            (docid (mu4e-message-field msg :docid)))
       (unless docid
         (mu4e-error "Message without docid: action is not possible"))

       ;; make sure to select the window if possible, or jumping won't be
       ;; reflected.
       (with-selected-window (or (get-buffer-window buffer)
                                 (get-buffer-window))
         (with-current-buffer buffer
           (mu4e-thread-unfold-all)
           (if (or (mu4e~headers-goto-docid docid)
                   ;; TODO: Is this the best way to find another
                   ;; relevant docid for a view buffer?
                   ;;
                   ;; If you attach a view buffer to another headers
                   ;; buffer that does not contain the current docid
                   ;; then `mu4e~headers-goto-docid' returns nil and we
                   ;; get an error. This "hack" instead gets its
                   ;; now-changed headers buffer's current message as a
                   ;; docid
                   (mu4e~headers-goto-docid
                    (with-current-buffer buffer
                      (mu4e-message-field (mu4e-message-at-point) :docid))))
               ,@body
             (mu4e-error "Cannot find message in headers buffer")))))))

(defun mu4e-view-headers-next (&optional n)
  "Move point to the next message header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth next
header."
  (interactive "P")
  (mu4e--view-in-headers-context
   (mu4e~headers-move (or n 1))))

(defun mu4e-view-headers-prev (&optional n)
  "Move point to the previous message header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth
previous header."
  (interactive "P")
  (mu4e--view-in-headers-context
   (mu4e~headers-move (- (or n 1)))))

(defun mu4e--view-prev-or-next (func backwards)
  "Move point to the next or previous message and invoke FUNC.
Go to the previous message if BACKWARDS is non-nil. If this
succeeds, return the new docid. Otherwise, return nil."
  (mu4e--view-in-headers-context (funcall func backwards))
  (mu4e-select-other-view)
  (mu4e-headers-view-message))

(defun mu4e-view-headers-prev-unread ()
  "Move point to the previous unread message header.
If this succeeds, return the new docid. Otherwise, return nil."
  (interactive)
  (mu4e--view-prev-or-next #'mu4e~headers-prev-or-next-unread t))

(defun mu4e-view-headers-next-unread ()
  "Move point to the next unread message header.
If this succeeds, return the new docid. Otherwise, return nil."
  (interactive)
  (mu4e--view-prev-or-next #'mu4e~headers-prev-or-next-unread nil))

(defun mu4e-view-headers-prev-thread()
  "Move point to the previous thread.
If this succeeds, return the new docid. Otherwise, return nil."
  (interactive)
  (mu4e--view-prev-or-next #'mu4e~headers-prev-or-next-thread t))

(defun mu4e-view-headers-next-thread()
  "Move point to the previous thread.
If this succeeds, return the new docid. Otherwise, return nil."
  (interactive)
  (mu4e--view-prev-or-next #'mu4e~headers-prev-or-next-thread nil))

(defun mu4e-view-thread-goto-root ()
  "Move to thread root."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-thread-goto-root)))

(defun mu4e-view-thread-fold-toggle-goto-next ()
  "Toggle threading or go to next."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-thread-fold-toggle-goto-next)))

(defun mu4e-view-thread-fold-toggle-all ()
  "Toggle all threads."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-thread-fold-toggle-all)))

;;; Interactive functions
(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG, then do it.
If MSG is nil apply action to message returned by
`mu4e-message-at-point'.

 The actions are specified in `mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (actionfunc (mu4e-read-option "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))

(defun mu4e-view-mark-pattern ()
  "Mark messages that match a certain pattern.
Ask user for a kind of mark, (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-headers-mark-pattern)))

(defun mu4e-view-mark-thread (&optional markpair)
  "Mark whole thread with a certain mark.
Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the thread at point in the headers view. The
optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e--view-in-headers-context
   (if markpair (mu4e-headers-mark-thread nil markpair)
     (call-interactively 'mu4e-headers-mark-thread))))

(defun mu4e-view-mark-subthread (&optional markpair)
  "Mark subthread with a certain mark.
Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the subthread at point in the headers view.
The optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e--view-in-headers-context
   (if markpair (mu4e-headers-mark-subthread markpair)
     (mu4e-headers-mark-subthread))))

(defun mu4e-view-search-narrow ()
  "Run `mu4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-search-narrow)))

(defun mu4e-view-search-edit ()
  "Run `mu4e-search-edit' in the headers buffer."
  (interactive)
  (mu4e--view-in-headers-context (mu4e-search-edit)))

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
        (overlay-put ov-inv 'mu4e-overlay t)
        (when (re-search-forward
               (concat "^" message-mark-insert-end) nil t)
          (setq ov-beg (match-beginning 0)
                ov-end (match-end 0)
                ov-inv (make-overlay ov-beg ov-end)
                end    ov-beg)
          (overlay-put ov-inv 'invisible t))
        (when (and beg end)
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'mu4e-overlay t)
            (overlay-put ov 'face 'mu4e-region-code))
          (setq beg nil end nil))))))

;;; View Utilities

(defun mu4e-view-mark-custom ()
  "Run some custom mark function."
  (mu4e--view-in-headers-context
   (mu4e-headers-mark-custom)))

(defun mu4e--view-split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))


(defun mu4e-view-detach ()
  "Detach the view buffer from its headers buffer."
  (interactive)
  (unless mu4e-linked-headers-buffer
    (mu4e-error "This view buffer is already detached"))
  (mu4e-message "Detached view buffer from %s"
                (progn mu4e-linked-headers-buffer
                  (with-current-buffer mu4e-linked-headers-buffer
                    (when (eq (selected-window) mu4e~headers-view-win)
                      (setq mu4e~headers-view-win nil)))
                  (setq mu4e-linked-headers-buffer nil)
                  ;; automatically rename mu4e-view-article buffer when
                  ;; detaching; will get renamed back when reattaching
                  (rename-buffer (make-temp-name (buffer-name)) t))))

(defun mu4e-view-attach (headers-buffer)
  "Attaches a view buffer to HEADERS-BUFFER."
  (interactive
   (list (get-buffer (read-buffer
                      "Select a headers buffer to attach to: " nil t
                      (lambda (buf) (with-current-buffer (car buf)
                                 (mu4e-current-buffer-type-p 'headers)))))))
  (mu4e-message "Attached view buffer to %s" headers-buffer)
  (setq mu4e-linked-headers-buffer headers-buffer)
  (with-current-buffer headers-buffer
    (setq mu4e~headers-view-win (selected-window))))

;;; Scroll commands

(defun mu4e-view-scroll-up-or-next ()
  "Scroll-up the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we cannot scroll up
any further, go the next message."
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
  (if (mu4e--view-split-view-p)
      (mu4e--view-in-headers-context (mu4e-mark-unmark-all))
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e--view-split-view-p)
      (mu4e-view-mark-for-unmark)
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defmacro mu4e--view-defun-mark-for (mark)
  "Define a function mu4e-view-mark-for- MARK."
  (let ((funcname (intern (format "mu4e-view-mark-for-%s" mark)))
        (docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e--view-in-headers-context
               (mu4e-headers-mark-and-next ',mark)))
       (put ',funcname 'definition-name ',mark))))

(mu4e--view-defun-mark-for move)
(mu4e--view-defun-mark-for refile)
(mu4e--view-defun-mark-for delete)
(mu4e--view-defun-mark-for flag)
(mu4e--view-defun-mark-for unflag)
(mu4e--view-defun-mark-for unmark)
(mu4e--view-defun-mark-for something)
(mu4e--view-defun-mark-for read)
(mu4e--view-defun-mark-for unread)
(mu4e--view-defun-mark-for trash)
(mu4e--view-defun-mark-for untrash)

(defun mu4e-view-marked-execute ()
  "Execute the marked actions."
  (interactive)
  (mu4e--view-in-headers-context
   (mu4e-mark-execute-all)))

;;; URL handling

(defvar mu4e--view-link-map nil
  "A map of some number->url so we can jump to url by number.")
(put 'mu4e--view-link-map 'permanent-local t)

(defvar mu4e-view-active-urls-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-2>")  #'mu4e--view-browse-url-from-binding)
    (define-key map (kbd "M-<return>") #'mu4e--view-browse-url-from-binding)
    map)
  "Keymap used for the URLs inside the body.")

(defvar mu4e--view-beginning-of-url-regexp
  "https?\\://\\|mailto:"
  "Regexp that matches the beginning of certain URLs.
Match-string 1 will contain the matched URL, if any.")


(defun mu4e--view-browse-url-from-binding (&optional url)
  "View in browser the url at point, or click location.
If the optional argument URL is provided, browse that instead.
If the url is mailto link, start writing an email to that address."
  (interactive)
  (let* (( url (or url (mu4e--view-get-property-from-event 'mu4e-url))))
    (when url
      (if (string-match-p "^mailto:" url)
          (browse-url-mail url)
        (browse-url url)))))

(defun mu4e--view-get-property-from-event (prop)
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
(defun mu4e--view-activate-urls ()
  "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e--view-link-map ;; buffer local
            (make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e--view-beginning-of-url-regexp nil t)
        (let ((bounds (thing-at-point-bounds-of-url-at-point)))
          (when bounds
            (let* ((url (thing-at-point-url-at-point))
                   (ov (make-overlay (car bounds) (cdr bounds))))
              (puthash (cl-incf num) url mu4e--view-link-map)
              (add-text-properties
               (car bounds)
               (cdr bounds)
               `(face mu4e-link-face
                      mouse-face highlight
                      mu4e-url ,url
                      keymap ,mu4e-view-active-urls-keymap
                      help-echo
                      "[mouse-1] or [M-RET] to open the link"))
              (overlay-put ov 'mu4e-overlay t)
              (overlay-put ov 'after-string
                           (propertize (format "\u200B[%d]" num)
                                       'face 'mu4e-url-number-face)))))))))


(defun mu4e--view-get-urls-num (prompt &optional multi)
  "Ask the user with PROMPT for an URL number for MSG.
The number is [1..n] for URLs \[0..(n-1)] in the message. If
MULTI is nil, return the number for the URL; otherwise (MULTI is
non-nil), accept ranges of URL numbers, as per
`mu4e-split-ranges-to-numbers', and return the corresponding
string."
  (let* ((count (hash-table-count mu4e--view-link-map)) (def))
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
  "Offer to go visit one or more URLs.
If MULTI (prefix-argument) is non-nil, offer to go to a range of URLs."
  (interactive "P")
  (mu4e--view-handle-urls "URL to visit"
                         multi
                         (lambda (url) (mu4e--view-browse-url-from-binding url))))

(defun mu4e-view-save-url (&optional multi)
  "Offer to save URLs to the kill ring.
If MULTI (prefix-argument) is nil, save a single one, otherwise, offer
to save a range of URLs."
  (interactive "P")
  (mu4e--view-handle-urls "URL to save" multi
                         (lambda (url)
                           (kill-new url)
                           (mu4e-message "Saved %s to the kill-ring" url))))

(defun mu4e-view-fetch-url (&optional multi)
  "Offer to fetch (download) URLs.
If MULTI (prefix-argument) is nil,
download a single one, otherwise, offer to fetch a range of
URLs. The urls are fetched to `mu4e-attachment-dir'."
  (interactive "P")
  (mu4e--view-handle-urls
   "URL to fetch" multi
   (lambda (url)
     (let ((target (concat (mu4e-determine-attachment-dir url) "/"
                           (file-name-nondirectory url))))
       (url-copy-file url target)
       (mu4e-message "Fetched %s -> %s" url target)))))

(defun mu4e--view-handle-urls (prompt multi urlfunc)
  "Handle URLs.
If MULTI is nil, apply URLFUNC to a single uri, otherwise, apply
it to a range of uris. PROMPT is the query to present to the user."
  (if multi
      (mu4e--view-handle-multi-urls prompt urlfunc)
    (mu4e--view-handle-single-url prompt urlfunc)))

(defun mu4e--view-handle-single-url (prompt urlfunc &optional num)
  "Apply URLFUNC to some URL with NUM in the current message.
Prompting the user with PROMPT for the number."
  (let* ((num (or num (mu4e--view-get-urls-num prompt)))
         (url (gethash num mu4e--view-link-map)))
    (unless url (mu4e-warn "Invalid number for URL"))
    (funcall urlfunc url)))

(defun mu4e--view-handle-multi-urls (prompt urlfunc)
  "Apply URLFUNC to a a range of URLs in the current message.

Prompting the user with PROMPT for the numbers.

Default is to apply it to all URLs, [1..n], where n is the number
of urls. You can type multiple values separated by space, e.g.  1
3-6 8 will visit urls 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which means all urls, but as
this is the default, you may not need it."
  (let* ((linkstr (mu4e--view-get-urls-num
                   "URL number range (or 'a' for 'all')" t))
         (count (hash-table-count mu4e--view-link-map))
         (linknums (mu4e-split-ranges-to-numbers linkstr count)))
    (dolist (num linknums)
      (mu4e--view-handle-single-url prompt urlfunc num))))

(defun mu4e-view-for-each-uri (func)
  "Evaluate FUNC(uri) for each uri in the current message."
  (maphash (lambda (_num uri) (funcall func uri)) mu4e--view-link-map))

(defun mu4e-view-message-with-message-id (msgid)
  "View message with message-id MSGID.
This (re)creates a
headers-buffer with a search for MSGID, then open a view for that
message."
  (mu4e-search (concat "msgid:" msgid) nil nil t msgid t))

;;; Variables

(defvar gnus-icalendar-additional-identities)
(defvar-local mu4e--view-rendering nil)

(defun mu4e--fake-original-article-buffer ()
  "Create a fake original gnus article buffer.

With this, some Gnus commands that require an original message
buffer can work, e.g. for dealing with mailing-lists. We only
store the headers, not the body, to save some memory, as we don't
need the body.

This must be called while in the raw message buffer."
   (message-narrow-to-headers-or-head)
   (let ((headers (buffer-string)))
     (widen)
     (with-current-buffer
         (get-buffer-create gnus-original-article-buffer 'no-hooks)
       (erase-buffer)
       (insert headers)
       (current-buffer))))

(defun mu4e--original-article-field (field)
  "Get FIELD from the original article."
  (when (bufferp gnus-original-article-buffer)
    (with-current-buffer gnus-original-article-buffer
      (gnus-fetch-field field))))

(defun mu4e-view (msg)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
\"In sync\" here means that moving to the next/previous message
in the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its
`unread' marking if it still had that."
  ;; update headers, if necessary.
  (mu4e~headers-update-handler msg nil nil)
  ;; Create a new view buffer (if needed) as it is not
  ;; feasible to recycle an existing buffer due to buffer-specific
  ;; state (buttons, etc.) that can interfere with message rendering
  ;; in gnus.
  ;;
  ;; Unfortunately that does create its own issues: namely ensuring
  ;; buffer-local state that *must* survive is correctly copied
  ;; across.
  (let ((linked-headers-buffer) (orig-art-buf))
    (when-let* ((existing-buffer (mu4e-get-view-buffer nil nil)))
      ;; required; this state must carry over from the killed buffer
      ;; to the new one.
      (setq linked-headers-buffer mu4e-linked-headers-buffer)
      (if (memq mu4e-split-view '(horizontal vertical))
          (delete-windows-on existing-buffer t))
      (kill-buffer existing-buffer))
    (setq gnus-article-buffer (mu4e-get-view-buffer nil t))
    (with-current-buffer gnus-article-buffer
      (when linked-headers-buffer
        (setq mu4e-linked-headers-buffer linked-headers-buffer))
      (let ((inhibit-read-only t)
            (gnus-unbuttonized-mime-types '(".*/.*"))
            (gnus-buttonized-mime-types
             (append (list "multipart/signed" "multipart/encrypted")
                     gnus-buttonized-mime-types))
            (gnus-inhibit-mime-unbuttonizing t))
        (remove-overlays (point-min)(point-max) 'mu4e-overlay t)
        (erase-buffer)
        (insert-file-contents-literally
         (mu4e-message-readable-path msg) nil nil nil t)
        ;; set up an original-article-buffer, gnus wants it.
        (setq orig-art-buf (mu4e--fake-original-article-buffer))
        ;; some messages have ^M which causes various rendering
        ;; problems later (#2260, #2508), so let's remove those
        (article-remove-cr)
        (setq-local mu4e--view-message msg)
        (ignore-errors
          (mu4e--view-render-buffer msg)))
      (mu4e-loading-mode 0))
    (unless (mu4e--view-detached-p gnus-article-buffer)
      (with-current-buffer mu4e-linked-headers-buffer
        ;; We need this here as we want to avoid displaying the buffer until
        ;; the last possible moment --- after the message is rendered in the
        ;; view buffer.
        ;;
        ;; Otherwise, `mu4e-display-buffer' may adjust the view buffer's
        ;; window height based on a buffer that has no text in it yet!
        (setq-local mu4e~headers-view-win
                    (mu4e-display-buffer gnus-article-buffer nil))
        (unless (window-live-p mu4e~headers-view-win)
          (mu4e-error "Cannot get a message view"))
        (select-window mu4e~headers-view-win)))
    (with-current-buffer gnus-article-buffer
      (let ((inhibit-read-only t))
        (run-hooks 'mu4e-view-rendered-hook))
      ;; support bookmarks.
      (setq-local bookmark-make-record-function
                  #'mu4e--make-bookmark-record
                  gnus-original-article orig-art-buf)
      ;; only needed on some setups; #2683
      (goto-char (point-min)))))

(defun mu4e-view-message-text (msg)
  "Return the rendered MSG as a string."
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-readable-path msg) nil nil nil t)
    (let ((gnus-inhibit-mime-unbuttonizing nil)
          (gnus-unbuttonized-mime-types '(".*/.*"))
          (mu4e-view-fields '(:from :to :cc :subject :date)))
      (mu4e--view-render-buffer msg)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mu4e-action-view-in-browser (msg &optional skip-headers)
  "Show current MSG in browser if it includes an HTML-part.
If SKIP-HEADERS is set, do not show include message headers.
The variables `browse-url-browser-function',
`browse-url-handlers', and `browse-url-default-handlers'
determine which browser function to use."
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-readable-path msg) nil nil nil t)
    ;; just continue if some of the decoding fails.
    (ignore-errors (run-hooks 'gnus-article-decode-hook))
    (let ((header (unless skip-headers
                    (cl-loop for field in '("from" "to" "cc" "date" "subject")
                             when (message-field-value field)
                             concat (format "%s: %s\n" (capitalize field) it))))
          (parts (mm-dissect-buffer t t)))
      ;; If singlepart, enforce a list.
      (when (and (bufferp (car parts))
                 (stringp (car (mm-handle-type parts))))
        (setq parts (list parts)))
      ;; Process the list
      (unless (gnus-article-browse-html-parts parts header)
        (mu4e-warn "Message does not contain a \"text/html\" part"))
      (mm-destroy-parts parts))))

(defun mu4e-action-view-in-xwidget (msg)
  "Show current MSG in an embedded xwidget, if available."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let ((browse-url-handlers nil)
        (browse-url-browser-function
         (lambda (url &optional _rest)
           (xwidget-webkit-browse-url url))))
    (mu4e-action-view-in-browser msg)))

(defun mu4e--view-render-buffer (msg)
  "Render current buffer with MSG using Gnus' article mode."
  (setq-local gnus-summary-buffer (get-buffer-create " *appease-gnus*"))
  (let* ((inhibit-read-only t)
         (max-specpdl-size mu4e-view-max-specpdl-size)
         (mm-decrypt-option 'known)
         (ct (mail-fetch-field "Content-Type"))
         (ct (and ct (mail-header-parse-content-type ct)))
         (charset (mail-content-type-get ct 'charset))
         (charset (and charset (intern charset)))
         (mu4e--view-rendering t); Needed if e.g. an ics file is buttonized
         (gnus-article-emulate-mime nil) ;; avoid perf problems
         (gnus-newsgroup-charset
          (if (and charset (coding-system-p charset)) charset
            (detect-coding-region (point-min) (point-max) t)))
         ;; Possibly add headers (before "Attachments")
         (gnus-display-mime-function (mu4e--view-gnus-display-mime msg)))
    (condition-case err
        (progn
          (mm-enable-multibyte)
          ;; just continue if some of the decoding fails.
          (ignore-errors (run-hooks 'gnus-article-decode-hook))
          (gnus-article-prepare-display)
          (mu4e--view-activate-urls)
          ;; `gnus-summary-bookmark-make-record' does not work properly when "appeased."
          (kill-local-variable 'bookmark-make-record-function)
          (setq mu4e~gnus-article-mime-handles gnus-article-mime-handles
                gnus-article-decoded-p gnus-article-decode-hook)
          (set-buffer-modified-p nil)
          (add-hook 'kill-buffer-hook #'mu4e--view-kill-mime-handles))
      (epg-error
       (mu4e-message "EPG error: %s; fall back to raw view"
                     (error-message-string err))))))

(defun mu4e-view-refresh ()
  "Refresh the message view."
  ;;; XXX: sometimes, side-effect: increase the header-buffers size
  (interactive)
  (when-let* ((msg (and (derived-mode-p 'mu4e-view-mode)
                       mu4e--view-message)))
    (mu4e-view-quit)
    (mu4e-view msg)))

(defun mu4e-view-show-mime-parts()
  "Show all MIME-parts.

This can be useful for messages with embedded images etc. that
you want to save, and that are not accessible otherwise. However,
note that Emacs can get slow with big attached images.

To go back to normal display, quit the message and re-open."
  (interactive)
  (let* ((toggle (not gnus-mime-display-multipart-as-mixed))
         (gnus-inhibit-mime-unbuttonizing (not toggle))
         (gnus-mime-display-multipart-as-mixed toggle))
    (mu4e-view-refresh)))

(defun mu4e-view-toggle-fill-flowed()
  "Toggle flowed-message text filling."
  (interactive)
  (setq mm-fill-flowed (not mm-fill-flowed))
  (mu4e-view-refresh))

(defun mu4e-view-toggle-emulate-mime()
  "Toggle GNUs MIME-emulation.
Note that for some messages, this can trigger high CPU load."
  (interactive)
  (setq gnus-article-emulate-mime (not gnus-article-emulate-mime))
  (mu4e-view-refresh))

(defun mu4e--view-gnus-display-mime (msg)
  "Like `gnus-display-mime', but include mu4e headers to MSG."
  (lambda (&optional ihandles)
    (gnus-display-mime ihandles)
    (unless ihandles
      (save-restriction
        (article-goto-body)
        (forward-line -1)
        (narrow-to-region (point) (point))
        (dolist (field mu4e-view-fields)
          (let ((fieldval (mu4e-message-field msg field)))
            (pcase field
              ((or ':path ':maildir ':list)
               (mu4e--view-gnus-insert-header field fieldval))
              (':message-id
               (when-let* ((msgid (plist-get msg :message-id)))
                 (mu4e--view-gnus-insert-header field (format "<%s>" msgid))))
              (':mailing-list
               (let ((list (plist-get msg :list)))
                 (if list (mu4e-get-mailing-list-shortname list) "")))
              ((or ':flags ':tags)
               (let ((flags (mapconcat (lambda (flag)
                                         (if (symbolp flag)
                                             (symbol-name flag)
                                           flag)) fieldval ", ")))
                 (mu4e--view-gnus-insert-header field flags)))
              (':size (mu4e--view-gnus-insert-header
                       field (mu4e-display-size fieldval)))
              ((or ':subject ':to ':from ':cc ':bcc ':from-or-to
                   ':user-agent ':date ':attachments
                   ':signature ':decryption)) ;; handled by Gnus
              (_
               (mu4e--view-gnus-insert-header-custom msg field)))))
        (let ((gnus-treatment-function-alist
               '((gnus-treat-highlight-headers
                  gnus-article-highlight-headers))))
          (gnus-treat-article 'head))))))

(defun mu4e--view-gnus-insert-header (field val)
  "Insert a header FIELD with value VAL."
  (let* ((info (cdr (assoc field mu4e-header-info)))
         (key (plist-get info :name))
         (help (plist-get info :help)))
    (if (and val (> (length val) 0))
        (insert (propertize (concat key ":") 'help-echo help)
                " " val "\n"))))

(defun mu4e--view-gnus-insert-header-custom (msg field)
  "Insert MSG's custom FIELD."
  (let* ((info (cdr-safe (or (assoc field mu4e-header-info-custom)
                             (mu4e-error "Custom field %S not found" field))))
         (key (plist-get info :name))
         (func (or (plist-get info :function)
                   (mu4e-error "No :function defined for custom field %S %S"
                               field info)))
         (val (funcall func msg))
         (help (plist-get info :help)))
    (when (and val (> (length val) 0))
      (insert (propertize (concat key ":") 'help-echo help) " " val "\n"))))

(define-advice gnus-icalendar-event-from-handle
    (:filter-args (handle-attendee) mu4e--view-fix-missing-charset)
  "Avoid error when displaying an ical attachment without a charset."
  (if (and (boundp 'mu4e--view-rendering) mu4e--view-rendering)
      (let* ((handle (car handle-attendee))
             (attendee (cadr handle-attendee))
             (buf (mm-handle-buffer handle))
             (ty (mm-handle-type handle))
             (rest (cddr handle)))
        ;; Put the fallback at the end:
        (setq ty (append ty '((charset . "utf-8"))))
        (setq handle (cons buf (cons ty rest)))
        (list handle attendee))
  handle-attendee))

(defun mu4e-view-jump-to-mime-part (number)
  "Jump to MIME-part with NUMBER."
  (interactive "P")
  (call-interactively #'gnus-article-jump-to-part number))

(defun mu4e--view-mode-p ()
  "Is the buffer in mu4e-view-mode or one of its descendants?"
  (or (eq major-mode 'mu4e-view-mode)
      (derived-mode-p '(mu4e-view-mode))))

(defun mu4e--view-nop (func &rest args)
  "Do not invoke FUNC with ARGS when in mu4e-view-mode.
This is useful for advising some Gnus-functionality that does not work in mu4e."
  (unless (mu4e--view-mode-p)
    (apply func args)))

(defun mu4e--view-button-reply (func &rest args)
  "Advise FUNC with ARGS to make `gnus-button-reply' links work in mu4e."
  (if (mu4e--view-mode-p)
      (mu4e-compose-reply)
    (apply func args)))

(defun mu4e--view-button-message-id (func &rest args)
  "Advise FUNC with ARGS to make `gnus-button-message-id' links work in mu4e."
  (if (and (mu4e--view-mode-p) (stringp (car-safe args)))
      (mu4e-view-message-with-message-id (car args))
    (apply func args)))

(defun mu4e--view-msg-mail (func &rest args)
  "Advise FUNC with ARGS  to make `gnus-msg-mail' links compose with mu4e."
  (if (mu4e--view-mode-p)
      (apply 'mu4e-compose-mail args)
    (apply func args)))

(defun mu4e-view-quit ()
  "Quit the mu4e-view buffer."
  (interactive)
  (if (memq mu4e-split-view '(horizontal vertical))
      (ignore-errors ;; try, don't error out.
        (kill-buffer-and-window))
    ;; single-window case
    (let ((docid (mu4e-field-at-point :docid)))
      (when mu4e-linked-headers-buffer ;; re-use mu4e-view-detach?
        (with-current-buffer mu4e-linked-headers-buffer
          (when (eq (selected-window) mu4e~headers-view-win)
            (setq mu4e~headers-view-win nil)))
        (setq mu4e-linked-headers-buffer nil)
        (kill-buffer)
        ;; attempt to move point to just-viewed message.
        (when docid
          (ignore-errors
            (mu4e~headers-goto-docid docid)))))))

(defvar mu4e-view-mode-map
  (let ((map (make-keymap)))
    (define-key map  (kbd "C-S-u") #'mu4e-update-mail-and-index)
    (define-key map  (kbd "C-c C-u") #'mu4e-update-mail-and-index)

    (define-key map "q" #'mu4e-view-quit)

    (define-key map "z" #'mu4e-view-detach)
    (define-key map "Z" #'mu4e-view-attach)

    (define-key map "%" #'mu4e-view-mark-pattern)
    (define-key map "t" #'mu4e-view-mark-subthread)
    (define-key map "T" #'mu4e-view-mark-thread)

    (define-key map "g" #'mu4e-view-go-to-url)
    (define-key map "k" #'mu4e-view-save-url)
    (define-key map "f" #'mu4e-view-fetch-url)

    (define-key map "." #'mu4e-view-raw-message)
    (define-key map "," #'mu4e-sexp-at-point)
    (define-key map "|" #'mu4e-view-pipe)
    (define-key map "a" #'mu4e-view-action)
    (define-key map "A" #'mu4e-view-mime-part-action)
    (define-key map "e" #'mu4e-view-save-attachments)
    (define-key map "J" #'mu4e-view-jump-to-mime-part)

    ;; change the number of headers
    (define-key map (kbd "C-+") #'mu4e-headers-split-view-grow)
    (define-key map (kbd "C--") #'mu4e-headers-split-view-shrink)
    (define-key map (kbd "<C-kp-add>") #'mu4e-headers-split-view-grow)
    (define-key map (kbd "<C-kp-subtract>") #'mu4e-headers-split-view-shrink)

    ;; intra-message navigation
    (define-key map (kbd "S-SPC") #'scroll-down)
    (define-key map (kbd "SPC") #'mu4e-view-scroll-up-or-next)
    (define-key map (kbd "RET")  #'mu4e-scroll-up)
    (define-key map (kbd "<backspace>") #'mu4e-scroll-down)

    ;; navigation between messages
    (define-key map "p" #'mu4e-view-headers-prev)
    (define-key map "n" #'mu4e-view-headers-next)
    ;; the same
    (define-key map (kbd "<M-down>") #'mu4e-view-headers-next)
    (define-key map (kbd "<M-up>") #'mu4e-view-headers-prev)

    (define-key map (kbd "[") #'mu4e-view-headers-prev-unread)
    (define-key map (kbd "]") #'mu4e-view-headers-next-unread)
    (define-key map (kbd "{") #'mu4e-view-headers-prev-thread)
    (define-key map (kbd "}") #'mu4e-view-headers-next-thread)

    ;; ;; threads
    ;; TODO: find some binding that don't conflict
    ;; (define-key map (kbd "<S-left>")  #'mu4e-view-thread-goto-root)
    ;; ;; <tab> is taken already
    ;; (define-key map (kbd "<C-S-tab>")   #'mu4e-view-thread-fold-toggle-goto-next)
    ;; (define-key map (kbd "<backtab>") #'mu4e-view-thread-fold-toggle-all)


    ;; switching from view <-> headers (when visible)
    (define-key map "y" #'mu4e-select-other-view)

    ;; marking/unmarking
    (define-key map "d" #'mu4e-view-mark-for-trash)
    (define-key map (kbd "<delete>") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "<deletechar>") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "D") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "m") #'mu4e-view-mark-for-move)
    (define-key map (kbd "r") #'mu4e-view-mark-for-refile)

    (define-key map (kbd "?") #'mu4e-view-mark-for-unread)
    (define-key map (kbd "!") #'mu4e-view-mark-for-read)

    (define-key map (kbd "+") #'mu4e-view-mark-for-flag)
    (define-key map (kbd "-") #'mu4e-view-mark-for-unflag)
    (define-key map (kbd "=") #'mu4e-view-mark-for-untrash)
    (define-key map (kbd "&") #'mu4e-view-mark-custom)

    (define-key map (kbd "*")             #'mu4e-view-mark-for-something)
    (define-key map (kbd "<kp-multiply>") #'mu4e-view-mark-for-something)
    (define-key map (kbd "<insert>")     #'mu4e-view-mark-for-something)
    (define-key map (kbd "<insertchar>") #'mu4e-view-mark-for-something)

    (define-key map ";" #'mu4e-context-switch)

    (define-key map (kbd "#") #'mu4e-mark-resolve-deferred-marks)
    ;; misc
    (define-key map "M" #'mu4e-view-massage)

    (define-key map "w" #'visual-line-mode)
    (define-key map "h" #'mu4e-view-toggle-html)
    (define-key map (kbd "M-q") #'article-fill-long-lines)

    (define-key map "c" #'mu4e-copy-thing-at-point)

    ;; next 3 only warn user when attempt in the message view
    (define-key map "u" #'mu4e-view-unmark)
    (define-key map "U" #'mu4e-view-unmark-all)
    (define-key map "x" #'mu4e-view-marked-execute)

    (define-key map "$" #'mu4e-show-log)
    (define-key map "H" #'mu4e-display-manual)

    ;; Make 0..9 shortcuts for digit-argument.  Actually, none of the bound
    ;; functions seem to use a prefix arg but those bindings existed because we
    ;; used to use `suppress-keymap'.  And possibly users added their own
    ;; prefix arg consuming commands.
    (dotimes (i 10)
      (define-key map (kbd (format "%d" i)) #'digit-argument))

    (set-keymap-parent map special-mode-map)
    (set-keymap-parent map button-buffer-map)
    map)
  "Keymap for mu4e-view mode.")

(easy-menu-define mu4e-view-mode-menu
  mu4e-view-mode-map "Menu for mu4e's view mode."
  (append
   '("View"
     "--"
     ["Toggle wrap lines" visual-line-mode]
     ["View raw" mu4e-view-raw-message]
     ["Pipe through shell" mu4e-view-pipe]
     "--"
     ["Mark for deletion" mu4e-view-mark-for-delete]
     ["Mark for untrash" mu4e-view-mark-for-untrash]
     ["Mark for trash"   mu4e-view-mark-for-trash]
     ["Mark for move"   mu4e-view-mark-for-move]
     )
   mu4e--compose-menu-items
   mu4e--search-menu-items
   mu4e--context-menu-items
   '(
     "--"
     ["Quit" mu4e-view-quit
      :help "Quit the view"]
     )))

(defcustom mu4e-raw-view-mode-hook nil
  "Hook run when entering \\[mu4e-raw-view] mode."
  :options '()
  :type 'hook
  :group 'mu4e-view)

(defcustom mu4e-view-mode-hook nil
  "Hook run when entering \\[mu4e-view] mode."
  :options '(turn-on-visual-line-mode)
  :type 'hook
  :group 'mu4e-view)

(defcustom mu4e-view-rendered-hook '(mu4e-resize-linked-headers-window)
  "Hook run by `mu4e-view' after a message is rendered."
  :type 'hook
  :group 'mu4e-view)

(define-derived-mode mu4e-raw-view-mode fundamental-mode "mu4e:raw-view"
  (view-mode))

;;  "Define the major-mode for the mu4e-view."
(define-derived-mode mu4e-view-mode gnus-article-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e.
Based on Gnus' article-mode."
  ;; some external tools (bbdb) depend on this
  (setq gnus-article-buffer (current-buffer))

  ;; ;; turn off gnus modeline changes and menu items
  (advice-add 'gnus-set-mode-line :around #'mu4e--view-nop)
  (advice-add 'gnus-button-reply :around #'mu4e--view-button-reply)
  (advice-add 'gnus-button-message-id :around #'mu4e--view-button-message-id)
  (advice-add 'gnus-msg-mail :around #'mu4e--view-msg-mail)

  ;; advice gnus-block-private-groups to always return "."
  ;; so that by default we block images.
  (advice-add 'gnus-block-private-groups :around
              (lambda(func &rest args)
                (if (mu4e--view-mode-p)
                    "." (apply func args))))
  (use-local-map mu4e-view-mode-map)
  (mu4e-context-minor-mode)
  (mu4e-search-minor-mode)
  (mu4e-compose-minor-mode)
  (setq buffer-undo-list t) ;; don't record undo info

  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t)))

;;; Massaging the message view

(defcustom mu4e-view-massage-options
  '( ("ctoggle citations"           . gnus-article-hide-citation)
     ("htoggle headers"             . gnus-article-hide-headers)
     ("ytoggle crypto"              . gnus-article-hide-pem)
     ("ftoggle fill-flowed"         . mu4e-view-toggle-fill-flowed)
     ("mtoggle show all MIME parts" . mu4e-view-toggle-show-mime-parts)
     ("Mtoggle show emulate MIME"   . mu4e-view-toggle-emulate-mime))
"Various options for \"massaging\" the message view. See `(gnus)
Article Treatment' for more options."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))

(defun mu4e-view-massage()
  "Massage current message view as per `mu4e-view-massage-options'."
  (interactive)
  (funcall (mu4e-read-option "Massage: " mu4e-view-massage-options)))

(defun mu4e-view-toggle-html ()
  "Toggle html-display of the first html-part found."
  (interactive)
  ;; This function assumes `gnus-article-mime-handle-alist' is sorted by
  ;; pertinence, i.e. the first HTML part found in it is the most important one.
  (save-excursion
    (if-let*((html-part
              (seq-find (lambda (handle)
                          (equal (mm-handle-media-type (cdr handle))
                                 "text/html"))
                        gnus-article-mime-handle-alist))
             (text-part
              (seq-find (lambda (handle)
                          (equal (mm-handle-media-type (cdr handle))
                                 "text/plain"))
                        gnus-article-mime-handle-alist)))
        (gnus-article-inline-part (car html-part))
      (mu4e-warn "Cannot switch; no html and/or text part in this message"))))
;;; Bug Reference mode support

;; Due to mu4e's view buffer handling (mu4e-view-mode is called long before the
;; actual mail text is inserted into the buffer), one should activate
;; bug-reference-mode in mu4e-after-view-message-hook, not mu4e-view-mode-hook.

;; This is Emacs 28 stuff but there is no need to guard it with some (f)boundp
;; checks (which would return nil if bug-reference.el is not loaded before
;; mu4e) since the function definition doesn't hurt and `add-hook' works fine
;; for not yet defined variables (by creating them).
(declare-function bug-reference-maybe-setup-from-mail "ext:bug-reference")

(defvar mu4e--view-bug-reference-checked-headers
  '("list" "list-id" "to" "from" "cc" "subject" "reply-to")
  "List of mail headers whose values are passed to bug-reference's auto-setup.")

(defun mu4e--view-try-setup-bug-reference-mode ()
  "Try to guess bug-reference setup from the current mu4e mail.
Looks at the maildir and the mail headers in
`mu4e--view-bug-reference-checked-headers' and tries to guess suitable
values for `bug-reference-bug-regexp' and
`bug-reference-url-format' by matching the maildir name against
GROUP-REGEXP and each header value against HEADER-REGEXP in
`bug-reference-setup-from-mail-alist'."
  (when (derived-mode-p 'mu4e-view-mode)
    (let (header-values)
      (save-excursion
        (goto-char (point-min))
        (dolist (field mu4e--view-bug-reference-checked-headers)
          (let ((val (mail-fetch-field field)))
            (when val
              (push val header-values)))))
      (bug-reference-maybe-setup-from-mail
       (mail-fetch-field "maildir")
       header-values))))

(with-eval-after-load 'bug-reference
  (add-hook 'bug-reference-auto-setup-functions
            #'mu4e--view-try-setup-bug-reference-mode))

(provide 'mu4e-view)
;;; mu4e-view.el ends here
