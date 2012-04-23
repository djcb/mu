;;; mu4e-view.el -- part of mu4e, the mu mail user agent
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

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:
(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-proc)

;; we prefer the improved fill-region
(require 'filladapt nil 'noerror)
(require 'comint)

;; some buffer-local variables
(defvar mu4e~view-hdrs-buffer nil
  "*internal* Headers buffer connected to this view.")

(defvar mu4e~view-lines-wrapped nil "*internal* Whether lines are wrapped.")
(defvar mu4e~view-cited-hidden nil "*internal* Whether cited lines are hidden.")

(defun mu4e-view-message-with-msgid (msgid)
  "View message with MSGID. This is meant for external programs
wanting to show specific messages - for example, `mu4e-org'."
  (mu4e~proc-view msgid))

(defun mu4e-view-message-text (msg)
  "Return the message to display (as a string), based on the MSG
plist."
  (concat
    (mapconcat
      (lambda (field)
	(let ((fieldname (cdr (assoc field mu4e-header-names)))
	       (fieldval (plist-get msg field)))
	  (case field
	    (:subject  (mu4e~view-construct-header fieldname fieldval))
	    (:path	   (mu4e~view-construct-header fieldname fieldval))
	    (:maildir  (mu4e~view-construct-header fieldname fieldval))
	    (:flags	   (mu4e~view-construct-header fieldname
			     (if fieldval (format "%S" fieldval) "")))
	    ;; contact fields
	    (:to	   (mu4e~view-construct-contacts msg field))
	    (:from	   (mu4e~view-construct-contacts msg field))
	    (:cc	   (mu4e~view-construct-contacts msg field))
	    (:bcc	   (mu4e~view-construct-contacts msg field))

	    ;; if we (`user-mail-address' are the From, show To, otherwise,
	    ;; show From
	    (:from-or-to
	      (let* ((from (plist-get msg :from))
		      (from (and from (cdar from))))
		(if (and from (string-match mu4e-user-mail-address-regexp from))
		  (mu4e~view-construct-contacts msg :to)
		  (mu4e~view-construct-contacts msg :from))))
	    ;; date
	    (:date
	      (let ((datestr
		      (when fieldval (format-time-string mu4e-view-date-format
				       fieldval))))
		(if datestr (mu4e~view-construct-header fieldname datestr) "")))
	    ;; size
	    (:size
	      (let* (size (mu4e-view-size msg)
		      (sizestr (when size (format "%d bytes" size))))
		(if sizestr (mu4e~view-construct-header fieldname sizestr))))
	    ;; attachments
	    (:attachments (mu4e~view-construct-attachments msg))
	    (t               (error "Unsupported field: %S" field)))))
      mu4e-view-fields "")
    "\n"
    (mu4e-body-text msg)))


(defun mu4e-view (msg hdrsbuf &optional update)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc. If
UPDATE is nil, the current message may be (visually) 'massaged',
based on the settings of `mu4e~view-wrap-lines' and
`mu4e~view-hide-cited'.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mu4e-view-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (setq mu4e-view-buffer buf)
      (erase-buffer)
      (insert (mu4e-view-message-text msg))

      ;; initialize view-mode
      (mu4e-view-mode)
      (setq ;; these are buffer-local
	buffer-read-only t
	mu4e-current-msg msg
	mu4e~view-hdrs-buffer hdrsbuf
	mu4e~view-link-map (make-hash-table :size 32 :rehash-size 2 :weakness nil))

      (switch-to-buffer buf)
      (goto-char (point-min))

      (mu4e~view-fontify-cited)
      (mu4e~view-fontify-footer)
      (mu4e~view-make-urls-clickable)

      (unless update
	;; if we're showing the message for the first time, use the values of
	;; user-settable variables `mu4e~view-wrap-lines' and
	;; `mu4e~view-hide-cited' to determine whether we should wrap/hide
	(progn
	  (when mu4e~view-lines-wrapped (mu4e~view-wrap-lines))
	  (when mu4e~view-cited-hidden  (mu4e~view-hide-cited))))

      ;; no use in trying to set flags again
      (unless update
	(mu4e~view-mark-as-read-maybe)))))


(defun mu4e~view-construct-header (key val &optional dont-propertize-val)
  "Return header KEY with value VAL if VAL is non-nil. If
DONT-PROPERTIZE-VAL is non-nil, do not add text-properties to VAL."
  (if val
    (with-temp-buffer
      (insert (propertize key 'face 'mu4e-view-header-key-face) ": "
	(if dont-propertize-val
	  val
	  (propertize val 'face 'mu4e-view-header-value-face)) "\n")
      ;; temporarily set the fill column <margin> positions to the right, so
      ;; we can indent following lines with positions
      (let*((margin 1) (fill-column (- fill-column margin)))
	(fill-region (point-min) (point-max))
	(goto-char (point-min))
	(while (and (zerop (forward-line 1)) (not (looking-at "^$")))
	  (indent-to-column margin)))
      (buffer-string))
    ""))


(defun mu4e~view-construct-contacts (msg field)
  "Add a header for a contact field (ie., :to, :from, :cc, :bcc)."
  (let* ((lst (plist-get msg field))
	  (fieldname (cdr (assoc field mu4e-header-names)))
	  (contacts
	    (and lst
	      (mapconcat
		(lambda(c)
		  (let ((name (car c)) (email (cdr c)))
		    (propertize
		      (if name
			(if mu4e-view-show-addresses
			  (format "%s <%s>" name email)
			  (format "%s" name))
			(format "%s" email))
		      'help-echo email)))
		lst ", "))))
    (if contacts
      (mu4e~view-construct-header fieldname contacts)
      "")))


(defun mu4e~view-open-save-attach-func (msg attachnum is-open)
  "Return a function that offers to save attachment NUM. If IS-OPEN
is nil, and otherwise open it."
  (lexical-let ((msg msg) (attachnum attachnum) (is-open is-open))
    (lambda ()
      (interactive)
      (if is-open
	(mu4e-view-open-attachment msg attachnum)
	(mu4e-view-save-attachment msg attachnum)))))

;; note -- attachments have an index which is needed for the backend, which does
;; not necessarily follow 1,2,3,4 etc.
(defun mu4e~view-construct-attachments (msg)
  "Display attachment information; the field looks like something like:
   	:attachments ((:index 4 :name \"test123.doc\"
                       :mime-type \"application/msword\" :size 1234))."
  (let* ((id 0)
	  (attstr
	   (mapconcat
	     (lambda (att)
	       (let ( (index (plist-get att :index))
		      (name (plist-get att :name))
		      (size (plist-get att :size))
		      (map (make-sparse-keymap)))
		 (incf id)
		 (define-key map [mouse-2] (mu4e~view-open-save-attach-func msg id nil))
		 (define-key map [?\r]     (mu4e~view-open-save-attach-func msg id nil))
		 (define-key map [S-mouse-2](mu4e~view-open-save-attach-func msg id t))
		 (define-key map (kbd "<S-return>")
		   (mu4e~view-open-save-attach-func msg id t))
		 (concat
		   (propertize (format "[%d]" id) 'face 'mu4e-view-attach-number-face)
		   (propertize name 'face 'mu4e-view-link-face
		     'keymap map 'mouse-face 'highlight)
		   (when (and size (> size 0))
		     (concat (format "(%s)"
			       (propertize (mu4e-display-size size)
				 'face 'mu4e-header-key-face)))))))
	     (plist-get msg :attachments) ", ")))
    (unless (zerop id)
      (mu4e~view-construct-header (format "Attachments(%d)" id) attstr t))))


(defvar mu4e-view-mode-map nil
  "Keymap for \"*mu4e-view*\" buffers.")
(unless mu4e-view-mode-map

  (setq mu4e-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mu4e-view-kill-buffer-and-window)

      ;; note, 'z' is by-default bound to 'bury-buffer'
      ;; but that's not very useful in this case
      (define-key map "z" 'mu4e-view-kill-buffer-and-window)

      (define-key map "s" 'mu4e-search)

      (define-key map "b" 'mu4e-search-bookmark)
      (define-key map "B" 'mu4e-search-bookmark-edit-first)

      (define-key map "j" 'mu4e-jump-to-maildir)

      (define-key map "g" 'mu4e-view-go-to-url)

      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-compose-edit)

      (define-key map "." 'mu4e-view-raw-message)
      (define-key map "|" 'mu4e-view-pipe)
      (define-key map "a" 'mu4e-view-action)

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'scroll-up)
      (define-key map (kbd "<home>")
	#'(lambda () (interactive) (goto-char (point-min))))
      (define-key map (kbd "<end>")
	#'(lambda () (interactive) (goto-char (point-max))))
      (define-key map (kbd "RET")
	#'(lambda () (interactive) (scroll-up 1)))
      (define-key map (kbd "<backspace>")
	#'(lambda () (interactive) (scroll-up -1)))

      ;; navigation between messages
      (define-key map "p" 'mu4e~view-prev-header)
      (define-key map "n" 'mu4e~view-next-header)
      ;; the same
      (define-key map (kbd "<M-down>") 'mu4e~view-next-header)
      (define-key map (kbd "<M-up>") 'mu4e~view-prev-header)

      ;; switching to view mode (if it's visible)
      (define-key map "y" 'mu4e-select-other-view)

      ;; attachments
      (define-key map "e" 'mu4e-view-save-attachment)
      (define-key map "o" 'mu4e-view-open-attachment)
      (define-key map "A" 'mu4e-view-attachment-action)

      ;; marking/unmarking
      (define-key map (kbd "<backspace>") 'mu4e-mark-for-trash)
      (define-key map "d" 'mu4e-view-mark-for-trash)

      (define-key map (kbd "<delete>") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "<deletechar>") 'mu4e-mark-for-delete)
      (define-key map "D" 'mu4e-view-mark-for-delete)
      (define-key map "m" 'mu4e-view-mark-for-move)

      ;; misc
      (define-key map "w" 'mu4e-view-toggle-wrap-lines)
      (define-key map "h" 'mu4e-view-toggle-hide-cited)

      (define-key map "r" 'mu4e-view-refresh)

      ;; next 3 only warn user when attempt in the message view
      (define-key map "u" 'mu4e-view-unmark)
      (define-key map "U" 'mu4e-view-unmark-all)
      (define-key map "x" 'mu4e-view-marked-execute)

      (define-key map "$" 'mu4e-show-log)
      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "View")))
	(define-key map [menu-bar headers] (cons "View" menumap))

	(define-key menumap [quit-buffer]
	  '("Quit view" . mu4e-view-kill-buffer-and-window))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . mu4e-view-toggle-wrap-lines))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . mu4e-view-toggle-hide-cited))
	(define-key menumap [raw-view]
	  '("View raw message" . mu4e-view-raw-message))
	(define-key menumap [pipe]
	  '("Pipe through shell" . mu4e-view-pipe))
	;; (define-key menumap [inspect]
	;;   '("Inspect with guile" . mu4e-inspect-message))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . mu4e-view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . mu4e-view-save-attachment))
	(define-key menumap [goto-url]
	  '("Visit URL" . mu4e-view-go-to-url))

	(define-key menumap [sepa1] '("--"))
	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mu4e-view-mark-for-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mu4e-view-mark-for-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mu4e-view-mark-for-move))

	(define-key menumap [sepa2] '("--"))
	(define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
	(define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
	(define-key menumap [sepa3] '("--"))

	(define-key menumap [search]  '("Search" . mu4e-search))
	(define-key menumap [jump]  '("Jump to maildir" . mu4e-jump-to-maildir))

	(define-key menumap [sepa4] '("--"))
	(define-key menumap [next]  '("Next" . mu4e~view-next-header))
	(define-key menumap [previous]  '("Previous" . mu4e~view-prev-header)))
      map)))

(fset 'mu4e-view-mode-map mu4e-view-mode-map)


(define-derived-mode mu4e-view-mode special-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e.
\\{mu4e-view-mode-map}."
  (use-local-map mu4e-view-mode-map)

  (make-local-variable 'mu4e~view-hdrs-buffer)
  (make-local-variable 'mu4e-current-msg)
  (make-local-variable 'mu4e~view-link-map)

  (make-local-variable 'mu4e~view-lines-wrapped)
  (make-local-variable 'mu4e~view-cited-hidden)

  (setq buffer-undo-list t) ;; don't record undo info

  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t))

  ;; filladapt is much better than the built-in filling
  ;; esp. with '>' cited parts
  (when (fboundp 'filladapt-mode)
    (filladapt-mode))
  (setq truncate-lines t))


;; we mark messages are as read when we leave the message; i.e., when skipping
;; to the next/previous one, or leaving the view buffer altogether.

(defun mu4e~view-mark-as-read-maybe ()
  "Clear the current message's New/Unread status and set it to
Seen; if the message is not New/Unread, do nothing."
  (when mu4e-current-msg
    (let ((flags (plist-get mu4e-current-msg :flags))
	   (docid (plist-get mu4e-current-msg :docid)))
      ;; is it a new message?
      (when (or (member 'unread flags) (member 'new flags))
	(mu4e~proc-move docid nil "+S-u-N")))))

(defun mu4e~view-fontify-cited ()
  "Colorize message content based on the citation level."
  (save-excursion
    (let ((more-lines t))
      (goto-char (point-min))
      (while more-lines
	;; Get the citation level at point -- i.e., the number of '>'
	;; prefixes, starting with 0 for 'no citation'
	(beginning-of-line 1)
	(let* ((text (re-search-forward "[[:word:]]" (line-end-position 1) t 1))
		(level (or (and text
			     (how-many ">" (line-beginning-position 1) text)) 0))
		(face
		  (cond
		    ((= 0 level) nil) ;; don't do anything
		    ((= 1 level) 'mu4e-cited-1-face)
		    ((= 2 level) 'mu4e-cited-2-face)
		    ((= 3 level) 'mu4e-cited-3-face)
		    ((= 4 level) 'mu4e-cited-4-face)
		    (t           nil))))
	  (when face
	    (add-text-properties (line-beginning-position 1)
	      (line-end-position 1) `(face ,face))))
	(setq more-lines
	  (and (= 0 (forward-line 1))
	    ;; we need to add this weird check below; it seems in some cases
	    ;; `forward-line' continues to return 0, even when at the end, which
	    ;; would lead to an infinite loop
	    (not (= (point-max) (line-end-position)))))))))

(defun mu4e~view-fontify-footer ()
  "Give the message footers a distinctive color."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (let ((p (search-forward "\n-- \n" nil t)))
	(when p
	  (add-text-properties p (point-max) '(face mu4e-footer-face)))))))

(defvar mu4e~view-link-map nil
  "*internal* A map of some number->url so we can jump to url by number.")

(defconst mu4e~view-url-regexp
  "\\(https?://[-+a-zA-Z0-9.?_$%/+&#@!~,:;=/()]+\\)"
  "*internal* regexp that matches URLs; match-string 1 will contain
  the matched URL, if any.")

(defun mu4e~view-browse-url-func (url)
  "Return a function that executes `browse-url' with URL."
  (lexical-let ((url url))
    (lambda ()
      (interactive)
      (browse-url url))))


;; this is fairly simplistic...
(defun mu4e~view-make-urls-clickable ()
  "Turn things that look like URLs into clickable things, and
number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mu4e~view-url-regexp nil t)
	(let ((url (match-string 0))
	       (map (make-sparse-keymap)))
	  (define-key map [mouse-2] (mu4e~view-browse-url-func url))
	  (define-key map [?\r] (mu4e~view-browse-url-func url))
	  (puthash (incf num) url mu4e~view-link-map)
	  (add-text-properties 0 (length url)
	    `(face mu4e-view-link-face
	       mouse-face highlight
	       keymap ,map) url)
	  (replace-match (concat url
			   (propertize (format "[%d]" num)
			     'face 'mu4e-view-url-number-face))))))))



(defun mu4e~view-wrap-lines ()
  "Wrap lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (search-forward "\n\n") ;; search for the message body
	(fill-region (point) (point-max)))
      (setq mu4e~view-lines-wrapped t))))

(defun mu4e~view-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (flush-lines "^[:blank:]*>")
      (setq mu4e~view-cited-hidden t))))


(defun mu4e~view-hdrs-move (lines)
  "Move point LINES lines forward (if LINES is positive) or
backward (if LINES is negative). If this succeeds, return the new
docid. Otherwise, return nil."
  (when (buffer-live-p mu4e~view-hdrs-buffer)
    (with-current-buffer mu4e~view-hdrs-buffer
      (mu4e~hdrs-move lines))))

(defun mu4e~view-next-header()(interactive)(mu4e~view-hdrs-move 1))
(defun mu4e~view-prev-header()(interactive)(mu4e~view-hdrs-move -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive functions


(defun mu4e-view-toggle-wrap-lines ()
  "Toggle line wrap in the message body."
  (interactive)
  (if mu4e~view-lines-wrapped
    (mu4e-view-refresh)
    (mu4e~view-wrap-lines)))

(defun mu4e-view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mu4e~view-cited-hidden
    (mu4e-view-refresh)
    (mu4e~view-hide-cited)))

(defun mu4e-view-refresh ()
  "Redisplay the current message, without wrapped lines or hidden
citations."
  (interactive)
  (mu4e-view mu4e-current-msg mu4e~view-hdrs-buffer t)
  (setq
    mu4e~view-lines-wrapped nil
    mu4e~view-cited-hidden nil))

(defun mu4e-view-kill-buffer-and-window ()
  "Quit the message view and return to the headers."
  (interactive)
  (when (buffer-live-p mu4e-view-buffer)
    (with-current-buffer mu4e-view-buffer
      (if (fboundp 'window-parent) ;; window-parent is an emacs24ism
	(if (window-parent)
	  (kill-buffer-and-window)
	  (kill-buffer))
	;; emacs23 hack: trial and error
	(condition-case nil
	  (kill-buffer-and-window)
	  (kill-buffer))))))

(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG (or message-at-point,
if nil), then do it. The actions are specified in
`mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point t)))
	  (actionfunc (mu4e-choose-action "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attachment handling
(defun mu4e~view-get-attach-num (prompt msg)
  "Ask the user with PROMPT for an attachment number for MSG, and
  ensure it is valid. The number is [1..n] for attachments
  [0..(n-1)] in the message."
  (let* ((attlist (plist-get msg :attachments))
	  (count (length attlist)))
    (when (zerop count) (error "No attachments for this message"))
    (if (= count 1)
      (read-number (format "%s (1): " prompt) 1)
      (read-number (format "%s (1-%d): " prompt count)))))

(defun mu4e~view-get-attach (msg attnum)
  "Return the attachment plist in MSG corresponding to attachment
number ATTNUM."
  (let ((attlist (plist-get msg :attachments)))
    (nth (- attnum 1) attlist)))

(defun mu4e-view-save-attachment (&optional msg attnum)
  "Save attachment number ATTNUM (or ask if nil) from MSG (or
message-at-point if nil) to disk."
  (interactive)
  (unless mu4e-attachment-dir
    (error "`mu4e-attachment-dir' is not set"))
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to save" msg)))
	  (att (mu4e~view-get-attach msg attnum))
	  (path (concat mu4e-attachment-dir "/" (plist-get att :name)))
	  (index (plist-get att :index))
	  (retry t))
    (while retry
      (setq path (expand-file-name (read-string "Save as " path)))
      (setq retry
	(and (file-exists-p path)
	  (not (y-or-n-p (concat "Overwrite " path "?"))))))
    (mu4e~proc-extract
      'save (plist-get msg :docid) index path)))


(defun mu4e-view-open-attachment (&optional msg attnum)
  "Open attachment number ATTNUM (or ask if nil) from MSG (or
message-at-point if nil)."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to open" msg)))
	  (att (mu4e~view-get-attach msg attnum))
	  (index (plist-get att :index)))
    (mu4e~proc-extract 'open (plist-get msg :docid) index)))


(defun mu4e~temp-action (docid index what &optional param)
  "Open attachment INDEX for message with DOCID, and invoke
ACTION."
  (interactive)
  (mu4e~proc-extract 'temp docid index nil what param))

(defun mu4e-view-open-attachment-with (msg attachnum &optional cmd)
  "Open MSG's attachment ATTACHNUM with CMD; if CMD is nil, ask
user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (cmd (or cmd (read-string "Shell command to open it with: ")))
	  (index (plist-get att :index)))
    (mu4e~temp-action (plist-get msg :docid) index "open-with" cmd)))

(defun mu4e-view-pipe-attachment (msg attachnum &optional pipecmd)
  "Feed MSG's attachment ATTACHNUM throught pipe PIPECMD; if
PIPECMD is nil, ask user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (pipecmd (or pipecmd (read-string "Pipe: ")))
	  (index (plist-get att :index)))
    (mu4e~temp-action (plist-get msg :docid) index "pipe" pipecmd)))

(defun mu4e-view-open-attachment-emacs (msg attachnum)
  "Open MSG's attachment ATTACHNUM in the current emacs instance."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (index (plist-get att :index)))
    (mu4e~temp-action (plist-get msg :docid) index "emacs")))


(defun mu4e-view-attachment-action (&optional msg)
  "Ask user what to do with attachments in MSG (or nil to use
message-at-point, then do it. The actions are specified in
`mu4e-view-attachment-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point t)))
	  (actionfunc (mu4e-choose-action
			"Action on attachment: "
			mu4e-view-attachment-actions))
	  (attnum (mu4e~view-get-attach-num "Which attachment" msg)))
    (when (and actionfunc attnum)
      (funcall actionfunc msg attnum))))


;; handler-function to handle the response we get from the server when we
;; want to do something with one of the attachments.
(defun mu4e-view-temp-handler (path what param)
  "Handler function for doing things with temp files (ie.,
attachments) in response to a (mu4e~proc-extract 'temp ... )."
    (cond
      ((string= what "open-with")
	;; 'param' will be the program to open-with
	(start-file-process-shell-command "*mu4e-open-with*" nil
	  (concat param " " path)))
      ((string= what "pipe")
	;; 'param' will be the pipe command, path the infile for this
	(mu4e-process-file-through-pipe path param))
      ((string= what "emacs")
	(find-file path)
	;; make the buffer read-only since it usually does not make
	;; sense to edit the temp buffer; use C-x C-q if you insist...
	(setq buffer-read-only t))
      (t (error "Unsupported action %S" what))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; marking
(defun mu4e~view-mark-set (mark)
  "Set mark on the current messages."
  (unless (buffer-live-p mu4e~view-hdrs-buffer)
    (error "No headers buffer available"))
  (let ((docid (mu4e-msg-field mu4e-current-msg :docid)))
    (with-current-buffer mu4e~view-hdrs-buffer
      (if (eq mark 'move)
	(mu4e-mark-for-move-set)
	(mu4e-mark-at-point mark)))))
  

(defun mu4e~split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))

(defun mu4e-view-unmark-all ()
  "If we're in split-view, unmark all messages. Otherwise, warn
user that unmarking only works in the header list."
  (interactive)
  (if (mu4e~split-view-p)
    (mu4e-mark-unmark-all)
    (message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point. Otherwise, warn
user that unmarking only works in the header list."
  (interactive)
  (if (mu4e~split-view-p)
    (mu4e~view-mark-set 'unmark)
    (message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-mark-for-move ()
  "Mark the current message for moving."
  (interactive)
  (mu4e~view-mark-set 'move)
  (mu4e~view-next-header))

(defun mu4e-view-mark-for-trash ()
  "Mark the current message for moving to the trash folder."
  (interactive)
  (mu4e~view-mark-set 'trash)
  (mu4e~view-next-header))

(defun mu4e-view-mark-for-delete ()
  "Mark the current message for deletion."
  (interactive)
  (mu4e~view-mark-set 'delete)
  (mu4e~view-next-header))

(defun mu4e-view-marked-execute ()
  "If we're in split-view, execute the marks. Otherwise, warn user
that execution can only take place in n the header list."
  (interactive)
  (if (mu4e~split-view-p)
    (with-current-buffer mu4e~view-hdrs-buffer
      (mu4e-mark-execute-all))
    (message "Execution needs to be done in the header list view")))

(defun mu4e-view-go-to-url (num)
  "Go to a numbered url."
  (interactive "nGo to url with number: ")
  (let ((url (gethash num mu4e~view-link-map)))
    (unless url (error "Invalid number for URL"))
    (browse-url url)))

(defconst mu4e~view-raw-buffer-name "*mu4e-raw-view*"
  "*internal* Name for the raw message view buffer")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-field-at-point :path))
	 (buf (get-buffer-create mu4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert-file-contents path)
	(view-mode)
	(goto-char (point-min))))
    (switch-to-buffer buf)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD, and display
the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-field-at-point :path)))
    (mu4e-process-file-through-pipe path cmd)))

(provide 'mu4e-view)
