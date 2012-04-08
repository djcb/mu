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
(eval-when-compile (require 'cl))
(require 'html2text)
;; we prefer the improved fill-region
(require 'filladapt nil 'noerror)
(require 'comint)

(defconst mu4e-view-buffer-name "*mu4e-view*"
  "*internal* Name for the message view buffer")

(defvar mu4e-view-buffer nil "*internal* The view buffer.")

;; some buffer-local variables
(defvar mu4e-hdrs-buffer nil
  "*internal* Headers buffer connected to this view.")

(defvar mu4e--current-msg nil
  "*internal* The plist describing the current message.")

(defun mu4e-view-message-with-msgid (msgid)
  "View message with MSGID. This is meant for external programs
wanting to show specific messages - for example, `mu4e-org'."
  (mu4e-proc-view-msg msgid))

(defun mu4e-view (msg hdrsbuf &optional update)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc. If
UPDATE is nil, the current message may be (visually) 'massaged',
based on the settings of `mu4e-view-wrap-lines' and
`mu4e-view-hide-cited'.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mu4e-view-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (setq mu4e-view-buffer buf)
      (erase-buffer)
      (insert
	(mapconcat
	  (lambda (field)
	    (let ((fieldname (cdr (assoc field mu4e-header-names)))
		   (fieldval (plist-get msg field)))
	      (case field
		(:subject  (mu4e-view-header fieldname fieldval))
		(:path	   (mu4e-view-header fieldname fieldval))
		(:maildir  (mu4e-view-header fieldname fieldval))
		(:flags	   (mu4e-view-header fieldname
			     (if fieldval (format "%S" fieldval) "")))
		;; contact fields
		(:to	   (mu4e-view-contacts msg field))
		(:from	   (mu4e-view-contacts msg field))
		(:cc	   (mu4e-view-contacts msg field))
		(:bcc	   (mu4e-view-contacts msg field))

		;; if we (`user-mail-address' are the From, show To, otherwise,
		;; show From
		(:from-or-to
		  (let* ((from (plist-get msg :from))
			  (from (and from (cdar from))))
		    (if (and from (string-match mu4e-user-mail-address-regexp from))
		      (mu4e-view-contacts msg :to)
		      (mu4e-view-contacts msg :from))))

		;; date
		(:date
		  (let ((datestr
			  (when fieldval (format-time-string mu4e-view-date-format
					   fieldval))))
		    (if datestr (mu4e-view-header fieldname datestr) "")))
		;; size
		(:size	(mu4e-view-size  msg)
		  (let ((sizestr (when size (format "%d bytes"))))
		    (if sizestr (mu4e-view-header fieldname sizestr))))
		;; attachments
		(:attachments (mu4e-view-attachments msg))
		(t               (error "Unsupported field: %S" field)))))
	    mu4e-view-fields "")
	"\n"
	(mu4e-body-text msg))

      ;; initialize view-mode
      (mu4e-view-mode)
      (setq ;; these are buffer-local
	buffer-read-only t
	mu4e--current-msg msg
	mu4e-hdrs-buffer hdrsbuf
	mu4e-link-map (make-hash-table :size 32 :rehash-size 2 :weakness nil))

      (switch-to-buffer buf)
      (goto-char (point-min))

      (mu4e-color-cited)
      (mu4e-mark-footer)
      (mu4e-make-urls-clickable)

      (unless update
	;; if we're showing the message for the first time, use the values of
	;; user-settable variables `mu4e-view-wrap-lines' and
	;; `mu4e-view-hide-cited' to determine whether we should wrap/hide
	(progn
	  (when mu4e-view-wrap-lines (mu4e-view-wrap-lines))
	  (when mu4e-view-hide-cited (mu4e-view-hide-cited))))

      ;; no use in trying to set flags again
      (unless update
	(mu4e-view-mark-as-read-maybe)))))

(defun mu4e-view-header (key val &optional dont-propertize-val)
  "Show header FIELD for MSG with KEY. ie. <KEY>: value-of-FIELD."
  (if val
    (concat
      (propertize key 'face 'mu4e-view-header-key-face) ": "
      (if dont-propertize-val
	val
        (propertize val 'face 'mu4e-view-header-value-face))
      "\n")
    ""))


(defun mu4e-view-contacts (msg field)
  "Add a header for a contact field (ie., :to, :from, :cc, :bcc)."
  (let* ((lst (plist-get msg field))
	  (fieldname (cdr (assoc field mu4e-header-names)))
	  (contacts
	    (and lst
	      (mapconcat
		(lambda(c)
		  (let ((name (car c)) (email (cdr c)))
		    (if name
		      (format "%s <%s>" name email)
		      (format "%s" email)))) lst ", "))))
    (if contacts
      (mu4e-view-header fieldname contacts)
      "")))

(defvar mu4e-attach-map nil
  "*internal* Hash which maps a number to a (part-id name mime-type).")


(defun mu4e-open-save-attach-func (num is-open)
  "Return a function that offers to extracts (saves) attachment NUM
if IS-OPEN is nil, and otherwise open it."
  (lexical-let ((num num) (is-open is-open))
    (lambda ()
      (interactive)
      (if is-open
	(mu4e-view-open-attachment num)
	(mu4e-view-extract-attachment num)))))

(defun mu4e-view-attachments (msg)
  "Display attachment information; the field looks like something like:
   	:attachments ((:index 4 :name \"test123.doc\"
                       :mime-type \"application/msword\" :size 1234))."
  (let ((atts (plist-get msg :attachments)))
    (when atts
      (setq mu4e-attach-map
	(make-hash-table :size 32 :rehash-size 2 :weakness nil))
      (let* ((id 0)
	      (vals
		(mapconcat
		  (lambda (att)
		    (let ( (index (plist-get att :index))
			   (name (plist-get att :name))
			   (mime-type (plist-get att :mime-type))
			   (size (plist-get att :size))
			   (map (make-sparse-keymap)))
		      (incf id)
		      (puthash id att mu4e-attach-map)
		      ;; mouse-2, RET offers to save the attachment,
		      ;; S-mouse-2, S-Ret opens it.
		      (define-key map [mouse-2] (mu4e-open-save-attach-func id nil))
		      (define-key map [?\r]     (mu4e-open-save-attach-func id nil))
		      (define-key map [S-mouse-2](mu4e-open-save-attach-func id t))
		      (define-key map (kbd "<S-return>")
			(mu4e-open-save-attach-func id t))
		      (concat
			(propertize (format "[%d]" id)
			  'face 'mu4e-view-attach-number-face)
			(propertize name
			  'face 'mu4e-view-link-face
			  'keymap map
			  'mouse-face 'highlight)
			(when (and size (> size 0))
			  (concat (format "(%s)"
			     (propertize (mu4e-display-size size)
				  'face 'mu4e-view-header-key-face)))))))
		    atts ", ")))
		(mu4e-view-header (format "Attachments(%d)" id) vals t)))))


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

      (define-key map "." 'mu4e-raw-view)
      (define-key map "|" 'mu4e-view-pipe)
      ;; (define-key map "I" 'mu4e-inspect-message)

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'scroll-up)
      (define-key map (kbd "<home>")
	'(lambda () (interactive) (goto-char (point-min))))
      (define-key map (kbd "<end>")
	'(lambda () (interactive) (goto-char (point-max))))
      (define-key map (kbd "RET")
	'(lambda () (interactive) (scroll-up 1)))
      (define-key map (kbd "<backspace>")
	'(lambda () (interactive) (scroll-up -1)))


      ;; navigation between messages
      (define-key map "n" 'mu4e-view-next-header)
      (define-key map "p" 'mu4e-view-prev-header)

      ;; attachments
      (define-key map "e" 'mu4e-view-extract-attachment)
      (define-key map "o" 'mu4e-view-open-attachment)

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
      (define-key map "U" 'mu4e-view-unmark)
      (define-key map "x" 'mu4e-view-marked-execute)

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
	  '("View raw message" . mu4e-raw-view))
	(define-key menumap [pipe]
	  '("Pipe through shell" . mu4e-view-pipe))
	;; (define-key menumap [inspect]
	;;   '("Inspect with guile" . mu4e-inspect-message))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . mu4e-view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . mu4e-view-extract-attachment))
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
	(define-key menumap [next]  '("Next" . mu4e-view-next-header))
	(define-key menumap [previous]  '("Previous" . mu4e-view-prev-header)))
      map)))

(fset 'mu4e-view-mode-map mu4e-view-mode-map)


(defvar mu4e-lines-wrapped nil "*internal* Whether lines are wrapped.")
(defvar mu4e-cited-hidden nil "*internal* Whether cited lines are hidden.")

(define-derived-mode mu4e-view-mode special-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e.
\\{mu4e-view-mode-map}."
  (use-local-map mu4e-view-mode-map)

  (make-local-variable 'mu4e-hdrs-buffer)
  (make-local-variable 'mu4e--current-msg)
  (make-local-variable 'mu4e-link-map)

  (make-local-variable 'mu4e-lines-wrapped)
  (make-local-variable 'mu4e-cited-hidden)

  ;; filladapt is much better than the built-in filling
  ;; esp. with '>' cited parts
  (when (fboundp 'filladapt-mode)
    (filladapt-mode))

  (setq truncate-lines t))


;; we mark messages are as read when we leave the message; i.e., when skipping to
;; the next/previous one, or leaving the view buffer altogether.

(defun mu4e-view-mark-as-read-maybe ()
  "Clear the current message's New/Unread status and set it to
Seen; if the message is not New/Unread, do nothing."
  (when mu4e--current-msg
    (let ((flags (plist-get mu4e--current-msg :flags))
	   (docid (plist-get mu4e--current-msg :docid)))
      ;; is it a new message?
      (when (or (member 'unread flags) (member 'new flags))
	(mu4e-proc-flag docid "+S-u-N")))))


(defun mu4e-color-cited ()
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

(defun mu4e-mark-footer ()
  "Give the message footers a distinctive color."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (let ((p (search-forward "\n-- \n" nil t)))
	(when p
	  (add-text-properties p (point-max) '(face mu4e-view-footer-face)))))))

(defvar mu4e-link-map nil
  "*internal* A map of some number->url so we can jump to url by number.")

(defconst mu4e-url-regexp
  "\\(https?://[-+a-zA-Z0-9.?_$%/+&#@!~,:;=/()]+\\)"
  "*internal* regexp that matches URLs; match-string 1 will contain
  the matched URL, if any.")

(defun mu4e-browse-url-func (url)
  "Return a function that executes `browse-url' with URL."
  (lexical-let ((url url))
    (lambda ()
      (interactive)
      (browse-url url))))


;; this is fairly simplistic...
(defun mu4e-make-urls-clickable ()
  "Turn things that look like URLs into clickable things, and
number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mu4e-url-regexp nil t)
	(let ((url (match-string 0))
	       (map (make-sparse-keymap)))
	  (define-key map [mouse-2] (mu4e-browse-url-func url))
	  (define-key map [?\r] (mu4e-browse-url-func url))
	  (puthash (incf num) url mu4e-link-map)
	  (add-text-properties 0 (length url)
	    `(face mu4e-view-link-face
	       mouse-face highlight
	       keymap ,map) url)
	  (replace-match (concat url
			   (propertize (format "[%d]" num)
			     'face 'mu4e-view-url-number-face))))))))


;; raw mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some buffer-local variables
(defconst mu4e-raw-view-buffer-name "*mu4e-raw-view*"
  "*internal* Name for the raw message view buffer")

(defvar mu4e-raw-view-buffer nil "*internal* The raw view buffer.")

(defvar mu4e-raw-view-mode-map nil
  "Keymap for \"*mu4e-raw-view*\" buffers.")

(unless mu4e-raw-view-mode-map
  (setq mu4e-raw-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mu4e-raw-view-quit-buffer)
      (define-key map "." 'mu4e-raw-view-quit-buffer)

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'scroll-up)
      (define-key map (kbd "<home>")
	'(lambda () (interactive) (goto-char (point-min))))
      (define-key map (kbd "<end>")
	'(lambda () (interactive) (goto-char (point-max))))
      (define-key map (kbd "RET")
	'(lambda () (interactive) (scroll-up 1)))
      (define-key map (kbd "<backspace>")
	'(lambda () (interactive) (scroll-up -1)))
      map)))

(fset 'mu4e-raw-view-mode-map mu4e-raw-view-mode-map)

(define-derived-mode mu4e-raw-view-mode special-mode
  "mu4e:raw"
  "Major mode for viewing of raw e-mail message in mu4e.
\\{mu4e-raw-view-mode-map}.")


(defun mu4e-raw-view-message (msg view-buffer)
  "Display the raw contents of message MSG in a new buffer."
  (let ((buf (get-buffer-create mu4e-raw-view-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file file)
      ;; initialize view-mode
      (mu4e-raw-view-mode)
      (setq mu4e-raw-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-view-shell-command-on-raw-message (msg view-buffer cmd)
  "Process the raw message with shell command CMD."
  (let ((buf (get-buffer-create mu4e-raw-view-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (process-file-shell-command cmd file buf)
      (mu4e-raw-view-mode)
      (setq mu4e-raw-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-raw-view-quit-buffer ()
  "Quit the raw view and return to the message."
  (interactive)
  (kill-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions for org-contacts

(defun mu4e-view-snarf-from (name-or-email)
  "Get the From:-data for the current message; NAME-OR-EMAIL should
be a symbol 'name or 'email to get the corresponding field. If the
field is not found, \"\" is returned.

You can use this with e.g. org-contact with a template like:
  (\"c\" \"Contacts\" entry (file \"~/Org/contacts.org\")
          \"* %(mu4e-view-snarf-from 'name)
  :PROPERTIES:
  :EMAIL: %(mu4e-view-snarf-from 'email)
  :END:\")))

See the `org-contacts' documentation for more details."
  ;; FIXME: we need to explictly go to some (hopefully the right!) view buffer,
  ;; since when using this from org-capture, we'll be taken to the capture
  ;; buffer instead.
  (with-current-buffer mu4e-view-buffer-name
    (unless (eq major-mode 'mu4e-view-mode)
      (error "Not in mu4e-view mode."))
    (unless mu4e--current-msg
      (error "No current message."))
    (let ((from (car-safe (plist-get mu4e--current-msg :from))))
      (cond
	((not from) "") ;; nothing found
	((eq name-or-email 'name)
	  (or (car-safe from) ""))
	((eq name-or-email 'email)
	  (or (cdr-safe from) ""))
	(t (error "Not supported: %S" name-or-email))))))


(defun mu4e-view-wrap-lines ()
  "Wrap lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (search-forward "\n\n") ;; search for the message body
	(fill-region (point) (point-max)))
      (setq mu4e-lines-wrapped t))))

(defun mu4e-view-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (flush-lines "^[:blank:]*>")
      (setq mu4e-cited-hidden t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive functions

(defun mu4e-view-toggle-wrap-lines ()
  "Toggle line wrap in the message body."
  (interactive)
  (if mu4e-lines-wrapped
    (mu4e-view-refresh)
    (mu4e-view-wrap-lines)))

(defun mu4e-view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mu4e-cited-hidden
    (mu4e-view-refresh)
    (mu4e-view-hide-cited)))

(defun mu4e-view-refresh ()
  "Redisplay the current message, without wrapped lines or hidden
citations."
  (interactive)
  (mu4e-view mu4e--current-msg mu4e-hdrs-buffer t)
  (setq
    mu4e-lines-wrapped nil
    mu4e-cited-hidden nil))

(defun mu4e-view-kill-buffer-and-window ()
  "Quit the message view and return to the headers."
  (interactive)
  (when (buffer-live-p mu4e-view-buffer)
    (with-current-buffer mu4e-view-buffer
      ;; (mu4e-kill-buffer-and-window mu4e-view-buffer)
      (kill-buffer-and-window))))
    
(defun mu4e-view-next-header ()
  "View the next header."
  (interactive)
  (when (mu4e-next-header)
    (mu4e-view-message)))

(defun mu4e-view-prev-header ()
  "View the previous header."
  (interactive)
  (when (mu4e-prev-header)
    (mu4e-view-message)))

(defun mu4e-view-mark-for-move ()
  "Mark the current message for moving."
  (interactive)
  (when (mu4e-mark-for-move)
    (mu4e-view-message)))

(defun mu4e-view-mark-for-trash ()
  "Mark the current message for moving to the trash folder."
  (interactive)
  (when (mu4e-mark-for-trash)
    (mu4e-view-message)))

(defun mu4e-view-mark-for-delete ()
  "Mark the current message for deletion."
  (interactive)
  (when (mu4e-mark-for-delete)
    (mu4e-view-message)))

(defun mu4e-view-extract-attachment (attnum)
  "Extract the attachment with ATTNUM."
  (unless mu4e-attachment-dir (error "`mu4e-attachment-dir' is not set"))
  (when (or (null mu4e-attach-map) (zerop (hash-table-count mu4e-attach-map)))
    (error "No attachments for this message"))
  (interactive "nAttachment to extract:")
  (let* ((att  (gethash attnum mu4e-attach-map))
	  (path (and att (concat mu4e-attachment-dir
			   "/"  (plist-get att :name))))
	  (id (and att (plist-get att :index)))
	  (retry t))
    (unless att (error "Not a valid attachment number"))
    (while retry
      (setq path (expand-file-name (read-string "Save as " path)))
      (setq retry
	(and (file-exists-p path)
	  (not (y-or-n-p (concat "Overwrite " path "?"))))))
    (mu4e-proc-save (plist-get mu4e--current-msg :docid) id path)))

(defun mu4e-view-open-attachment (attnum)
  "Extract the attachment with ATTNUM"
  (unless mu4e-attach-map
    (error "No attachments for this message"))
  (interactive "nAttachment to open:")
  (let* ((att (gethash attnum mu4e-attach-map))
	  (id (and att (plist-get att :index))))
    (unless id (error "Not a valid attachment number"))
    (mu4e-proc-open (plist-get mu4e--current-msg :docid) id)))

(defun mu4e-view-unmark ()
  "Warn user that unmarking only works in the header list."
  (interactive)
  (message "Unmarking needs to be done in the header list view"))


(defun mu4e-view-marked-execute ()
  "Warn user that execution can only take place in n the header
list."
  (interactive)
  (message "Execution needs to be done in the header list view"))

(defun mu4e-view-go-to-url (num)
  "Go to a numbered url."
  (interactive "nGo to url with number: ")
  (let ((url (gethash num mu4e-link-map)))
    (unless url (error "Invalid number for URL"))
    (browse-url url)))

(defun mu4e-raw-view ()
  "Show the the raw text of the current message."
  (interactive)
  (unless mu4e--current-msg
    (error "No current message"))
  (mu4e-raw-view-message mu4e--current-msg (current-buffer)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message through shell command CMD, and display the
results."
  (interactive "sShell command: ")
  (unless mu4e--current-msg
    (error "No current message"))
  (mu4e-view-shell-command-on-raw-message mu4e--current-msg
    (current-buffer) cmd))




(provide 'mu4e-view)
