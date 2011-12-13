;; mm-view.el -- part of mm, the mu mail user agent
;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

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

;; In this file are function related to creating the list of one-line
;; descriptions of emails, aka 'headers' (not to be confused with headers like
;; 'To:' or 'Subject:')

;; mm

;;; Code:
(eval-when-compile (require 'cl))
(require 'html2text)
(require 'filladapt)
(require 'comint)

(defconst mm/view-buffer-name "*mm-view*"
  "*internal* Name for the message view buffer")

(defconst mm/view-raw-buffer-name "*mm-view-raw*"
  "*internal* Name for the raw message view buffer")

;; some buffer-local variables
(defvar mm/hdrs-buffer nil
  "*internal* Headers buffer connected to this view.")

(defvar mm/current-msg nil
  "*internal* The plist describing the current message.")

(defun mm/view (msg hdrsbuf &optional update)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc. If
UPDATE is non-nil, the current message will be (visually) updated.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mm/view-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
	(mapconcat
	  (lambda (field)
	    (let ((fieldname (cdr (assoc field mm/header-names)))
		   (fieldval (plist-get msg field)))
	      (case field

		(:subject  (mm/view-header fieldname fieldval))
		(:path	   (mm/view-header fieldname fieldval))
		(:maildir  (mm/view-header fieldname fieldval))
		(:flags	   (mm/view-header fieldname
			     (if fieldval (format "%S" fieldval) "")))
		;; contact fields
		(:to	   (mm/view-contacts msg field))
		(:from	   (mm/view-contacts msg field))
		(:cc	   (mm/view-contacts msg field))
		(:bcc	   (mm/view-contacts msg field))

		;; if we (`user-mail-address' are the From, show To, otherwise,
		;; show From
		(:from-or-to
		  (let* ((from (plist-get msg :from))
			  (from (and from (cdar from))))
		    (if (and from (string-match mm/user-mail-address-regexp from))
		      (mm/view-contacts msg :to)
		      (mm/view-contacts msg :from))))

		;; date
		(:date
		  (let ((datestr
			  (when fieldval (format-time-string mm/view-date-format fieldval))))
		    (if datestr (mm/view-header fieldname datestr) "")))
		;; size
		(:size	(mm/view-size  msg)
		  (let ((sizestr (when size (format "%d bytes"))))
		    (if sizestr (mm/view-header fieldname sizestr))))
		;; attachments
		(:attachments (mm/view-attachments msg))
		(t               (error "Unsupported field: %S" field)))))
	    mm/view-fields "")
	"\n"
	(mm/view-body msg))

      ;; initialize view-mode
      (mm/view-mode)
      (setq ;; these are buffer-local
	mode-name (if (plist-get msg :subject)
		    (truncate-string-to-width (plist-get msg :subject) 16 0 nil t)
		    (propertize "No subject" 'face 'mm/system-face))
	mm/current-msg msg
	mm/hdrs-buffer hdrsbuf
	mm/link-map (make-hash-table :size 32 :rehash-size 2 :weakness nil))

      (switch-to-buffer buf)
      (goto-char (point-min))
      (mm/view-beautify)

      (unless update
	(mm/view-mark-as-read-maybe)))))


(defun mm/view-body (msg)
  "Get the body for this message, which is either :body-txt,
or if not available, :body-html converted to text)."
  (or (plist-get msg :body-txt)
    (with-temp-buffer
      (plist-get msg :body-html)
      (html2text)
      (buffer-string))
    "No body found"))


(defun mm/view-header (key val &optional dont-propertize-val)
  "Show header FIELD for MSG with KEY. ie. <KEY>: value-of-FIELD."
  (if val
    (concat
      (propertize key 'face 'mm/view-header-key-face) ": "
      (if dont-propertize-val
	val
        (propertize val 'face 'mm/view-header-value-face))
      "\n")
    ""))


(defun mm/view-contacts (msg field)
  "Add a header for a contact field (ie., :to, :from, :cc, :bcc)."
  (let* ((lst (plist-get msg field))
	  (fieldname (cdr (assoc field mm/header-names)))
	  (contacts
	    (and lst
	      (mapconcat
		(lambda(c)
		  (let ((name (car c)) (email (cdr c)))
		    (if name
		      (format "%s <%s>" name email)
		      (format "%s" email)))) lst ", "))))
    (if contacts
      (mm/view-header fieldname contacts)
      "")))

(defvar mm/attach-map nil
  "*internal* Hash which maps a number to a (part-id name mime-type).")


(defun mm/view-attachments (msg)
  "Display attachment information; the field looks like something like:
   	:attachments ((:index 4 :name \"test123.doc\"
                       :mime-type \"application/msword\" :size 1234))."
  (let ((atts (plist-get msg :attachments)))
    (when atts
      (setq mm/attach-map
	(make-hash-table :size 32 :rehash-size 2 :weakness nil))
      (let* ((id 0)
	      (vals
		(mapconcat
		  (lambda (att)
		    (let ( (index (plist-get att :index))
			   (name (plist-get att :name))
			   (mime-type (plist-get att :mime-type))
			   (size (plist-get att :size)))
		      (incf id)
		      (puthash id att mm/attach-map)
		      (concat
			(propertize (format "[%d]" id) 'face 'mm/view-attach-number-face)
			(propertize name 'face 'mm/view-link-face)
			(if size
			  (concat
			    "(" (propertize (mm/display-size size) 'face 'mm/view-header-key-face)
			    ")")
			  "")
			)))
		    atts ", ")))
		(mm/view-header (format "Attachments(%d)" id) vals t)))))


(defvar mm/view-mode-map nil
  "Keymap for \"*mm-view*\" buffers.")
(unless mm/view-mode-map
  (setq mm/view-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q" 'mm/view-quit-buffer)

      (define-key map "s" 'mm/search)
      (define-key map "S" 'mm/search-full)

      (define-key map "b" 'mm/search-bookmark)
      (define-key map "j" 'mm/jump-to-maildir)

      (define-key map "g" 'mm/view-go-to-url)
      (define-key map "f" 'mm/compose-forward)
      (define-key map "r" 'mm/compose-reply)
      (define-key map "c" 'mm/compose-new)
      (define-key map "e" 'mm/edit-draft)

      (define-key map "." 'mm/view-raw)
      (define-key map "|" 'mm/view-pipe)
      ;; (define-key map "I" 'mm/inspect-message)

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
      (define-key map "n" 'mm/view-next-header)
      (define-key map "p" 'mm/view-prev-header)

      ;; attachments
      (define-key map "e" 'mm/view-extract-attachment)
      (define-key map "o" 'mm/view-open-attachment)

      ;; marking/unmarking
      (define-key map (kbd "<backspace>") 'mm/mark-for-trash)
      (define-key map "d" 'mm/view-mark-for-trash)

      (define-key map (kbd "<delete>") 'mm/view-mark-for-delete)
      (define-key map "D" 'mm/view-mark-for-delete)
      (define-key map "a" 'mm/mark-for-move-quick)

      (define-key map "m" 'mm/view-mark-for-move)

      ;; misc
      (define-key map "w" 'mm/view-toggle-wrap-lines)
      (define-key map "h" 'mm/view-toggle-hide-cited)

      (define-key map "R" 'mm/view-refresh)

      ;; next 3 only warn user when attempt in the message view
      (define-key map "u" 'mm/view-unmark)
      (define-key map "U" 'mm/view-unmark)
      (define-key map "x" 'mm/view-marked-execute)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "View")))
	(define-key map [menu-bar headers] (cons "View" menumap))

	(define-key menumap [quit-buffer] '("Quit view" . mm/view-quit-buffer))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . mm/view-toggle-wrap-lines))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . mm/view-toggle-hide-cited))
	(define-key menumap [view-raw]
	  '("View raw message" . mm/view-raw))
	(define-key menumap [pipe]
	  '("Pipe through shell" . mm/view-pipe))
	(define-key menumap [inspect]
	  '("Inspect with guile" . mm/inspect-message))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . mm/view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . mm/view-extract-attachment))
	(define-key menumap [goto-url]
	  '("Visit URL" . mm/view-go-to-url))

	(define-key menumap [sepa1] '("--"))
	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mm/view-mark-for-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mm/view-mark-for-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mm/view-mark-for-move))

	(define-key menumap [sepa2] '("--"))
	(define-key menumap [compose-new]  '("Compose new" . mm/compose-new))
	(define-key menumap [forward]  '("Forward" . mm/compose-forward))
	(define-key menumap [reply]  '("Reply" . mm/compose-reply))
	(define-key menumap [sepa3] '("--"))

	(define-key menumap [search]  '("Search" . mm/search))
	(define-key menumap [jump]  '("Jump to maildir" . mm/jump-to-maildir))

	(define-key menumap [sepa4] '("--"))
	(define-key menumap [next]  '("Next" . mm/view-next-header))
	(define-key menumap [previous]  '("Previous" . mm/view-prev-header)))
      map)))

(fset 'mm/view-mode-map mm/view-mode-map)


(defvar mm/wrap-lines nil
  "*internal* Whether to wrap lines or not (variable controlled by
  `mm/view-toggle-wrap-lines').")

(defvar mm/hide-cited nil
  "*internal* Whether to hide cited lines or not (the variable can
  be changed with `mm/view-toggle-hide-cited').")


(defun mm/view-mode ()
  "Major mode for viewing an e-mail message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mm/view-mode-map)

  (make-local-variable 'mm/hdrs-buffer)
  (make-local-variable 'mm/current-msg)
  (make-local-variable 'mm/link-map)

  (make-local-variable 'mm/wrap-lines)
  (make-local-variable 'mm/hide-cited)

  (setq major-mode 'mm/view-mode mode-name mm/view-buffer-name)
  (setq truncate-lines t buffer-read-only t))

;;;;;;


;; we mark messages are as read when we leave the message; ie., when skipping to
;; the next/previous one, or leaving the view buffer altogether.

(defun mm/view-mark-as-read-maybe ()
  "Clear the current message's New/Unread status and set it to
Seen; if the message is not New/Unread, do nothing."
  (when mm/current-msg
    (let ((flags (plist-get mm/current-msg :flags))
	   (docid (plist-get mm/current-msg :docid)))
      ;; is it a new message?
      (when (or (member 'unread flags) (member 'new flags))
	(mm/proc-flag docid "+S-u-N")))))


(defvar mm/link-map nil
  "*internal* A map of some number->url so we can jump to url by number.")

(defun mm/view-beautify ()
  "Improve the message view a bit, by making URLs clickable,
removing '^M' etc."
  (let ((num 0))
  (save-excursion
    ;; remove the stupid CRs
    (goto-char (point-min))
    (while (re-search-forward "[\r\240]" nil t)
      (replace-match " " nil t))
    ;; give the footer a different color...
    (goto-char (point-min))
    (let ((p (search-forward "\n-- \n" nil t)))
      (when p
	(add-text-properties p (point-max) '(face mm/view-footer-face))))
    ;; this is fairly simplistic...
    (goto-char (point-min))
    (while (re-search-forward "\\(https?://[-a-zA-Z0-9?_.$%/=+&#@!~,:;]*\\)\\>"
	     nil t)
      (let ((subst (propertize (match-string-no-properties 0)
		     'face 'mm/view-link-face)))
	(incf num)
	(puthash num (match-string-no-properties 0) mm/link-map)
	(replace-match (concat subst
			 (propertize (format "[%d]" num)
			   'face 'mm/view-url-number-face))))))))


;; raw mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some buffer-local variables
(defvar mm/view-buffer nil
  "*internal* View buffer connected to this raw view.")

(defun mm/view-raw-mode ()
  "Major mode for viewing of raw e-mail message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mm/view-raw-mode-map)

  (make-local-variable 'mm/view-buffer)

  (setq major-mode 'mm/view-raw-mode
    mode-name "mm: raw view")
  (setq truncate-lines t buffer-read-only t))

(defvar mm/view-raw-mode-map nil
  "Keymap for \"*mm-view-raw*\" buffers.")

(unless mm/view-raw-mode-map
  (setq mm/view-raw-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mm/view-raw-quit-buffer)
      (define-key map "." 'mm/view-raw-quit-buffer)

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

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Raw view")))
	(define-key map [menu-bar headers] (cons "Raw view" menumap))
	(define-key menumap [quit-buffer] '("Quit" .
					     mm/view-raw-quit-buffer))
      map))))

(fset 'mm/view-raw-mode-map mm/view-raw-mode-map)


(defun mm/view-raw-message (msg view-buffer)
  "Display the raw contents of message MSG in a new buffer."
  (let ((buf (get-buffer-create mm/view-raw-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file file)
      ;; initialize view-mode
      (mm/view-raw-mode)
      (setq mm/view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mm/view-shell-command-on-raw-message (msg view-buffer cmd)
  "Process the raw message with shell command CMD."
  (let ((buf (get-buffer-create mm/view-raw-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (process-file-shell-command cmd file buf)
      (mm/view-raw-mode)
      (setq mm/view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mm/view-raw-quit-buffer ()
  "Quit the raw view and return to the message."
  (interactive)
  (if (buffer-live-p mm/view-buffer)
    (switch-to-buffer mm/view-buffer)
    (kill-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions for org-contacts

(defun mm/org-contacts-from (name-or-email)
  "Get a message field if we are in view mode; NAME-OR-EMAIL should
be either 'name or 'email to get the corresponding field. If the
field is not found, \"\" is returned. Use this with org-contact
with a template like:

  (\"c\" \"Contacts\" entry (file \"~/Org/contacts.org\")
          \"* %(mm/org-contacts-from 'name)
  :PROPERTIES:
  :EMAIL: %(mm/org-contacts-from 'email)
  :END:\")))

See the `org-contacts' documentation for more details."
  (with-current-buffer mm/view-buffer-name ;; hackish...
    (unless (eq major-mode 'mm/view-mode)
      (error "Not in mm/view mode."))
    (unless mm/current-msg
      (error "No current message."))
    (let ((from (car-safe (plist-get mm/current-msg :from))))
      (cond
	((not from) "") ;; nothing found
	((eq name-or-email 'name)
	  (or (car-safe from) ""))
	((eq name-or-email 'email)
	  (or (cdr-safe from) ""))
	(t (error "Not supported: %S" name-or-email))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; Interactive functions

(defun mm/view-toggle-wrap-lines ()
  "Toggle line wrap in the message body."
  (interactive)
  (if mm/wrap-lines
    (progn
      (setq mm/wrap-lines nil)
      (mm/view-refresh)) ;; back to normal
    (save-excursion
      (let ((inhibit-read-only t))
	(setq mm/wrap-lines t)
	(goto-char (point-min))
	(when (search-forward "\n\n") ;; search for the message body
	  (fill-region (point) (point-max)))))))

(defun mm/view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mm/hide-cited
    (progn
      (setq mm/hide-cited nil)
      (mm/view-refresh))
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(flush-lines "^[:blank:]*>")
	(setq mm/hide-cited t)))))


(defun mm/view-refresh ()
  "Redisplay the current message."
  (interactive)
  (mm/view mm/current-msg mm/hdrs-buffer t))


(defun mm/view-quit-buffer ()
  "Quit the message view and return to the headers."
  (interactive)
  (if (buffer-live-p mm/hdrs-buffer)
    (switch-to-buffer mm/hdrs-buffer)
    (kill-buffer)))

(defun mm/view-next-header ()
  "View the next header."
  (interactive)
  (when (mm/next-header)
    (mm/view-message)))

(defun mm/view-prev-header ()
  "View the previous header."
  (interactive)
  (when (mm/prev-header)
    (mm/view-message)))

(defun mm/view-mark-for-move ()
  "Mark the current message for moving."
  (interactive)
  (when (mm/mark-for-move)
    (mm/view-message)))

(defun mm/view-mark-for-trash ()
  "Mark the current message for moving to the trash folder."
  (interactive)
  (when (mm/mark-for-trash)
    (mm/view-message)))

(defun mm/view-mark-for-delete ()
  "Mark the current message for deletion."
  (interactive)
  (when (mm/mark-for-delete)
    (mm/view-message)))

(defun mm/view-extract-attachment (attnum)
  "Extract the attachment with ATTNUM."
  (unless mm/attachment-dir (error "`mm/attachment-dir' is not set"))
  (when (or (null mm/attach-map) (zerop (hash-table-count mm/attach-map)))
    (error "No attachments for this message"))
  (interactive "nAttachment to extract:")
  (let* ((att  (gethash attnum mm/attach-map))
	  (path (and att (concat mm/attachment-dir
			   "/"  (plist-get att :name))))
	  (id (and att (plist-get att :index)))
	  (retry t))
    (unless att (error "Not a valid attachment number"))
    (while retry
      (setq path (expand-file-name (read-string "Save as " path)))
      (setq retry
	(and (file-exists-p path)
	  (not (y-or-n-p (concat "Overwrite " path "?"))))))
    (mm/proc-save (plist-get mm/current-msg :docid) id path)))

(defun mm/view-open-attachment (attnum)
  "Extract the attachment with ATTNUM"
  (unless mm/attach-map
    (error "No attachments for this message"))
  (interactive "nAttachment to open:")
  (let* ((att (gethash attnum mm/attach-map))
	  (id (and att (plist-get att :index))))
    (unless id (error "Not a valid attachment number"))
    (mm/proc-open (plist-get mm/current-msg :docid) id)))

(defun mm/view-unmark ()
  "Warn user that unmarking only works in the header list."
  (interactive)
  (message "Unmarking needs to be done in the header list view"))


(defun mm/view-marked-execute ()
  "Warn user that execution can only take place in n the header
list."
  (interactive)
  (message "Execution needs to be done in the header list view"))

(defun mm/view-go-to-url (num)
  "Go to a numbered url."
  (interactive "nGo to url with number: ")
  (let ((url (gethash num mm/link-map)))
    (unless url (error "Invalid number for URL"))
    (browse-url url)))

(defun mm/view-raw ()
  "Show the the raw text of the current message."
  (interactive)
  (unless mm/current-msg
    (error "No current message"))
  (mm/view-raw-message mm/current-msg (current-buffer)))

(defun mm/view-pipe (cmd)
  "Pipe the message through shell command CMD, and display the
results."
  (interactive "sShell command: ")
  (unless mm/current-msg
    (error "No current message"))
  (mm/view-shell-command-on-raw-message mm/current-msg (current-buffer) cmd))

(defconst mm/muile-buffer-name "*muile*"
  "Name of the buffer to execute muile.")

(defconst mm/muile-process-name "*muile*"
  "Name of the muile process.")

;; note, implementation is very basic/primitive; we probably need comint to do
;; something like geiser does (http://www.nongnu.org/geiser/). Desirable
;; features: a) the output is not editable b) tab-completions work
(defun mm/inspect-message ()
  "Inspect the current message in the Guile/Muile shell."
  (interactive)
  (unless mm/muile-binary (error "`mm/muile-binary' is not defined"))
  (unless (or (file-executable-p mm/muile-binary)
	    (executable-find mm/muile-binary))
    (error "%S not found" mm/muile-binary))
  (unless mm/current-msg
    (error "No current message"))
  (get-buffer-create mm/muile-buffer-name)
  (start-process mm/muile-buffer-name mm/muile-process-name
    mm/muile-binary "--msg" (plist-get mm/current-msg :path))
  (switch-to-buffer mm/muile-buffer-name)
  (shell-mode))

(provide 'mm-view)
