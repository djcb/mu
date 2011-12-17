;; mu4e-view.el -- part of mm, the mu mail user agent
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

(defconst mu4e-view-buffer-name "*mu4e-view*"
  "*internal* Name for the message view buffer")

(defconst mu4e-view-raw-buffer-name "*mu4e-view-raw*"
  "*internal* Name for the raw message view buffer")

;; some buffer-local variables
(defvar mu4e-hdrs-buffer nil
  "*internal* Headers buffer connected to this view.")

(defvar mu4e-current-msg nil
  "*internal* The plist describing the current message.")

(defun mu4e-view (msg hdrsbuf &optional update)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc. If
UPDATE is non-nil, the current message will be (visually) updated.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mu4e-view-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
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
			  (when fieldval (format-time-string mu4e-view-date-format fieldval))))
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
	(mu4e-view-body msg))

      ;; initialize view-mode
      (mu4e-view-mode)
      (setq ;; these are buffer-local
	mode-name (if (plist-get msg :subject)
		    (truncate-string-to-width (plist-get msg :subject) 16 0 nil t)
		    (propertize "No subject" 'face 'mu4e-system-face))
	mu4e-current-msg msg
	mu4e-hdrs-buffer hdrsbuf
	mu4e-link-map (make-hash-table :size 32 :rehash-size 2 :weakness nil))

      (switch-to-buffer buf)
      (goto-char (point-min))
      (mu4e-view-beautify)

      (unless update
	(mu4e-view-mark-as-read-maybe)))))


(defun mu4e-view-body (msg)
  "Get the body for this message, which is either :body-txt,
or if not available, :body-html converted to text)."
  (or (plist-get msg :body-txt)
    (with-temp-buffer
      (plist-get msg :body-html)
      (html2text)
      (buffer-string))
    "No body found"))


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
			   (size (plist-get att :size)))
		      (incf id)
		      (puthash id att mu4e-attach-map)
		      (concat
			(propertize (format "[%d]" id) 'face 'mu4e-view-attach-number-face)
			(propertize name 'face 'mu4e-view-link-face)
			(if size
			  (concat
			    "(" (propertize (mu4e-display-size size) 'face 'mu4e-view-header-key-face)
			    ")")
			  "")
			)))
		    atts ", ")))
		(mu4e-view-header (format "Attachments(%d)" id) vals t)))))


(defvar mu4e-view-mode-map nil
  "Keymap for \"*mu4e-view*\" buffers.")
(unless mu4e-view-mode-map
  (setq mu4e-view-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q" 'mu4e-view-quit-buffer)

      (define-key map "s" 'mu4e-search)

      (define-key map "b" 'mu4e-search-bookmark)
      (define-key map "j" 'mu4e-jump-to-maildir)

      (define-key map "g" 'mu4e-view-go-to-url)
      
      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-edit-draft)

      (define-key map "." 'mu4e-view-raw)
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
      (define-key map "D" 'mu4e-view-mark-for-delete)
      (define-key map "a" 'mu4e-mark-for-move-quick)

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

	(define-key menumap [quit-buffer] '("Quit view" . mu4e-view-quit-buffer))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . mu4e-view-toggle-wrap-lines))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . mu4e-view-toggle-hide-cited))
	(define-key menumap [view-raw]
	  '("View raw message" . mu4e-view-raw))
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


(defvar mu4e-wrap-lines nil
  "*internal* Whether to wrap lines or not (variable controlled by
  `mu4e-view-toggle-wrap-lines').")

(defvar mu4e-hide-cited nil
  "*internal* Whether to hide cited lines or not (the variable can
  be changed with `mu4e-view-toggle-hide-cited').")


(defun mu4e-view-mode ()
  "Major mode for viewing an e-mail message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu4e-view-mode-map)

  (make-local-variable 'mu4e-hdrs-buffer)
  (make-local-variable 'mu4e-current-msg)
  (make-local-variable 'mu4e-link-map)

  (make-local-variable 'mu4e-wrap-lines)
  (make-local-variable 'mu4e-hide-cited)

  (setq major-mode 'mu4e-view-mode mode-name mu4e-view-buffer-name)
  (setq truncate-lines t buffer-read-only t))

;;;;;;


;; we mark messages are as read when we leave the message; ie., when skipping to
;; the next/previous one, or leaving the view buffer altogether.

(defun mu4e-view-mark-as-read-maybe ()
  "Clear the current message's New/Unread status and set it to
Seen; if the message is not New/Unread, do nothing."
  (when mu4e-current-msg
    (let ((flags (plist-get mu4e-current-msg :flags))
	   (docid (plist-get mu4e-current-msg :docid)))
      ;; is it a new message?
      (when (or (member 'unread flags) (member 'new flags))
	(mu4e-proc-flag docid "+S-u-N")))))


(defvar mu4e-link-map nil
  "*internal* A map of some number->url so we can jump to url by number.")

(defun mu4e-view-beautify ()
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
	(add-text-properties p (point-max) '(face mu4e-view-footer-face))))
    ;; this is fairly simplistic...
    (goto-char (point-min))
    (while (re-search-forward "\\(https?://[-a-zA-Z0-9?_.$%/=+&#@!~,:;]*\\)\\>"
	     nil t)
      (let ((subst (propertize (match-string-no-properties 0)
		     'face 'mu4e-view-link-face)))
	(incf num)
	(puthash num (match-string-no-properties 0) mu4e-link-map)
	(replace-match (concat subst
			 (propertize (format "[%d]" num)
			   'face 'mu4e-view-url-number-face))))))))


;; raw mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some buffer-local variables
(defvar mu4e-view-buffer nil
  "*internal* View buffer connected to this raw view.")

(defun mu4e-view-raw-mode ()
  "Major mode for viewing of raw e-mail message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu4e-view-raw-mode-map)

  (make-local-variable 'mu4e-view-buffer)

  (setq major-mode 'mu4e-view-raw-mode
    mode-name "mm: raw view")
  (setq truncate-lines t buffer-read-only t))

(defvar mu4e-view-raw-mode-map nil
  "Keymap for \"*mu4e-view-raw*\" buffers.")

(unless mu4e-view-raw-mode-map
  (setq mu4e-view-raw-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mu4e-view-raw-quit-buffer)
      (define-key map "." 'mu4e-view-raw-quit-buffer)

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
					     mu4e-view-raw-quit-buffer))
      map))))

(fset 'mu4e-view-raw-mode-map mu4e-view-raw-mode-map)


(defun mu4e-view-raw-message (msg view-buffer)
  "Display the raw contents of message MSG in a new buffer."
  (let ((buf (get-buffer-create mu4e-view-raw-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file file)
      ;; initialize view-mode
      (mu4e-view-raw-mode)
      (setq mu4e-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-view-shell-command-on-raw-message (msg view-buffer cmd)
  "Process the raw message with shell command CMD."
  (let ((buf (get-buffer-create mu4e-view-raw-buffer-name))
	 (inhibit-read-only t)
	 (file (plist-get msg :path)))
    (unless (and file (file-readable-p file))
      (error "Not a readable file: %S" file))
    (with-current-buffer buf
      (erase-buffer)
      (process-file-shell-command cmd file buf)
      (mu4e-view-raw-mode)
      (setq mu4e-view-buffer view-buffer)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


(defun mu4e-view-raw-quit-buffer ()
  "Quit the raw view and return to the message."
  (interactive)
  (if (buffer-live-p mu4e-view-buffer)
    (switch-to-buffer mu4e-view-buffer)
    (kill-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions for org-contacts

(defun mu4e-org-contacts-from (name-or-email)
  "Get a message field if we are in view mode; NAME-OR-EMAIL should
be either 'name or 'email to get the corresponding field. If the
field is not found, \"\" is returned. Use this with org-contact
with a template like:

  (\"c\" \"Contacts\" entry (file \"~/Org/contacts.org\")
          \"* %(mu4e-org-contacts-from 'name)
  :PROPERTIES:
  :EMAIL: %(mu4e-org-contacts-from 'email)
  :END:\")))

See the `org-contacts' documentation for more details."
  (with-current-buffer mu4e-view-buffer-name ;; hackish...
    (unless (eq major-mode 'mu4e-view-mode)
      (error "Not in mu4e-view mode."))
    (unless mu4e-current-msg
      (error "No current message."))
    (let ((from (car-safe (plist-get mu4e-current-msg :from))))
      (cond
	((not from) "") ;; nothing found
	((eq name-or-email 'name)
	  (or (car-safe from) ""))
	((eq name-or-email 'email)
	  (or (cdr-safe from) ""))
	(t (error "Not supported: %S" name-or-email))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; Interactive functions

(defun mu4e-view-toggle-wrap-lines ()
  "Toggle line wrap in the message body."
  (interactive)
  (if mu4e-wrap-lines
    (progn
      (setq mu4e-wrap-lines nil)
      (mu4e-view-refresh)) ;; back to normal
    (save-excursion
      (let ((inhibit-read-only t))
	(setq mu4e-wrap-lines t)
	(goto-char (point-min))
	(when (search-forward "\n\n") ;; search for the message body
	  (fill-region (point) (point-max)))))))

(defun mu4e-view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mu4e-hide-cited
    (progn
      (setq mu4e-hide-cited nil)
      (mu4e-view-refresh))
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(flush-lines "^[:blank:]*>")
	(setq mu4e-hide-cited t)))))


(defun mu4e-view-refresh ()
  "Redisplay the current message."
  (interactive)
  (mu4e-view mu4e-current-msg mu4e-hdrs-buffer t))


(defun mu4e-view-quit-buffer ()
  "Quit the message view and return to the headers."
  (interactive)
  (if (buffer-live-p mu4e-hdrs-buffer)
    (switch-to-buffer mu4e-hdrs-buffer)
    (kill-buffer)))

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
    (mu4e-proc-save (plist-get mu4e-current-msg :docid) id path)))

(defun mu4e-view-open-attachment (attnum)
  "Extract the attachment with ATTNUM"
  (unless mu4e-attach-map
    (error "No attachments for this message"))
  (interactive "nAttachment to open:")
  (let* ((att (gethash attnum mu4e-attach-map))
	  (id (and att (plist-get att :index))))
    (unless id (error "Not a valid attachment number"))
    (mu4e-proc-open (plist-get mu4e-current-msg :docid) id)))

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

(defun mu4e-view-raw ()
  "Show the the raw text of the current message."
  (interactive)
  (unless mu4e-current-msg
    (error "No current message"))
  (mu4e-view-raw-message mu4e-current-msg (current-buffer)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message through shell command CMD, and display the
results."
  (interactive "sShell command: ")
  (unless mu4e-current-msg
    (error "No current message"))
  (mu4e-view-shell-command-on-raw-message mu4e-current-msg (current-buffer) cmd))

(defconst mu4e-muile-buffer-name "*muile*"
  "Name of the buffer to execute muile.")

(defconst mu4e-muile-process-name "*muile*"
  "Name of the muile process.")

;; note, implementation is very basic/primitive; we probably need comint to do
;; something like geiser does (http://www.nongnu.org/geiser/). Desirable
;; features: a) the output is not editable b) tab-completions work
(defun mu4e-inspect-message ()
  "Inspect the current message in the Guile/Muile shell."
  (interactive)
  (unless mu4e-muile-binary (error "`mu4e-muile-binary' is not defined"))
  (unless (or (file-executable-p mu4e-muile-binary)
	    (executable-find mu4e-muile-binary))
    (error "%S not found" mu4e-muile-binary))
  (unless mu4e-current-msg
    (error "No current message"))
  (get-buffer-create mu4e-muile-buffer-name)
  (start-process mu4e-muile-buffer-name mu4e-muile-process-name
    mu4e-muile-binary "--msg" (plist-get mu4e-current-msg :path))
  (switch-to-buffer mu4e-muile-buffer-name)
  (shell-mode))

(provide 'mu4e-view)
