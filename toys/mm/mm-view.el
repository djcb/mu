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
(require 'mm-common)
(require 'html2text)
(require 'filladapt)

(defconst mm/view-buffer-name "*mm-view*"
  "*internal* Name for the message view buffer")

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
	    (case field
	      (:subject	(mm/view-header  "Subject" (plist-get msg :subject)))
	      (:path	(mm/view-header  "Path"    (plist-get msg :path)))
	      (:to	(mm/view-contacts msg field))
	      (:from	(mm/view-contacts msg field))
	      (:cc	(mm/view-contacts msg field))
	      (:bcc	(mm/view-contacts msg field))
	      (:date
		(let* ((date (plist-get msg :date))
			(datestr (when date (format-time-string "%c" date))))
		  (if datestr (mm/view-header "Date" datestr) "")))

	      (:flags	"") ;; TODO
	      (:maildir	(mm/view-header  "Maildir" (plist-get msg :maildir)))
	      (:size	(mm/view-size  msg)
		(let* ((size (plist-get msg :size))
			(sizestr (when size (format "%d bytes"))))
		  (if sizestr (mm/view-header "Size" sizestr))))

	      (:attachments (mm/view-attachments msg))
	      (t               (error "Unsupported field: %S" field))))
	  mm/view-headers "")
	"\n"
	(mm/view-body msg))

      ;; initialize view-mode
      (mm/view-mode)
      (setq ;; these are buffer-local
	mode-name (if (plist-get msg :subject)
		    (truncate-string-to-width (plist-get msg :subject) 16 0 nil t)
		    "No subject")
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


(defun mm/view-header (key val)
  "Show header FIELD for MSG with KEY. ie. <KEY>: value-of-FIELD\n."
  (if val
    (concat
      (propertize key 'face 'mm/view-header-key-face) ": "
      (propertize val 'face 'mm/view-header-value-face) "\n")
    ""))


(defun mm/view-contacts (msg field)
  (unless (member field '(:to :from :bcc :cc)) (error "Wrong type"))
  (let* ((lst (plist-get msg field))
	  (contacts
	    (when lst
	      (mapconcat
		(lambda(c)
		  (let ((name (car c)) (email (cdr c)))
		    (if name
		      (format "%s <%s>" name email)
		      (format "%s" email)))) lst ", "))))
    (if contacts
      (mm/view-header
	(case field (:to "To") (:from "From") (:bcc "Bcc") (:cc "Cc"))
	contacts)
      "")))

(defvar mm/attach-map nil
  "*internal* Hash which maps a number to a (part-id name mime-type).")


(defun mm/view-attachments (msg)
  "Display attachment information; the field looks like something like:
   	:attachments ((4 \"statement Bray Eile.doc\" \"application/msword\"))."
  (let ((atts (plist-get msg :attachments)))
    (when atts
      (setq mm/attach-map
	(make-hash-table :size 32 :rehash-size 2 :weakness nil))
      (let* ((id 0)
	      (vals
		(mapconcat
		  (lambda (att)
		    (incf id)
		    (puthash id att mm/attach-map)
		    (concat
		      (propertize (nth 1 att) 'face 'mm/view-link-face)
		      (propertize (format "[%d]" id) 'face 'mm/view-attach-number-face)))
		  atts ", ")))
	(mm/view-header (format "Attachments(%d):" id) vals)))))


(defvar mm/view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mm/view-quit-buffer)

    (define-key map "s" 'mm/search)
    (define-key map "j" 'mm/jump-to-maildir)

    (define-key map "g" 'mm/view-go-to-url)

    (define-key map "f" 'mm/compose-forward)
    (define-key map "r" 'mm/compose-reply)
    (define-key map "c" 'mm/compose-new)
    (define-key map "e" 'mm/edit-draft)

    ;; intra-message navigation
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "<home>")
      '(lambda () (interactive) (goto-char (point-min))))
    (define-key map (kbd "<end>")
      '(lambda () (interactive) (goto-char (point-max))))


    ;; navigation between messages
    (define-key map "n" 'mm/view-next-header)
    (define-key map "p" 'mm/view-prev-header)

    ;; attachments
    (define-key map "e" 'mm/view-extract-attachment)
    (define-key map "o" 'mm/view-open-attachment)

    ;; marking/unmarking
    (define-key map "d" 'mm/view-mark-for-trash)
    (define-key map (kbd "<backspace>") 'mm/mark-for-trash)

    (define-key map "D" 'mm/view-mark-for-delete)
    (define-key map (kbd "<delete>") 'mm/view-mark-for-delete)

    (define-key map "m" 'mm/view-mark-for-move)

    ;; misc
    (define-key map "w" 'mm/view-toggle-wrap-lines)
    (define-key map "h" 'mm/view-toggle-hide-quoted)

    (define-key map "R" 'mm/view-refresh)
    
    ;; next 3 only warn user when attempt in the message view
    (define-key map "u" 'mm/view-unmark)
    (define-key map "U" 'mm/view-unmark)
    (define-key map "x" 'mm/view-marked-execute)
    map)
  "Keymap for \"*mm-view*\" buffers.")
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
    (while (re-search-forward "\\| " nil t)
      (replace-match "" nil t))

    ;; give the footer a different color...
    (goto-char (point-min))
    (let ((p (search-forward "\n-- \n" nil t)))
      (when p
	(add-text-properties p (point-max) '(face mm/view-footer-face))))

    ;; this is fairly simplistic...
    (goto-char (point-min))
    (while (re-search-forward "\\(https?://.*\\)\\>" nil t)
      (let ((subst (propertize (match-string-no-properties 0)
		     'face 'mm/view-link-face)))
	(incf num)
	(puthash num (match-string-no-properties 0) mm/link-map)
	(replace-match (concat subst
			 (propertize (format "[%d]" num)
			   'face 'mm/view-url-number-face))))))))



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
  (let ((inhibit-read-only t))
    (kill-buffer)
    (switch-to-buffer mm/hdrs-buffer)))

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
  "Extract the attachment with ATTNUM"
  (unless mm/attachment-dir (error "`mm/attachment-dir' is not set"))
  (when (zerop (hash-table-count mm/attach-map))
    (error "No attachments for this message"))
  (interactive "nAttachment to extract:")
  (let* ((att  (gethash attnum mm/attach-map))
	  (path (when att (concat mm/attachment-dir "/" (nth 1 att))))
	  (retry t))
    (unless att (error "Not a valid attachment number"))
    (while retry
      (setq path (expand-file-name (read-string "Save as " path)))
      (setq retry
	(and (file-exists-p path)
	  (not (y-or-n-p (concat "Overwrite " path "?"))))))
    (mm/proc-save (plist-get mm/current-msg :docid) (car att) path)))

(defun mm/view-open-attachment (attnum)
  "Extract the attachment with ATTNUM"
  (when (zerop (hash-table-count mm/attach-map))
    (error "No attachments for this message"))
  (interactive "nAttachment to open:")
  (let* ((att  (gethash attnum mm/attach-map)))
    (unless att (error "Not a valid attachment number"))
    (mm/proc-open (plist-get mm/current-msg :docid) (car att))))


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


(provide 'mm-view)
