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

(defconst mm/view-buffer-name "*mm-view*"
  "*internal* Name for the message view buffer")

;; some buffer-local variables
(defvar mm/hdrs-buffer nil
  "*internal* Headers buffer connected to this view.")

(defvar mm/current-msg nil
  "*internal* The plist describing the current message.")

(defun mm/view (msg hdrsbuf)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mm/view-buffer-name)) (inhibit-read-only t))
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

	      (:attachments "") ;; TODO
	      (t               (error "Unsupported field: %S" field))))
	  mm/view-headers "")
	"\n"
	(mm/view-body msg))
      (mm/view-mode)
      (setq
	mode-name (format "%s" mm/view-buffer-name (plist-get msg :docid))
	;; these are buffer-local
	mm/current-msg msg
	mm/hdrs-buffer hdrsbuf)
      (switch-to-buffer buf)
      (goto-char (point-min)))))


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


(defvar mm/view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mm/view-quit-buffer)

    (define-key map "s" 'mm/search)
    (define-key map "j" 'mm/jump-to-maildir)

    ;; (define-key map "f" 'mua/view-forward)
    ;; (define-key map "r" 'mua/view-reply)
    ;; (define-key map "c" 'mua/view-compose)

    ;; navigation between messages
    (define-key map "n" 'mm/view-next)
    (define-key map "p" 'mm/view-prev)

    ;; marking/unmarking
    (define-key map "d" 'mm/view-mark-for-trash)
    (define-key map "D" 'mm/view-mark-for-delete)
    (define-key map "m" 'mm/view-mark-for-move)

    ;; next two only warn user
    (define-key map "u" 'mm/view-unmark)
    (define-key map "U" 'mm/view-unmark)

    (define-key map "x" 'mm/view-marked-execute)
    map)
  "Keymap for \"*mm-view*\" buffers.")
(fset 'mm/view-mode-map mm/view-mode-map)


(defun mm/view-mode ()
  "Major mode for viewing an e-mail message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mm/view-mode-map)

  (make-local-variable 'mm/hdrs-buffer)
  (make-local-variable 'mm/current-msg)

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
	;; if so, mark it as non-new and read
	(mm/proc-flag-msg docid "+S-u-N")))))

;; Interactive functions

(defun mm/view-quit-buffer ()
  "Quit the message view and return to the headers."
  (interactive)
  (mm/view-mark-as-read-maybe)
  (let ((inhibit-read-only t))
    (kill-buffer)
    (switch-to-buffer mm/hdrs-buffer)))

(defun mm/view-next ()
  "View the next message."
  (interactive)
  (mm/view-mark-as-read-maybe)
  (with-current-buffer mm/hdrs-buffer
    (when (mm/next-header)
      (mm/hdrs-view))))

(defun mm/view-prev ()
  "View the previous message."
  (interactive)
  (mm/view-mark-as-read-maybe)
  (with-current-buffer mm/hdrs-buffer
    (when (mm/prev-header)
      (mm/hdrs-view))))

(defun mm/view-mark-for-trash ()
  "Mark the viewed message to be moved to the trash folder."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (when (mm/mark-for-trash)
      (mm/hdrs-view))))

(defun mm/view-mark-for-delete ()
  "Mark the viewed message to be deleted."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (when (mm/mark-for-trash)
      (mm/hdrs-view))))

(defun mm/view-mark-for-move ()
  "Mark the viewed message to be moved to some folder."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (when (mm/mark-for-move)
      (mm/view-next))))

(defun mm/view-unmark ()
  "Warn user that unmarking only works in the header list."
  (interactive)
  (message "Unmarking needs to be done in the header list view"))


(defun mm/view-marked-execute ()
  "Warn user that execution can only take place in n the header
list."
  (interactive)
  (message "Execution needs to be done in the header list view"))


(provide 'mm-view)
