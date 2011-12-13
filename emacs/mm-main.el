;;; mm-main.el -- part of mm, the mu mail user agent
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

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mm main view mode + keybindings
(defconst mm/main-buffer-name "*mm*"
  "*internal* Name of the mm main view buffer.")

(defvar mm/mm-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "b" 'mm/search-bookmark)
    (define-key map "s" 'mm/search)
    (define-key map "S" 'mm/search-full)
    (define-key map "q" 'mm/quit-mm)
    (define-key map "j" 'mm/jump-to-maildir)
    (define-key map "c" 'mm/compose-new)

    (define-key map "m" 'mm/toggle-mail-sending-mode)
    (define-key map "f" 'smtpmail-send-queued-mail)
    (define-key map "u" 'mm/retrieve-mail-update-db)

    map)
  "Keymap for the *mm* buffer.")
(fset 'mm/mm-mode-map mm/mm-mode-map)

(defun mm/mm-mode ()
  "Major mode for the mm main screen."
  (interactive)

  (kill-all-local-variables)
  (use-local-map mm/mm-mode-map)

  (setq
    mm/marks-map (make-hash-table :size 16  :rehash-size 2)
    major-mode 'mm/mm-mode
    mode-name "mm: main view"
    truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defun mm/action-str (str)
  "Highlight the first occurence of [..] in STR."
  (if (string-match "\\[\\(\\w+\\)\\]" str)
    (let* ((key (match-string 1 str))
	    (keystr (propertize key 'face 'mm/highlight-face)))
      (replace-match keystr nil t str 1))
    str))


(defun mm/main-view()
  "Show the mm main view."
  (let ((buf (get-buffer-create mm/main-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
	"* "
	(propertize "mm - mu mail for emacs version " 'face 'mm/title-face)
	(propertize  mm/mu-version 'face 'mm/view-header-key-face)
	"\n\n"
	(propertize "  Basics\n\n" 'face 'mm/title-face)
	(mm/action-str "\t* [j]ump to some maildir\n")
	(mm/action-str "\t* enter a [s]earch query\n")
	(mm/action-str "\t* [c]ompose a new message\n")
	"\n"
	(propertize "  Bookmarks\n\n" 'face 'mm/title-face)
	(mapconcat
	  (lambda (bm)
	    (let* ((query (nth 0 bm)) (title (nth 1 bm)) (key (nth 2 bm)))
	      (mm/action-str
		(concat "\t* [b" (make-string 1 key) "] " title))))
	  mm/bookmarks "\n")

	"\n"
	(propertize "  Misc\n\n" 'face 'mm/title-face)
	(mm/action-str "\t* [u]pdate email & database\n")
	(mm/action-str "\t* toggle [m]ail sending mode ")
	"(" (propertize (if smtpmail-queue-mail "queued" "direct")
	      'face 'mm/view-header-key-face) ")\n"
	(mm/action-str "\t* [f]lush queued mail\n")
	"\n"
	(mm/action-str "\t* [q]uit mm\n"))
      (mm/mm-mode)
      (switch-to-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun mm/retrieve-mail-update-db ()
  "Get new mail and update the database."
  (interactive)
  (mm/proc-retrieve-mail-update-db))

(defun mm/toggle-mail-sending-mode ()
  "Toggle sending mail mode, either queued or direct."
  (interactive)
  (setq smtpmail-queue-mail (not smtpmail-queue-mail))
  (message
    (if smtpmail-queue-mail
      "Outgoing mail will now be queued"
      "Outgoing mail will now be sent directly"))
  (mm))


(defun mm/quit-mm()
  "Quit the mm session."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit mm? ")
    (message nil)
    (mm/kill-proc)
    (kill-buffer)))

(provide 'mm-main)
