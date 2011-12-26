;;; mu4e-main.el -- part of mm, the mu mail user agent
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
(defconst mu4e-main-buffer-name "*mu4e-main*"
  "*internal* Name of the mm main buffer.")

(defvar mu4e-main-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "b" 'mu4e-search-bookmark)
    (define-key map "s" 'mu4e-search)
    (define-key map "q" 'mu4e-quit)
    (define-key map "j" 'mu4e-jump-to-maildir)
    (define-key map "C" 'mu4e-compose-new)

    (define-key map "m" 'mu4e-toggle-mail-sending-mode)
    (define-key map "f" 'smtpmail-send-queued-mail)
    (define-key map "U" 'mu4e-retrieve-mail-update-db)

    (define-key map "H" 'mu4e-display-manual)
    map)
  
  "Keymap for the *mu4e-main* buffer.")
(fset 'mu4e-main-mode-map mu4e-main-mode-map)

(defun mu4e-main-mode ()
  "Major mode for the mm main screen."
  (interactive)

  (kill-all-local-variables)
  (use-local-map mu4e-main-mode-map)

  (setq
    major-mode 'mu4e-main-mode
    mode-name "mu4e"
    truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))



(defun mu4e-action-str (str &optional func)
  "Highlight the first occurence of [..] in STR. Also, optionally
set FUNC to be called with the STR is clicked."
  (let ((newstr
	  (replace-regexp-in-string
	    "\\[\\(\\w+\\)\\]" 
	    (lambda(m)
	      (format "[%s]"
		(propertize (match-string 1 str) 'face 'mu4e-highlight-face))) str))
	 (map (make-sparse-keymap)))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "\\w" newstr)
      (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))


(defun mu4e-main-view()
  "Show the mm main view."
  (let ((buf (get-buffer-create mu4e-main-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
	"* "
	(propertize "mu4e - mu for emacs version " 'face 'mu4e-title-face)
	(propertize  mu4e-mu-version 'face 'mu4e-view-header-key-face)
	"\n\n"
	(propertize "  Basics\n\n" 'face 'mu4e-title-face)
	(mu4e-action-str "\t* [j]ump to some maildir\n" 'mu4e-jump-to-maildir)
	(mu4e-action-str "\t* enter a [s]earch query\n" 'mu4e-search)
	(mu4e-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new)
	"\n"
	(propertize "  Bookmarks\n\n" 'face 'mu4e-title-face)
	(mapconcat
	  (lambda (bm)
	    (let* ((query (nth 0 bm)) (title (nth 1 bm)) (key (nth 2 bm)))
	      (mu4e-action-str
		(concat "\t* [b" (make-string 1 key) "] " title))))
	  mu4e-bookmarks "\n")

	"\n"
	(propertize "  Misc\n\n" 'face 'mu4e-title-face)

	(mu4e-action-str "\t* [U]pdate email & database\n"
	  'mu4e-retrieve-mail-update-db)
	
	;; show the queue functions if `smtpmail-queue-dir' is defined
	(if smtpmail-queue-dir
	  (concat 
	    (mu4e-action-str "\t* toggle [m]ail sending mode "
	      'mu4e-toggle-mail-sending-mode)
	    "(" (propertize (if smtpmail-queue-mail "queued" "direct")
		  'face 'mu4e-view-header-key-face) ")\n"
	    (mu4e-action-str "\t* [f]lush queued mail\n"
	      'smtpmail-send-queued-mail))
	  "") 
	"\n"
    
	(mu4e-action-str "\t* [H]elp\n" 'mu4e-display-manual)
	(mu4e-action-str "\t* [q]uit\n" 'mu4e-quit))
      (mu4e-main-mode)
      (switch-to-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun mu4e-retrieve-mail-update-db ()
  "Get new mail and update the database."
  (interactive)
  (mu4e-proc-retrieve-mail-update-db))

(defun mu4e-toggle-mail-sending-mode ()
  "Toggle sending mail mode, either queued or direct."
  (interactive)
  (setq smtpmail-queue-mail (not smtpmail-queue-mail))
  (message
    (if smtpmail-queue-mail
      "Outgoing mail will now be queued"
      "Outgoing mail will now be sent directly"))
  (mu4e-main-view))

(defun mu4e-display-manual ()
  "Display the mu4e manual info pages."
  (interactive)
  (info-display-manual "mu4e"))


(defun mu4e-quit()
  "Quit the mm session."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit? ")
    (message nil)
    (mu4e-kill-proc)
    (kill-buffer)))

(provide 'mu4e-main)
