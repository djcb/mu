
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

(eval-when-compile (require 'cl))

(require 'mm-hdrs)
(require 'mm-view)
(require 'mm-send)
(require 'mm-common)
(require 'mm-proc)

;; Customization

(defgroup mm nil
  "Mm." :group 'local)


(defcustom mm/mu-home nil
  "Location of the mu homedir, or nil for the default."
  :type 'directory
  :group 'mm
  :safe 'stringp)

(defcustom mm/mu-binary "mu"
  "Name of the mu-binary to use; if it cannot be found in your
PATH, you can specifiy the full path."
  :type 'file
  :group 'mm
  :safe 'stringp)

(defcustom mm/maildir nil
  "Your Maildir directory. When `nil', mu will try to find it."
  :type 'directory
  :safe 'stringp
  :group 'mm)


(defcustom mm/get-mail-command nil
  "Shell command to run to retrieve new mail; e.g. 'offlineimap' or
'fetchmail'."
  :type 'string
  :group 'mm
  :safe 'stringp)

(defcustom mm/attachment-dir (expand-file-name "~/")
  "Default directory for saving attachments."
  :type 'string
  :group 'mm
  :safe 'stringp)

(defvar mm/debug nil
  "When set to non-nil, log debug information to the *mm-log* buffer.")


;; Folders

(defgroup mm/folders nil
  "Special folders for mm."
  :group 'mm)


(defcustom mm/inbox-folder nil
  "Your Inbox folder, relative to `mm/maildir'."
  :type 'string
  :safe 'stringp
  :group 'mm/folders)

(defcustom mm/outbox-folder nil
  "Your Outbox folder, relative to `mm/maildir'."
  :type 'string
  :safe 'stringp
  :group 'mm/folders)

(defcustom mm/sent-folder nil
  "Your folder for sent messages, relative to `mm/maildir'."
  :type 'string
  :safe 'stringp
  :group 'mm/folders)

(defcustom mm/draft-folder nil
  "Your folder for draft messages, relative to `mm/maildir'."
  :type 'string
  :safe 'stringp
  :group 'mm/folders)

(defcustom mm/trash-folder nil
  "Your folder for trashed messages, relative to `mm/maildir'."
  :type 'string
  :safe 'stringp
  :group 'mm/folders)


(defgroup mm/view nil
  "Settings for the message view."
  :group 'mm)

;; the message view

(defcustom mm/view-headers
  '(:from :to :cc :subject :flags :date :maildir :path :attachments)
  "Header fields to display in the message view buffer."
  :type (list 'symbol)
  :group 'mm/view)


;; Composing / Sending messages
(defgroup mm/compose nil
  "Customizations for composing/sending messages."
  :group 'mm)

(defcustom mm/msg-citation-prefix "> "
  "String to prefix cited message parts with."
  :type 'string
  :group 'mm/compose)

(defcustom mm/msg-reply-prefix "Re: "
  "String to prefix the subject of replied messages with."
  :type 'string
  :group 'mm/compose)

(defcustom mm/msg-forward-prefix "Fwd: "
  "String to prefix the subject of forwarded messages with."
  :type 'string
  :group 'mm/compose)

(defcustom mm/user-agent nil
  "The user-agent string; leave at `nil' for the default."
  :type 'string
  :group 'mm/compose)



;; Faces

(defgroup mm/faces nil
  "Faces used in by mm."
  :group 'mm
  :group 'faces)

(defface mm/unread-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for an unread mm message header."
  :group 'mm/faces)

(defface mm/moved-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for an mm message header that has been moved to some
folder (it's still visible in the search results, since we cannot
be sure it no longer matches)."
  :group 'mm/faces)

(defface mm/trashed-face
  '((t :inherit font-lock-comment-face :strike-though t))
  "Face for an message header in the trash folder."
  :group 'mm/faces)

(defface mm/header-face
  '((t :inherit default))
  "Face for an mm header without any special flags."
  :group 'mm/faces)

(defface mm/title-face
  '((t :inherit font-lock-type-face))
  "Face for an mm title."
  :group 'mm/faces)

(defface mm/view-header-key-face
  '((t :inherit font-lock-builtin-face))
  "Face for the header title (such as \"Subject\" in the message view)."
  :group 'mm/faces)

(defface mm/view-header-value-face
  '((t :inherit font-lock-doc-face))
  "Face for the header value (such as \"Re: Hello!\" in the message view)."
  :group 'mm/faces)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal variables / constant
(defconst mm/mm-buffer-name "*mm*"
  "*internal* Name of the mm main buffer.")

(defvar mm/mu-version nil
  "*interal* version of the mu binary")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mm mode + keybindings
(defvar mm/mm-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "I" 'mm/jump-to-inbox)
    (define-key map "T" 'mm/search-today)
    (define-key map "W" 'mm/search-last-7-days)
    (define-key map "U" 'mm/search-unread)

    (define-key map "s" 'mm/search)
    (define-key map "q" 'mm/quit-mm)
    (define-key map "j" 'mm/jump-to-maildir)
    (define-key map "c" 'mm/compose-new)

    (define-key map "r" 'mm/retrieve-mail)
    (define-key map "u" 'mm/update-database)

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
    mode-name "*mm*"
    truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defun mm()
  "Start mm."
  (interactive)
  (let ((buf (get-buffer-create mm/mm-buffer-name))
	 (inhibit-read-only t))
     (with-current-buffer buf
       (erase-buffer)
       (insert
	"* "
	 (propertize "mm - mail for emacs\n" 'face 'mm/title-face)
	 "\n"
	 "  Watcha wanna do?\n\n"
	 "    * Show me some messages:\n"
	 "      - In your " (propertize "I" 'face 'highlight) "nbox\n"
	 "      - " (propertize "U" 'face 'highlight) "nread messages\n"
	 "      - Received " (propertize "T" 'face 'highlight) "oday\n"
	 "      - Received this " (propertize "W" 'face 'highlight) "eek\n"
	 "\n"
	 "    * " (propertize "j" 'face 'highlight) "ump to a folder\n"
	 "    * " (propertize "s" 'face 'highlight) "earch for a specific message\n"
	 "\n"
	 "    * " (propertize "c" 'face 'highlight) "ompose a new message\n"
	 "\n"
	 "    * " (propertize "r" 'face 'highlight) "etrieve new mail\n"
	 "    * " (propertize "u" 'face 'highlight) "pdate the message database\n"
	 "\n"
	 "    * " (propertize "q" 'face 'highlight) "uit mm\n")

      (mm/mm-mode)
      (switch-to-buffer buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions

(defun mm/jump-to-inbox ()
  "Jump to your Inbox folder (as specified in `mm/inbox-folder')."
  (interactive)
  (mm/hdrs-search (concat "maildir:" mm/inbox-folder)))

(defun mm/search-unread ()
  "List all your unread messages."
  (interactive)
  (mm/hdrs-search "flag:unread AND NOT flag:trashed"))

(defun mm/search-today ()
  "List messages received today."
  (interactive)
  (mm/hdrs-search "date:today..now"))

(defun mm/search-last-7-days ()
  "List messages received in the last 7 days."
  (interactive)
  (mm/hdrs-search "date:7d..now"))

(defun mm/retrieve-mail ()
  "Get new mail."
  (interactive)
  (unless mm/get-mail-command
    (error "`mm/get-mail-command' is not set"))
  (when (y-or-n-p "Sure you want to retrieve new mail?")
    (shell-command mm/get-mail-command)))

(defun mm/update-database ()
  "Update the database (ie., 'mu index')."
  (interactive)
  (unless mm/maildir (error "`mm/maildir' not set"))
  (when (y-or-n-p "Sure you want to update the database?")
    (mm/proc-index mm/maildir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mm/quit-mm()
  "Quit the mm session."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit mm? ")
    (message nil)
    (mm/kill-proc)
    (kill-buffer)))


(provide 'mm)
