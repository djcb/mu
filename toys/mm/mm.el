;;; mm.el -- part of mm, the mu mail user agent
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

(add-to-list 'load-path "/home/djcb/Sources/mu/toys/mm")

(require 'mm-hdrs)
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
  '((t :inherit font-lock-comment-face :italic t))
  "Face for an mm message header that has been moved from the
search results."
  :group 'mm/faces)

(defface mm/header-face
  '((t :inherit default))
  "Face for an mm header without any special flags."
  :group 'deft-faces)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME
(setq
  mm/maildir       "/home/djcb/Maildir"
  mm/inbox-folder  "/inbox"
  mm/outbox-folder "/outbox"
  mm/sent-folder   "/sent"
  mm/drafts-folder "/drafts"
  mm/trash-folder  "/trash")

(defvar mm/working-folders nil)

(setq mm/working-folders
  '("/bulk" "/archive" "/bulkarchive" "/todo"))

(setq mm/header-fields
  '( (:date          .  25)
     (:flags         .   6)
     (:from          .  22)
     (:subject       .  40)))

;;; my stuff
(setq mm/mu-binary "/home/djcb/Sources/mu/src/mu")
(setq mm/mu-home   "/home/djcb/.mu")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mm)
