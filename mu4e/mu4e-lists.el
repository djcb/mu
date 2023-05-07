;;; mu4e-lists.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2023 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file, we create a table of list-id -> shortname for mailing lists.
;; The shortname (friendly) should a at most 8 characters, camel-case

;;; Code:

;;; Configuration
(defvar mu4e-mailing-lists
  '( (:list-id "bbdb-info.lists.sourceforge.net"         :name "BBDB")
     (:list-id "boost-announce.lists.boost.org"          :name "BoostA")
     (:list-id "boost-interest.lists.boost.org"          :name "BoostI")
     (:list-id "curl-library.cool.haxx.se"               :name "LibCurl")
     (:list-id "dbus.lists.freedesktop.org"              :name "DBus")
     (:list-id "desktop-devel-list.gnome.org"            :name "GnomeDT")
     (:list-id "discuss-webrtc.googlegroups.com"         :name "WebRTC")
     (:list-id "emacs-devel.gnu.org"                     :name "EmacsDev")
     (:list-id "emacs-orgmode.gnu.org"                   :name "Orgmode")
     (:list-id "emms-help.gnu.org"                       :name "Emms")
     (:list-id "evolution-hackers.lists.ximian.com"      :name "EvoDev")
     (:list-id "mailman.lists.freedesktop.org"           :name "FDeskTop")
     (:list-id "gcc-help.gcc.gnu.org"                    :name "Gcc")
     (:list-id "gmime-devel-list.gnome.org"              :name "GMimeDev")
     (:list-id "gnome-shell-list.gnome.org"              :name "GnomeSh")
     (:list-id "gnu-emacs-sources.gnu.org"               :name "EmacsSrc")
     (:list-id "gnupg-users.gnupg.org"                   :name "GnupgU")
     (:list-id "gstreamer-devel.lists.freedesktop.org"   :name "GstDev")
     (:list-id "gtk-devel-list.gnome.org"                :name "GtkDev")
     (:list-id "gtkmm-list.gnome.org"                    :name "GtkmmDev")
     (:list-id "guile-devel.gnu.org"                     :name "GuileDev")
     (:list-id "guile-user.gnu.org"                      :name "GuileUsr")
     (:list-id "help-gnu-emacs.gnu.org"                  :name "EmacsUsr")
     (:list-id "mu-discuss.googlegroups.com"             :name "Mu")
     (:list-id "nautilus-list.gnome.org"                 :name "Nautilus")
     (:list-id "notmuch.notmuchmail.org"                 :name "Notmuch")
     (:list-id "sqlite-announce.sqlite.org"              :name "SQliteAnn")
     (:list-id "sqlite-dev.sqlite.org"                   :name "SQLiteDev")
     (:list-id "xapian-discuss.lists.xapian.org"         :name "Xapian")
     (:list-id "xdg.lists.freedesktop.org"               :name "XDG")
     (:list-id "wl-en.lists.airs.net"                    :name "Wdrlust")
     (:list-id "wl-en.ml.gentei.org"                     :name "WdrLust")
     (:list-id "xapian-devel.lists.xapian.org"           :name "Xapian")
     (:list-id "zsh-users.zsh.org"                       :name "ZshUsr"))
  "List of plists with keys:
     - `:list-id'       - the mailing list id
     - `:name'          - the display name.")

(defgroup mu4e-lists nil "Configuration for mailing lists."
  :group 'mu4e)

(defcustom mu4e-user-mailing-lists nil
  "A list with plists like `mu4e-mailing-lists'.
These are used in addition to the built-in list
`mu4e-mailing-lists'.

The older format, a list of cons cells,
  (LIST-ID . NAME)
is still supported for backward compatibility.

After changing, use `mu4e-mailing-list-info-refresh' to make mu4e
use the new values."
  :group 'mu4e-headers
  :type '(repeat (plist)))

(defcustom mu4e-mailing-list-patterns '("\\([^.]*\\)\\.")
  "A list of regexps to capture a shortname out of a list-id.
For the first regex that matches, its first match-group will be
used as the shortname."
  :group 'mu4e-headers
  :type '(repeat (regexp)))

(defvar mu4e--lists-hash nil
  "Hash-table of list-id => plist.
Based on `mu4e-mailing-lists' and `mu4e-user-mailing-lists'.")

(defun mu4e-mailing-list-info-refresh ()
  "Refresh the mailing list info.
Based on the current value of `mu4e-mailing-lists' and
`mu4e-user-mailing-lists'."
  (interactive)
  (setq mu4e--lists-hash (make-hash-table :test 'equal))
  (seq-do (lambda (item)
            (if (plistp item)
                ;; the new format
                (puthash (plist-get item :list-id) item mu4e--lists-hash)
              ;; backward compatibility
              (puthash (car item) (cdr item) mu4e--lists-hash)))
          (append mu4e-mailing-lists
                  mu4e-user-mailing-lists)))

(defun mu4e-mailing-list-info (list-id)
  "Get mailing list info for LIST-ID.
Return nil if not found."
  (unless mu4e--lists-hash (mu4e-mailing-list-info-refresh))
  (gethash list-id mu4e--lists-hash))


(defun mu4e-get-mailing-list-shortname (list-id)
  "Get the shortname for a mailing-list with list-id LIST-ID.
Either we know about this mailing list, or otherwise
we guess one."
  (or  ;; 1. perhaps we have it in one of our lists?
   (plist-get (mu4e-mailing-list-info list-id) :name)
   ;;     2. see if it matches some pattern
   (if (seq-find (lambda (p) (string-match p list-id))
                 mu4e-mailing-list-patterns)
       (match-string 1 list-id)
     ;;   3. otherwise, just return the whole thing
     list-id)))

(provide 'mu4e-lists)
;;; mu4e-lists.el ends here
