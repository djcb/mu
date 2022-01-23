;;; mu4e-lists.el -- part of mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2021 Dirk-Jan C. Binnema

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
  '( ("bbdb-info.lists.sourceforge.net"                       . "BBDB")
     ("boost-announce.lists.boost.org"                        . "BoostA")
     ("boost-interest.lists.boost.org"                        . "BoostI")
     ("conkeror.mozdev.org"                                   . "Conkeror")
     ("curl-library.cool.haxx.se"                             . "LibCurl")
     ("crypto-gram-list.schneier.com "                        . "CryptoGr")
     ("dbus.lists.freedesktop.org"                            . "DBus")
     ("desktop-devel-list.gnome.org"                          . "GnomeDT")
     ("discuss-webrtc.googlegroups.com"                       . "WebRTC")
     ("emacs-devel.gnu.org"                                   . "EmacsDev")
     ("emacs-orgmode.gnu.org"                                 . "Orgmode")
     ("emms-help.gnu.org"                                     . "Emms")
     ("enlightenment-devel.lists.sourceforge.net"             . "E-Dev")
     ("erlang-questions.erlang.org"                           . "Erlang")
     ("evolution-hackers.lists.ximian.com"                    . "EvoDev")
     ("farsight-devel.lists.sourceforge.net"                  . "Farsight")
     ("mailman.lists.freedesktop.org"                         . "FDeskTop")
     ("gcc-help.gcc.gnu.org"                                  . "Gcc")
     ("gmime-devel-list.gnome.org"                            . "GMimeDev")
     ("gnome-shell-list.gnome.org"                            . "GnomeSh")
     ("gnu-emacs-sources.gnu.org"                             . "EmacsSrc")
     ("gnupg-users.gnupg.org"                                 . "GnupgU")
     ("gpe.handhelds.org"                                     . "GPE")
     ("gstreamer-devel.lists.freedesktop.org"                 . "GstDev")
     ("gstreamer-devel.lists.sourceforge.net"                 . "GstDev")
     ("gstreamer-openmax.lists.sourceforge.net"               . "GstOmx")
     ("gtk-devel-list.gnome.org"                              . "GtkDev")
     ("gtkmm-list.gnome.org"                                  . "GtkmmDev")
     ("guile-devel.gnu.org"                                   . "GuileDev")
     ("guile-user.gnu.org"                                    . "GuileUsr")
     ("help-gnu-emacs.gnu.org"                                . "EmacsUsr")
     ("lggdh-algemeen.vvtp.tudelft.nl"                        . "LGGDH")
     ("linux-bluetooth.vger.kernel.org"                       . "Bluez")
     ("maemo-developers.maemo.org"                            . "MaemoDev")
     ("maemo-users.maemo.org"                                 . "MaemoUsr")
     ("monit-general.nongnu.org"                              . "Monit")
     ("mu-discuss.googlegroups.com"                           . "Mu")
     ("nautilus-list.gnome.org"                               . "Nautilus")
     ("notmuch.notmuchmail.org"                               . "Notmuch")
     ("orbit-list.gnome.org"                                  . "ORBit")
     ("pulseaudio-discuss.lists.freedesktop.org"              . "PulseA")
     ("sqlite-announce.sqlite.org"                            . "SQliteAnn")
     ("sqlite-dev.sqlite.org"                                 . "SQLiteDev")
     ("sup-talk.rubyforge.org"                                . "Sup")
     ("sylpheed-claws-users.lists.sourceforge.net"            . "Sylpheed")
     ("tinymail-devel-list.gnome.org"                         . "Tinymail")
     ("unicode.sarasvati.unicode.org"                         . "Unicode")
     ("xapian-discuss.lists.xapian.org"                       . "Xapian")
     ("xdg.lists.freedesktop.org"                             . "XDG")
     ("wl-en.lists.airs.net"                                  . "Wdrlust")
     ("wl-en.ml.gentei.org"                                   . "WdrLust")
     ("xapian-devel.lists.xapian.org"                         . "Xapian")
     ("zsh-users.zsh.org"                                     . "ZshUsr"))
  "AList of cells (MAILING-LIST-ID . SHORTNAME).")

(defcustom mu4e-user-mailing-lists nil
  "An alist with cells (MAILING-LIST-ID . SHORTNAME).
These are used in addition to the built-in list `mu4e-mailing-lists'."
  :group 'mu4e-headers
  :type '(repeat (cons string string)))

(defcustom mu4e-mailing-list-patterns nil
  "A list of regexps to capture a shortname out of a list-id.
For the first regex that matches, its first matchgroup will be
used as the shortname."
  :group 'mu4e-headers
  :type '(repeat (regexp)))


(defvar mu4e--lists-hash nil
  "Hashtable of mailing-list-id => shortname.
Based on `mu4e-mailing-lists' and `mu4e-user-mailing-lists'.")


(defun mu4e-get-mailing-list-shortname (list-id)
  "Get the shortname for a mailing-list with list-id LIST-ID.
Based on `mu4e-mailing-lists', `mu4e-user-mailing-lists', and
`mu4e-mailing-list-patterns'."
  (unless mu4e--lists-hash
    (setq mu4e--lists-hash (make-hash-table :test 'equal))
    (dolist (cell mu4e-mailing-lists)
      (puthash (car cell) (cdr cell) mu4e--lists-hash))
    (dolist (cell mu4e-user-mailing-lists)
      (puthash (car cell) (cdr cell) mu4e--lists-hash)))
  (or
   (gethash list-id mu4e--lists-hash)
   (and (boundp 'mu4e-mailing-list-patterns)
        (seq-drop-while
         (lambda (pattern)
           (not (string-match pattern list-id)))
         mu4e-mailing-list-patterns)
        (match-string 1 list-id))
   ;; if it's not in the db, take the part until the first dot if there is one;
   ;; otherwise just return the whole thing
   (if (string-match "\\([^.]*\\)\\." list-id)
       (match-string 1 list-id)
     list-id)))
;;; _
(provide 'mu4e-lists)
;;; mu4e-lists.el ends here
