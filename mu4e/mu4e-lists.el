;;; mu4e-lists.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2011-2016 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

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

;; In this file, we create a table of list-id -> shortname for mailing lists.
;; The shortname (friendly) should a at most 8 characters, camel-case

;;; Code:

(defvar mu4e~mailing-lists
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
  "AList of cells (MAILING-LIST-ID . SHORTNAME)")

(defcustom mu4e-user-mailing-lists nil
  "An alist with cells (MAILING-LIST-ID . SHORTNAME); these are
used in addition to the built-in list `mu4e~mailing-lists'."
  :group 'mu4e-headers
  :type '(repeat (cons string string)))


(defcustom mu4e-mailing-list-patterns nil
  "A list of regex patterns to capture a shortname out of a list
ID. For the first regex that matches, its first matchgroup will
be used as the shortname."
  :group 'mu4e-headers
  :type '(repeat (regexp)))

;;; _
(provide 'mu4e-lists)
;;; mu4e-lists.el ends here
