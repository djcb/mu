;;; mua-common.el -- part of mua, the mu mail user agent
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

;; mua-common contains common utility functions for mua

;;; Code:
 
(eval-when-compile (require 'cl))

(defconst mua/log-buffer-name "*mua-log*" "name of the logging buffer")

(defun mua/warn (frm &rest args)
  "warn user in echo-area, return nil"
  (let ((str (apply 'format frm args)))
    (message str)
    nil))

(defun mua/log (frm &rest args)
  "write something in the *mua-log* buffer - mainly useful for debugging"
  (with-current-buffer (get-buffer-create mua/log-buffer-name)
    (goto-char (point-max))
    (insert (apply 'format (concat (format-time-string "%x %X " (current-time))
			     frm "\n") args))))

(defun mua/warn-and-log (frm &rest args)
  "log and warn (ie., mua/warn + mua/log); return nil"
  (apply 'mua/log frm args)
  (apply 'mua/warn frm args)
  nil)

(defun mua/new-buffer (bufname)
  "return a new buffer BUFNAME; if such already exists, kill the
old one first"
  (when (get-buffer bufname)
    (kill-buffer bufname))
  (get-buffer-create bufname))

(defun mua/message (frm &rest args)
  "print a mua message at point"
  (let ((str (apply 'format frm args)) (inhibit-read-only t))
    (insert (propertize str 'face 'italic))))

(defun mua/quit-buffer ()
  "kill this buffer, and switch to it's parentbuf if it is alive"
  (interactive)  
  (let ((parentbuf mua/parent-buffer))
    (kill-buffer)
    (when (and parentbuf (buffer-live-p parentbuf))
      (switch-to-buffer parentbuf))))

(defun mua/ask-maildir (prompt &optional fullpath)
  "ask user with PROMPT for a maildir name, if fullpath is
non-nill, return the fulpath (ie, mu-maildir prepended to the
maildir"
  (interactive)
  (let* ((showfolders
	   (delete-dups
	     (append (list mua/inbox-folder mua/sent-folder)
	       mua/working-folders)))
	  (chosen (ido-completing-read prompt showfolders)))
    (concat (if fullpath mua/maildir "") chosen)))

(defun mua/mu-binary-version ()
  "Get the version of the mu binary."
  (let ((cmd (concat mua/mu-binary
	       " --version | head -1 | sed 's/.*version //'")))
    (substring (shell-command-to-string cmd) 0 -1)))



(provide 'mua-common)
