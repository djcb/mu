;;; mu4e-contrib.el --- User-contributed functions -*- lexical-binding: t -*-

;; Copyright (C) 2013-2023 Dirk-Jan C. Binnema

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

;; Some user-contributed functions for mu4e

;;; Code:

(require 'mu4e-headers)
(require 'mu4e-view)
(require 'bookmark)
(require 'eshell)


;;; Various simple commands
(defun mu4e-headers-mark-all-unread-read ()
  "Put a ! \(read) mark on all visible unread messages."
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'read nil)
   (lambda (msg _param)
     (memq 'unread (mu4e-msg-field msg :flags)))))

(defun mu4e-headers-flag-all-read ()
  "Flag all visible messages as \"read\"."
  (interactive)
  (mu4e-headers-mark-all-unread-read)
  (mu4e-mark-execute-all t))

(defun mu4e-headers-mark-all ()
  "Mark all headers for some action.
Ask user what action to execute."
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'something nil)
   (lambda (_msg _param) t))
  (mu4e-mark-execute-all))



;;; Bogofilter/SpamAssassin
;;
;; Support for handling spam with Bogofilter with the possibility
;; to define it for SpamAssassin, contributed by Gour.
;;
;; To add the actions to the menu, you can use something like:
;;
;; (add-to-list 'mu4e-headers-actions
;;              '("sMark as spam" . mu4e-register-msg-as-spam) t)
;; (add-to-list 'mu4e-headers-actions
;;              '("hMark as ham" . mu4e-register-msg-as-ham) t)

(defvar mu4e-register-as-spam-cmd nil
  "Command for invoking spam processor to register message as spam.
For example for bogofilter, use \"/usr/bin/bogofilter -Ns < %s\"")

(defvar mu4e-register-as-ham-cmd nil
  "Command for invoking spam processor to register message as ham.
For example for bogofile, use \"/usr/bin/bogofilter -Sn < %s\"")

(defun mu4e-register-msg-as-spam (msg)
  "Register MSG  as spam."
  (interactive)
  (let* ((path (shell-quote-argument (mu4e-message-field msg :path)))
         (command (format mu4e-register-as-spam-cmd path)))
    (shell-command command))
  (mu4e-mark-at-point 'delete nil))

(defun mu4e-register-msg-as-ham (msg)
  "Register MSG as ham."
  (interactive)
  (let* ((path (shell-quote-argument(mu4e-message-field msg :path)))
         (command (format mu4e-register-as-ham-cmd path)))
    (shell-command command))
  (mu4e-mark-at-point 'something nil))

;; (add-to-list 'mu4e-view-actions
;;              '("sMark as spam" . mu4e-view-register-msg-as-spam) t)
;; (add-to-list 'mu4e-view-actions
;;              '("hMark as ham" . mu4e-view-register-msg-as-ham) t)

(defun mu4e-view-register-msg-as-spam (msg)
  "Register MSG as spam (view mode)."
  (interactive)
  (let* ((path (shell-quote-argument (mu4e-message-field msg :path)))
         (command (format mu4e-register-as-spam-cmd path)))
    (shell-command command))
  (mu4e-view-mark-for-delete))

(defun mu4e-view-register-msg-as-ham (msg)
  "Mark MSG as ham (view mode)."
  (interactive)
  (let* ((path (shell-quote-argument(mu4e-message-field msg :path)))
         (command (format mu4e-register-as-ham-cmd path)))
    (shell-command command))
  (mu4e-view-mark-for-something))


;;; Eshell functions
;;
;; Code for `gnus-dired-attached' modified to run from eshell,
;; allowing files to be attached to an email via mu4e using the
;; eshell.  Does not depend on gnus.


(defun mu4e--active-composition-buffers ()
  "Return all active mu4e composition buffers."
  (let (buffers)
    (save-excursion
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (eq major-mode 'mu4e-compose-mode)
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))



;; backward compat until 27.1 is univeral.
(defalias 'mu4e--flatten-list
  (if (fboundp 'flatten-list)
      #'flatten-list
    (with-no-warnings
      #'eshell-flatten-list)))

;; backward compat ntil 28.1 is universal.
(defalias 'mu4e--mm-default-file-type
  (if (fboundp 'mm-default-file-type)
      #'mm-default-file-type
    (with-no-warnings
      #'mm-default-file-encoding)))

(defun eshell/mu4e-attach (&rest args)
  "Attach files to a mu4e message using eshell with ARGS.
If no mu4e buffers found, compose a new message and then attach
the file."
  (let ((destination nil)
        (files-str nil)
        (bufs nil)
        ;; Remove directories from the list
        (files-to-attach
         (delq nil (mapcar
                    (lambda (f) (if (or (not (file-exists-p f))
                                        (file-directory-p f))
                                    nil
                                  (expand-file-name f)))
                    (mu4e--flatten-list (reverse args))))))
    ;; warn if user tries to attach without any files marked
    (if (null files-to-attach)
        (error "No files to attach")
      (setq files-str
            (mapconcat
             (lambda (f) (file-name-nondirectory f))
             files-to-attach ", "))
      (setq bufs (mu4e--active-composition-buffers))
      ;; set up destination mail composition buffer
      (if (and bufs
               (y-or-n-p "Attach files to existing mail composition buffer? "))
          (setq destination
                (if (= (length bufs) 1)
                    (get-buffer (car bufs))
                  (let ((prompt (mu4e-format "%s" "Attach to buffer")))
                    (substring-no-properties
                     (funcall mu4e-completing-read-function prompt
                              bufs)))))
        ;; setup a new mail composition buffer
        (if (y-or-n-p "Compose new mail and attach this file? ")
            (progn (mu4e-compose-new)
                   (setq destination (current-buffer)))))
      ;; if buffer was found, set buffer to destination buffer, and attach files
      (if (not (eq destination 'nil))
          (progn (set-buffer destination)
                 (goto-char (point-max)) ; attach at end of buffer
                 (while files-to-attach
                   (mml-attach-file (car files-to-attach)
                                    (or (mu4e--mm-default-file-type
                                         (car files-to-attach))
                                        "application/octet-stream") nil)
                   (setq files-to-attach (cdr files-to-attach)))
                 (message "Attached file(s) %s" files-str))
        (message "No buffer to attach file to.")))))

;;; _
(provide 'mu4e-contrib)
;;; mu4e-contrib.el ends here
