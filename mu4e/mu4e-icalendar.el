;;; mu4e-icalendar.el --- iCalendar & diary integration -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Christophe Troestler

;; Author: Christophe Troestler <Christophe.Troestler@umons.ac.be>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email icalendar
;; Version: 0.0

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

;; To install:
;; (require 'mu4e-icalendar)
;; (gnus-icalendar-setup)
;; Optional:
;; (setq mu4e-icalendar-trash-after-reply t)

;; By default, the original message is not cited.  However, if you
;; would like to reply to it, the citation is in the kill-ring (paste
;; it with `yank').

;; To add the event to a diary file of your choice:
;; (setq mu4e-icalendar-diary-file "/path/to/your/diary")
;; If the file specified is not your main diary file, add
;; #include "/path/to/your/diary"
;; to you main diary file to display the events.

;; To enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
;; (require 'org-agenda)
;; (setq gnus-icalendar-org-capture-file "~/org/notes.org")
;; (setq gnus-icalendar-org-capture-headline '("Calendar"))
;; (gnus-icalendar-org-setup)

;;; Code:

(require 'gnus-icalendar)
(require 'cl-lib)

(require 'mu4e-mark)
(require 'mu4e-helpers)
(require 'mu4e-contacts)
(require 'mu4e-headers)
(require 'mu4e-obsolete)


;;; Configuration
;;;; Calendar

(defgroup mu4e-icalendar nil
  "Icalendar related settings."
  :group 'mu4e)

;; (defcustom mu4e-icalendar-trash-after-reply nil
;;   "If non-nil, trash the icalendar invitation after replying."
;;   :type 'boolean
;;   :group 'mu4e-icalendar)

(defcustom mu4e-icalendar-diary-file nil
  "If non-nil, the file in which to add events upon reply."
  :type '(choice (const :tag "Do not insert a diary entry" nil)
                 (string :tag "Insert a diary entry in this file"))
  :group 'mu4e-icalendar)


(defun mu4e--icalendar-has-email (email list)
  "Check that EMAIL is in LIST."
  (let ((email (downcase email)))
    (cl-find-if (lambda (c) (let ((e (mu4e-contact-email c)))
                              (and (stringp e) (string= email (downcase e)))))
                list)))

(declare-function mu4e--view-mode-p "mu4e-view")
(defun mu4e--icalendar-reply (orig data)
  "Wrapper for using either `mu4e-icalender-reply' or the ORIG function."
  (funcall (if (mu4e--view-mode-p) #'mu4e-icalendar-reply orig) data))

(advice-add  #'gnus-icalendar-reply :around #'mu4e--icalendar-reply)
;;(advice-remove #'gnus-icalendar-reply #'mu4e--icalendar-reply)

(defun mu4e-icalendar-reply (data)
  "Reply to the text/calendar event present in DATA."
  ;; Based on `gnus-icalendar-reply'.
  (let* ((handle (car data))
         (status (cadr data))
         (event (caddr data))
         (gnus-icalendar-additional-identities
          (mu4e-personal-addresses 'no-regexp))
         (reply (gnus-icalendar-with-decoded-handle
                 handle
                 (gnus-icalendar-event-reply-from-buffer
                  (current-buffer) status (gnus-icalendar-identities))))
         (msg (mu4e-message-at-point 'noerror))
         (charset (cdr (assoc 'charset (mm-handle-type handle)))))
    (when reply
      (cl-labels
          ((fold-icalendar-buffer
             ()
             (goto-char (point-min))
             (while (re-search-forward "^\\(.\\{72\\}\\)\\(.+\\)$" nil t)
               (replace-match "\\1\n \\2")
               (goto-char (line-beginning-position)))))

        (let ((ical-name gnus-icalendar-reply-bufname))
          (with-current-buffer (get-buffer-create ical-name)
            (delete-region (point-min) (point-max))
            (insert reply)
            (fold-icalendar-buffer)
            (when (and charset (string= (downcase charset) "utf-8"))
              (decode-coding-region (point-min) (point-max) 'utf-8)))

          (save-excursion ;; Compose the reply message.
            (let* ((message-signature nil)
                   (organizer (gnus-icalendar-event:organizer event))
                   (organizer (when (and organizer
                                         (not (string-empty-p organizer)))
                                organizer))
                   (organizer
                    (or organizer
                        (plist-get (car (plist-get msg :reply-to)) :email)
                        (plist-get (car (plist-get msg :from)) :email)
                        (mu4e-warn "Cannot find organizer")))
                   (message-cite-function #'mu4e-message-cite-nothing))
              (mu4e-compose-reply-to organizer)
              (message-goto-body)
              (mml-insert-multipart "alternative")
              (mml-insert-empty-tag 'part 'type "text/plain")
              (mml-attach-buffer ical-name
                                 "text/calendar; method=REPLY; charset=UTF-8")
              ;; XXX: not currently supported
              ;;(when mu4e-icalendar-trash-after-reply
              ;;   ;; Override `mu4e-sent-handler' set by `mu4e-compose-mode' to
              ;;   ;; also trash the message (thus must be appended to hooks).
              ;;   (add-hook 'message-sent-hook
              ;;             (mu4e--icalendar-trash-message-hook msg) 90 t))

              (when gnus-icalendar-org-enabled-p
                (if (gnus-icalendar-find-org-event-file event)
                    (gnus-icalendar--update-org-event event status)
                  (gnus-icalendar:org-event-save event status)))
              (when mu4e-icalendar-diary-file
                (mu4e--icalendar-insert-diary event status
                                              mu4e-icalendar-diary-file)))))))))

(declare-function mu4e-view-headers-next "mu4e-view")
(defun mu4e--icalendar-trash-message (original-msg)
  "Trash the message ORIGINAL-MSG and move to the next one."
  (lambda (docid path)
    "See `mu4e-sent-handler' for DOCID and PATH."
    (mu4e-sent-handler docid path)
    (let* ((docid (mu4e-message-field original-msg :docid))
           (markdescr (assq 'trash mu4e-marks))
           (action (plist-get (cdr markdescr) :action))
           (target (mu4e-get-trash-folder original-msg)))
      (with-current-buffer (mu4e-get-headers-buffer)
        (run-hook-with-args 'mu4e-mark-execute-pre-hook 'trash original-msg)
        (funcall action docid original-msg target))
      (when (and (mu4e~headers-view-this-message-p docid)
                 (buffer-live-p (mu4e-get-view-buffer)))
        (mu4e-display-buffer (mu4e-get-view-buffer))
        (or (mu4e-view-headers-next)
            (kill-buffer-and-window))))))

;; (defun mu4e--icalendar-trash-message-hook (original-msg)
;;   "Trash the iCalendar message ORIGINAL-MSG."
;;   (lambda ()
;;     (setq mu4e-sent-func
;;           (mu4e--icalendar-trash-message original-msg))))

(defun mu4e--icalendar-insert-diary (event reply-status filename)
  "Insert a diary entry for the EVENT in file named FILENAME.
REPLY-STATUS is the status of the reply.  The possible values are
given in the doc of `gnus-icalendar-event-reply-from-buffer'."
  ;; FIXME: handle recurring events
  (let* ((beg (gnus-icalendar-event:start-time event))
         (beg-date (format-time-string "%d/%m/%Y" beg))
         (beg-time (format-time-string "%H:%M" beg))
         (end (gnus-icalendar-event:end-time event))
         (end-date (format-time-string "%d/%m/%Y" end))
         (end-time (format-time-string "%H:%M" end))
         (summary (gnus-icalendar-event:summary event))
         (location (gnus-icalendar-event:location event))
         (status (capitalize (symbol-name reply-status)))
         (txt (if location
                  (format "%s (%s)\n %s " summary status location)
                (format "%s (%s)" summary status))))
    (with-temp-buffer
      (if (string= beg-date end-date)
          (insert beg-date " " beg-time "-" end-time " " txt "\n")
        (insert beg-date " " beg-time " Start of: " txt "\n")
        (insert beg-date " " end-time " End of: " txt "\n"))
      (write-region (point-min) (point-max) filename t))))

;;;
(provide 'mu4e-icalendar)
;;; mu4e-icalendar.el ends here
