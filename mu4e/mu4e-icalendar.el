;;; mu4e-icalendar.el --- reply to iCalendar meeting requests (part of mu4e)  -*- lexical-binding: t; -*- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019- Christophe Troestler

;; Author: Christophe Troestler <Christophe.Troestler@umons.ac.be>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email icalendar
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

;; To install:
;; (require 'mu4e-icalendar)
;; (mu4e-icalendar-setup)
;; Optional
;; (setq mu4e-icalendar-trash-after-reply t)

;; To enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
;; (require 'org-agenda)
;; (setq gnus-icalendar-org-capture-file "~/org/notes.org")
;; (setq gnus-icalendar-org-capture-headline '("Calendar"))
;; (gnus-icalendar-org-setup)

;;; Code:

(require 'gnus-icalendar)
(require 'cl-lib)

;;;###autoload
(defun mu4e-icalendar-setup ()
  "Perform the necessary initialization to use mu4e-icalendar."
  (gnus-icalendar-setup)
  (cl-defmethod gnus-icalendar-event:inline-reply-buttons :around
    ((event gnus-icalendar-event) handle)
    (if (and (boundp 'mu4e~view-rendering)
             (gnus-icalendar-event:rsvp event))
        (let ((method (gnus-icalendar-event:method event)))
          (when (or (string= method "REQUEST") (string= method "PUBLISH"))
            `(("Accept" mu4e-icalendar-reply (,handle accepted ,event))
              ("Tentative" mu4e-icalendar-reply (,handle tentative ,event))
              ("Decline" mu4e-icalendar-reply (,handle declined ,event)))))
      (cl-call-next-method event handle))))

(defun mu4e-icalendar-reply (data)
  "Reply to the text/calendar event present in DATA."
  ;; Based on `gnus-icalendar-reply'.
  (let* ((handle (car data))
         (status (cadr data))
         (event (caddr data))
         (gnus-icalendar-additional-identities mu4e-user-mail-address-list)
         (reply (gnus-icalendar-with-decoded-handle handle
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

        (with-current-buffer (get-buffer-create gnus-icalendar-reply-bufname)
          (delete-region (point-min) (point-max))
          (insert reply)
          (fold-icalendar-buffer)
          (when (string= (downcase charset) "utf-8")
            (decode-coding-region (point-min) (point-max) 'utf-8))
          (mu4e-icalendar-reply-ical msg event status (buffer-name)))

        ;; Back in article buffer
        (setq-local gnus-icalendar-reply-status status)
        (when gnus-icalendar-org-enabled-p
          (gnus-icalendar--update-org-event event status))
        (when mu4e-icalendar-diary-file
          (mu4e~icalendar-insert-diary event status
                                       mu4e-icalendar-diary-file))))))

(defun mu4e~icalendar-delete-citation ()
  "Function passed to `mu4e-compose-cite-function' to remove the citation."
  (delete-region (point-min) (point-max)))

(defun mu4e~icalendar-trash-message (original-msg)
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
        (switch-to-buffer (mu4e-get-view-buffer))
        (or (mu4e-view-headers-next)
            (kill-buffer-and-window))))))

(defun mu4e-icalendar-reply-ical (original-msg event status buffer-name)
  "Reply to ORIGINAL-MSG containing invitation EVENT with STATUS.
See `gnus-icalendar-event-reply-from-buffer' for the possible
STATUS values.  BUFFER-NAME is the name of the buffer holding the
response in icalendar format."
  (let ((message-signature nil))
    (let ((mu4e-compose-cite-function #'mu4e~icalendar-delete-citation)
          (mu4e-sent-messages-behavior 'delete)
          (mu4e-compose-reply-recipients 'sender))
      (mu4e~compose-handler 'reply original-msg))
    ;; Make sure the recipient is the organizer
    (let ((organizer (gnus-icalendar-event:organizer event)))
      (unless (string= organizer "")
        (message-remove-header "To")
        (message-goto-to)
        (insert organizer)))
    ;; Not (message-goto-body) to possibly skip mll sign directive
    ;; inserted by `mu4e-compose-mode-hook':
    (goto-char (point-max))
    (mml-insert-multipart "alternative")
    (mml-insert-part "text/plain")
    (let ((reply-event (gnus-icalendar-event-from-buffer
                        buffer-name mu4e-user-mail-address-list)))
      (insert (gnus-icalendar-event->gnus-calendar reply-event status)))
    (forward-line 1); move past closing tag
    (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=utf-8")
    (message-remove-header "Subject")
    (message-goto-subject)
    (insert (capitalize (symbol-name status))
            ": " (gnus-icalendar-event:summary event))
    (set-buffer-modified-p nil); not yet modified by user
    (when mu4e-icalendar-trash-after-reply
      ;; Override `mu4e-sent-handler' set by `mu4e-compose-mode' to
      ;; also trash the message (thus must be appended to hooks).
      (add-hook
       'message-sent-hook
       (lambda () (setq mu4e-sent-func
                          (mu4e~icalendar-trash-message original-msg)))
       t t))))


(defun mu4e~icalendar-insert-diary (event reply-status filename)
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


(provide 'mu4e-icalendar)
;;; mu4e-icalendar.el ends here
