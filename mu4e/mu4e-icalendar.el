;;; mu4e-icalendar.el --- reply to iCalendar meeting requests

;;; Commentary:

;; To install:
;; (require 'mu4e-icalendar)
;; (mu4e-icalendar-setup)

;; To enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
;; (require 'org-agenda)
;; (setq gnus-icalendar-org-capture-file "~/org/notes.org")
;; (setq gnus-icalendar-org-capture-headline '("Calendar"))
;; (gnus-icalendar-org-setup)

(require 'mu4e)
(require 'gnus-icalendar)

(eval-when-compile (require 'cl))


(defun mu4e-icalendar-setup ()
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
  "Reply to a text/calendar event."
  ;; Based on `gnus-icalendar-reply'.
  (let* ((handle (car data))
         (status (cadr data))
         (event (caddr data))
         (gnus-icalendar-additional-identities mu4e-user-mail-address-list)
         (reply (gnus-icalendar-with-decoded-handle handle
                  (gnus-icalendar-event-reply-from-buffer
                   (current-buffer) status (gnus-icalendar-identities))))
         (msg (mu4e-message-at-point 'noerror)))

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
          (mu4e-icalendar-reply-ical msg event status (buffer-name)))

        ;; Back in article buffer
        (setq-local gnus-icalendar-reply-status status)
        (when gnus-icalendar-org-enabled-p
          (gnus-icalendar--update-org-event event status))
        (when mu4e-icalendar-diary-file
          (mu4e~icalendar-insert-diary event status mu4e-icalendar-diary-file))
        ;; refresh article buffer to update the reply status
        (with-current-buffer mu4e~headers-buffer-name
          (mu4e-headers-rerun-search))))))

(defun mu4e~icalendar-delete-citation ()
  (delete-region (point-min) (point-max)))

(defun mu4e-icalendar-reply-ical (original-msg event status buffer-name)
  (let ((message-signature nil))
    (let ((mu4e-compose-cite-function #'mu4e~icalendar-delete-citation)
          (mu4e-sent-messages-behavior 'delete)
          (mu4e-compose-reply-recipients 'sender))
      (mu4e~compose-handler 'reply original-msg))
    ;; Make sure the recipient is the organizer
    (let ((organizer (gnus-icalendar-event:organizer event)))
      (unless (string= organizer "")
        (message-goto-to)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "To: " organizer)))
    (message-goto-body)
    (insert "\n\n")
    (mml-insert-multipart "alternative")
    (mml-insert-part "text/plain")
    (let ((reply-event (gnus-icalendar-event-from-buffer
                        buffer-name mu4e-user-mail-address-list)))
      (insert (gnus-icalendar-event->gnus-calendar reply-event status)))
    (forward-line 1); move past closing tag
    (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=utf-8")
    (message-goto-subject)
    (delete-region (line-beginning-position) (line-end-position))
    (insert "Subject: " (capitalize (symbol-name status))
            ": " (gnus-icalendar-event:summary event))
;    (message-send-and-exit)
    ))


(defun mu4e~icalendar-insert-diary (event reply-status filename)
  "Insert a diary entry for the EVENT with reply STATUS in FILE."
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
