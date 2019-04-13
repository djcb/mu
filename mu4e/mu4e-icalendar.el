;;; mu4e-icalendar.el --- reply to iCalendar meeting requests

;;; Commentary:

;; To install:
;; (require 'gnus-icalendar)
;; (gnus-icalendar-setup)

;; to enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
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
        `(("Accept" mu4e-icalendar-reply (,handle accepted ,event))
          ("Tentative" mu4e-icalendar-reply (,handle tentative ,event))
          ("Decline" mu4e-icalendar-reply (,handle declined ,event)))
      (cl-call-next-method event handle))))

(defun mu4e-icalendar-reply (data)
  "Reply to a text/calendar event."
  ;; Based on `gnus-icalendar-reply'.
  (let* ((handle (car data))
         (status (cadr data))
         (event (caddr data))
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
          (let* ((subject (concat (capitalize (symbol-name status))
                                 ": " (gnus-icalendar-event:summary event)))
                 (reply-event (gnus-icalendar-event-from-buffer
                               (buffer-name) mu4e-user-mail-address-list))
                 (body (gnus-icalendar-event->gnus-calendar reply-event
                                                            status)))
            (mu4e-icalendar-reply-with-buffer msg subject body (buffer-name))))

          ;; Back in article buffer
          (setq-local gnus-icalendar-reply-status status)
          (when gnus-icalendar-org-enabled-p
            (gnus-icalendar--update-org-event event status)
            ;; refresh article buffer to update the reply status
            (with-current-buffer mu4e~headers-buffer-name
              (mu4e-headers-rerun-search)))))))

(defun mu4e~icalendar-delete-citation ()
  (delete-region (point-min) (point-max)))

(defun mu4e-icalendar-reply-with-buffer (original-msg subject body buffer-name)
  (let ((message-signature nil))
    (let ((mu4e-compose-cite-function #'mu4e~icalendar-delete-citation)
          (mu4e-sent-messages-behavior 'delete)
          (mu4e-compose-reply-recipients 'sender))
      ;; FIXME: only reply to the original sender (do not ask)
      (mu4e~compose-handler 'reply original-msg))
    (message-goto-body)
    (insert "\n\n")
    (insert body)
    (mml-insert-multipart "alternative")
    (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=UTF-8")
    (message-goto-subject)
    (delete-region (line-beginning-position) (line-end-position))
    (insert "Subject: " subject)
;    (message-send-and-exit)
    ))



(provide 'mu4e-icalendar)
;;; mu4e-icalendar.el ends here
