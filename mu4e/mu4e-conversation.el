;;; mu4e-conversation.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2018 Dirk-Jan C. Binnema

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

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

;; In this file we define mu4e-conversation-mode (+ helper functions), which is
;; used for viewing all e-mail messages of a thread in a single buffer.

;; TODO: Use unwind-protect to set handlers?  I don't think it would work.
;; TODO: Don't hide e-mails in header buffer.
;; TODO: Create mode: read-only (or use view-mode), quit with "q", browse messages with "C-c C-n/p", reply with "r".
;; Should we reply to the selected message or to the last?  Make it an option: 'current, 'last, 'ask.
;; Binding to switch to regular view?
;; TODO: Mark visible messages as read.
;; TODO: Indent user messages?
;; TODO: Detect subject changes.
;; TODO: Trim top-posting quote.
;; TODO: Mention in manual.
;; TODO: Support fill-paragraph.  See `mu4e-view-fill-long-lines'.

(defconst mu4e~conversation-buffer-name "*mu4e-conversation*"
  "Name of the conversation view buffer.")

(defvar mu4e-conversation-my-name "Me")

(defvar mu4e~conversation-thread-headers nil)
(defvar mu4e~conversation-thread nil)
(defvar mu4e~conversation-current-message nil)

(defvar mu4e~conversation-previous-view-func nil)
(defun mu4e-conversation-view-handler (msg)
  "Handler function for displaying a message."
  (push msg mu4e~conversation-thread)
  (when (= (length mu4e~conversation-thread)
           (length mu4e~conversation-thread-headers))
    (mu4e~conversation-show)))

(defvar mu4e-conversation-print-message-function 'mu4e-conversation-print-message
  "Function that takes a message index and insert it's content in the current buffer.
The message can be retrieved from `mu4e~conversation-thread'.")

(defun mu4e~conversation-show ()
  ;; See the docstring of `mu4e-message-field-raw'.
  ;; mu4e~conversation-thread is in reverse order.
  ;; TODO: Use same windowing configuration as mu4e-view.
  (setq mu4e-view-func (or mu4e~conversation-previous-view-func
                           'mu4e~headers-view-handler))
  (switch-to-buffer (get-buffer-create mu4e~conversation-buffer-name))
  (erase-buffer)
  (setq header-line-format (mu4e-message-field (car mu4e~conversation-thread) :subject))
  (let ((current-message-pos 0)
        (index 0))
    (dolist (msg mu4e~conversation-thread)
      ;; TODO: Show pictures.
      (when (= (mu4e-message-field msg :docid)
               (mu4e-message-field mu4e~conversation-current-message :docid))
        (setq current-message-pos (point)))
      (funcall mu4e-conversation-print-message-function index)
      (setq index (1+ index)))
    (goto-char current-message-pos)))

(defface mu4e-conversation-sender-me
  '((t :inherit default))
  "Face for conversation message sent by yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-1
  '((t :background "#335533"))
  "Face for conversation message from the 1st sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-2
  '((t :background "#553333"))
  "Face for conversation message from the 2rd sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-3
  '((t :background "#333355"))
  "Face for conversation message from the 3rd sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-4
  '((t :background "#888833"))
  "Face for conversation message from the 4th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-5
  '((t :background "#4a708b"))
  "Face for conversation message from the 5th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-6
  '((t :background "#8b4500"))
  "Face for conversation message from the 6th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-7
  '((t :background "#551a8b"))
  "Face for conversation message from the 7th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-8
  '((t :background "#8b0a50"))
  "Face for conversation message from the 8th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-sender-9
  '((t :background "#00008b"))
  "Face for conversation message from the 9th sender who is not yourself."
  :group 'mu4e-faces)

(defface mu4e-conversation-header
  '((t :foreground "grey70" :background "grey25"))
  "Face for conversation message sent by someone else."
  :group 'mu4e-faces)

(defun mu4e-conversation-print-message (index)
  "Insert formatted message found at INDEX in `mu4e~conversation-thread'."
  ;; See the docstring of `mu4e-message-field-raw'.
  ;; mu4e~conversation-thread is in reverse order.
  ;; TODO: Use same windowing configuration as mu4e-view.
  (let* ((msg (nth index mu4e~conversation-thread))
         (from (car (mu4e-message-field msg :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list))
         (face-index 1)
         (sender-faces (make-hash-table :test 'equal)))
    ;; Map "from" addresses to "sender-N" faces in chronological order.
    (dotimes (i (1+ index))
      (let* ((msg (nth i mu4e~conversation-thread))
             (from (car (mu4e-message-field msg :from)))
             (from-me-p (member (cdr from) mu4e-user-mail-address-list)))
        (unless (or from-me-p
                    (gethash (cdr from) sender-faces))
          (unless (facep (intern (format "mu4e-conversation-sender-%s" face-index)))
            (setq face-index 1))
          (puthash (cdr from)
                   (intern (format "mu4e-conversation-sender-%s" face-index))
                   sender-faces)
          (setq face-index (1+ face-index)))))
    ;; Actual printing.
    (insert (propertize (concat (format "%s, %s %s\n"
                                        (if from-me-p
                                            mu4e-conversation-my-name
                                          (format "%s <%s>" (car from) (cdr from)))
                                        (current-time-string (mu4e-message-field msg :date))
                                        (mu4e-message-field msg :flags)))
                        'face
                        'mu4e-conversation-header)
            (propertize (mu4e-message-body-text msg) 'face
                        (if from-me-p
                            'mu4e-conversation-sender-me
                          (gethash (cdr from) sender-faces)))
            "\n")))

(defvar mu4e~conversation-previous-header-func nil)
(defun mu4e-conversation-header-handler (msg)
  "Store thread messages.
The header handler is run for all messages before the found-handler.
See `mu4e~proc-filter'"
  (push msg mu4e~conversation-thread-headers))

(defvar mu4e~conversation-previous-update-func nil)
(defun mu4e-conversation-update-handler (_msg _is-move)
  "Don't update the header buffer.")

(defvar mu4e~conversation-previous-found-func nil)
(defun mu4e-conversation-found-handler (_count)
  (setq mu4e-header-func mu4e~conversation-previous-header-func
        mu4e-update-func mu4e~conversation-previous-update-func
        mu4e-found-func mu4e~conversation-previous-found-func)
  ;; TODO: Check if current buffer is mu4e-headers?
  (if (= (length mu4e~conversation-thread-headers) 1)
      (mu4e-headers-view-message)
    (setq mu4e~conversation-thread nil
          mu4e~conversation-previous-view-func mu4e-view-func
          mu4e-view-func 'mu4e-conversation-view-handler)
    (dolist (msg mu4e~conversation-thread-headers)
      (let ((docid (mu4e-message-field msg :docid))
            ;; decrypt (or not), based on `mu4e-decryption-policy'.
            (decrypt
             (and (member 'encrypted (mu4e-message-field msg :flags))
                  (if (eq mu4e-decryption-policy 'ask)
                      (yes-or-no-p (mu4e-format "Decrypt message?")) ; TODO: Never ask?
                    mu4e-decryption-policy))))
        (mu4e~proc-view docid mu4e-view-show-images decrypt)))))

;;;###autoload
(defun mu4e-conversation ()
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (setq mu4e~conversation-current-message (mu4e-message-at-point))
  (unless mu4e~conversation-current-message
    (mu4e-warn "No message at point"))
  (setq mu4e~conversation-thread-headers nil
        mu4e~conversation-previous-update-func mu4e-update-func
        mu4e-update-func 'mu4e-conversation-update-handler
        mu4e~conversation-previous-header-func mu4e-header-func
        mu4e-header-func 'mu4e-conversation-header-handler
        mu4e~conversation-previous-found-func mu4e-found-func
        mu4e-found-func 'mu4e-conversation-found-handler)
  (mu4e~proc-find
   (funcall mu4e-query-rewrite-function
            (format "msgid:%s" (mu4e-message-field (mu4e-message-at-point) :message-id)))
   (not 'show-threads)            ; TODO: Add option to use tree view, e.g. with outline-mode.
   :date
   'ascending
   (not 'limited)
   'skip-duplicates
   'include-related))

(provide 'mu4e-conversation)
;; end of mu4e-conversation
