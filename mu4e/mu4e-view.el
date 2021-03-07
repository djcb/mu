;;; mu4e-view.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dirk-Jan C. Binnema

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

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:

;;; Options

(defgroup mu4e-view nil
  "Settings for the message view."
  :group 'mu4e)

(defcustom mu4e-view-use-gnus t
  "If non-nil, use the new Gnus-based viewer.
Otherwise, use the old viewer."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-scroll-to-next t
  "Move to the next message when calling
`mu4e-view-scroll-up-or-next' (typically bound to SPC) when at
the end of a message. Otherwise, don't move to the next message."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-fields
  '(:from :to  :cc :subject :flags :date :maildir :mailing-list :tags
          :attachments :signature :decryption)
  "Header fields to display in the message view buffer.
For the complete list of available headers, see `mu4e-header-info'."
  :type (list 'symbol)
  :group 'mu4e-view)

(defcustom mu4e-view-actions
  '( ("capture message"  . mu4e-action-capture-message)
     ("view as pdf"      . mu4e-action-view-as-pdf)
     ("show this thread" . mu4e-action-show-thread))
  "List of actions to perform on messages in view mode.
The actions are cons-cells of the form:
  (NAME . FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first letter of NAME is used as a shortcut character."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))



(defun mu4e-view (msg)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that.

Depending on the value of `mu4e-view-use-gnus', either use mu4e's
internal display mode, or a display mode based on Gnus'
article-mode."
  (mu4e~headers-update-handler msg nil nil);; update headers, if necessary.

  ;; sanity check; only one can be active.
  (if mu4e-view-use-gnus
      (progn
        (when (featurep 'mu4e-view-old)
          (mu4e-error "Cannot load gnus-based view with old one loaded. Restart emacs"))
        (require 'mu4e-view-gnus)
        (mu4e~view-gnus msg))
    (progn
      (when (featurep 'mu4e-view-gnus)
        (mu4e-error "Cannot load old view with gnus-based view loaded. Restart emacs"))
      (require 'mu4e-view-old)
      (mu4e~view-old msg))))



(defun mu4e~view-quit-buffer ()
  "Quit the mu4e-view buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (if (eq mu4e-split-view 'single-window)
      (when (buffer-live-p (mu4e-get-view-buffer))
        (kill-buffer (mu4e-get-view-buffer)))
    (unless (eq major-mode 'mu4e-view-mode)
      (mu4e-error "Must be in mu4e-view-mode (%S)" major-mode))
    (let ((curbuf (current-buffer))
          (curwin (selected-window))
          (headers-win))
      (walk-windows
       (lambda (win)
         ;; check whether the headers buffer window is visible
         (when (eq (mu4e-get-headers-buffer) (window-buffer win))
           (setq headers-win win))
         ;; and kill any _other_ (non-selected) window that shows the current
         ;; buffer
         (when
             (and
              (eq curbuf (window-buffer win)) ;; does win show curbuf?
              (not (eq curwin win))         ;; but it's not the curwin?
              (not (one-window-p))) ;; and not the last one on the frame?
           (delete-window win))))  ;; delete it!
      ;; now, all *other* windows should be gone.
      ;; if the headers view is also visible, kill ourselves + window; otherwise
      ;; switch to the headers view
      (if (window-live-p headers-win)
          ;; headers are visible
          (progn
            (kill-buffer-and-window) ;; kill the view win
            (setq mu4e~headers-view-win nil)
            (select-window headers-win)) ;; and switch to the headers win...
        ;; headers are not visible...
        (progn
          (kill-buffer)
          (setq mu4e~headers-view-win nil)
          (when (buffer-live-p (mu4e-get-headers-buffer))
            (switch-to-buffer (mu4e-get-headers-buffer))))))))


(defconst mu4e~view-raw-buffer-name " *mu4e-raw-view*"
  "Name for the raw message view buffer.")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path))
        (buf (get-buffer-create mu4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (mu4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (view-mode)
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD.
Then, display the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-message-field (mu4e-message-at-point) :path)))
    (mu4e-process-file-through-pipe path cmd)))



(defmacro mu4e~view-in-headers-context (&rest body)
  "Evaluate BODY in the context of the headers buffer connected to
this view."
  `(progn
     (unless (buffer-live-p (mu4e-get-headers-buffer))
       (mu4e-error "no headers buffer connected"))
     (let* ((msg (mu4e-message-at-point))
            (docid (mu4e-message-field msg :docid)))
       (unless docid
         (mu4e-error "message without docid: action is not possible."))
       (with-current-buffer (mu4e-get-headers-buffer)
         (unless (eq mu4e-split-view 'single-window)
           (when (get-buffer-window)
             (select-window (get-buffer-window))))
         (if (mu4e~headers-goto-docid docid)
             ,@body
           (mu4e-error "cannot find message in headers buffer."))))))

(defun mu4e-view-headers-next (&optional n)
  "Move point to the next message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (mu4e~view-in-headers-context
   (mu4e~headers-move (or n 1))))

(defun mu4e-view-headers-prev (&optional n)
  "Move point to the previous message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (mu4e~view-in-headers-context
   (mu4e~headers-move (- (or n 1)))))

(defun mu4e~view-prev-or-next-unread (backwards)
  "Move point to the next or previous (when BACKWARDS is non-`nil')
unread message header in the headers buffer connected with this
message view. If this succeeds, return the new docid. Otherwise,
return nil."
  (mu4e~view-in-headers-context
   (mu4e~headers-prev-or-next-unread backwards))
  (if (eq mu4e-split-view 'single-window)
      (when (eq (window-buffer) (mu4e-get-view-buffer))
        (with-current-buffer (mu4e-get-headers-buffer)
          (mu4e-headers-view-message)))
    (mu4e-select-other-view)
    (mu4e-headers-view-message)))

(defun mu4e-view-headers-prev-unread ()
  "Move point to the previous unread message header in the headers
buffer connected with this message view. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~view-prev-or-next-unread t))

(defun mu4e-view-headers-next-unread ()
  "Move point to the next unread message header in the headers
buffer connected with this message view. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~view-prev-or-next-unread nil))


;;; Interactive functions
(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG, then do it.
If MSG is nil apply action to message returned
bymessage-at-point.  The actions are specified in
`mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (actionfunc (mu4e-read-option "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))

(defun mu4e-view-mark-pattern ()
  "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-mark-pattern)))

(defun mu4e-view-mark-thread (&optional markpair)
  "Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the thread at point in the headers view. The
optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e~view-in-headers-context
   (if markpair (mu4e-headers-mark-thread nil markpair)
     (call-interactively 'mu4e-headers-mark-thread))))

(defun mu4e-view-mark-subthread (&optional markpair)
  "Ask user for a kind of mark (move, delete etc.), and apply it
to all messages in the subthread at point in the headers view.
The optional MARKPAIR can also be used to provide the mark
selection."
  (interactive)
  (mu4e~view-in-headers-context
   (if markpair (mu4e-headers-mark-subthread markpair)
     (mu4e-headers-mark-subthread))))

(defun mu4e-view-search-narrow ()
  "Run `mu4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context
   (call-interactively 'mu4e-headers-search-narrow)))

(defun mu4e-view-search-edit ()
  "Run `mu4e-headers-search-edit' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-search-edit)))

(defun mu4e-mark-region-code ()
  "Highlight region marked with `message-mark-inserted-region'.
Add this function to `mu4e-view-mode-hook' to enable this feature."
  (require 'message)
  (let (beg end ov-beg ov-end ov-inv)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" message-mark-insert-begin) nil t)
        (setq ov-beg (match-beginning 0)
              ov-end (match-end 0)
              ov-inv (make-overlay ov-beg ov-end)
              beg    ov-end)
        (overlay-put ov-inv 'invisible t)
        (when (re-search-forward
               (concat "^" message-mark-insert-end) nil t)
          (setq ov-beg (match-beginning 0)
                ov-end (match-end 0)
                ov-inv (make-overlay ov-beg ov-end)
                end    ov-beg)
          (overlay-put ov-inv 'invisible t))
        (when (and beg end)
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'mu4e-region-code))
          (setq beg nil end nil))))))

;;; View Utilities

(defun mu4e-view-mark-custom ()
  "Run some custom mark function."
  (mu4e~view-in-headers-context
   (mu4e-headers-mark-custom)))

(defun mu4e~view-split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))

;;; Scroll commands

(defun mu4e-view-scroll-up-or-next ()
  "Scroll-up the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we can't scroll-up
anymore, go the next message."
  (interactive)
  (condition-case nil
      (scroll-up)
    (error
     (when mu4e-view-scroll-to-next
       (mu4e-view-headers-next)))))

(defun mu4e-scroll-up ()
  "Scroll text of selected window up one line."
  (interactive)
  (scroll-up 1))

(defun mu4e-scroll-down ()
  "Scroll text of selected window down one line."
  (interactive)
  (scroll-down 1))

;;; Mark commands

(defun mu4e-view-unmark-all ()
  "If we're in split-view, unmark all messages.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
      (mu4e~view-in-headers-context (mu4e-mark-unmark-all))
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
      (mu4e-view-mark-for-unmark)
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defmacro mu4e~view-defun-mark-for (mark)
  "Define a function mu4e-view-mark-for-MARK."
  (let ((funcname (intern (format "mu4e-view-mark-for-%s" mark)))
        (docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e~view-in-headers-context
               (mu4e-headers-mark-and-next ',mark)))
       (put ',funcname 'definition-name ',mark))))

(mu4e~view-defun-mark-for move)
(mu4e~view-defun-mark-for refile)
(mu4e~view-defun-mark-for delete)
(mu4e~view-defun-mark-for flag)
(mu4e~view-defun-mark-for unflag)
(mu4e~view-defun-mark-for unmark)
(mu4e~view-defun-mark-for something)
(mu4e~view-defun-mark-for read)
(mu4e~view-defun-mark-for unread)
(mu4e~view-defun-mark-for trash)
(mu4e~view-defun-mark-for untrash)

(defun mu4e-view-marked-execute ()
  "Execute the marked actions."
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e-mark-execute-all)))


(provide 'mu4e-view)
;;; mu4e-view.el ends here
