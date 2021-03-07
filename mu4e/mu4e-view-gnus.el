;;; mu4e-view-gnus.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'mu4e-utils) ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-proc)
(require 'mu4e-actions)
(require 'mu4e-compose)
(require 'mu4e-message)

(eval-when-compile (require 'gnus-art))

(require 'comint)
(require 'button)
(require 'epa)
(require 'epg)
(require 'thingatpt)
(require 'calendar)

(declare-function mu4e-view-mode "mu4e-view")
(defvar gnus-icalendar-additional-identities)
(defvar mu4e~headers-view-win)
(defvar helm-comp-read-use-marked)

;;; Options

;;; Variables

;; It's useful to have the current view message available to
;; `mu4e-view-mode-hooks' functions, and we set up this variable
;; before calling `mu4e-view-mode'.  However, changing the major mode
;; clobbers any local variables.  Work around that by declaring the
;; variable permanent-local.
(defvar-local mu4e~view-message nil
  "The message being viewed in view mode.")
(put 'mu4e~view-message 'permanent-local t)


(defvar mu4e~view-rendering nil)

;;; Main

(defun mu4e-view-message-with-message-id (msgid)
  "View message with message-id MSGID. This (re)creates a
headers-buffer with a search for MSGID, then open a view for that
message."
  (mu4e-headers-search (concat "msgid:" msgid) nil nil t msgid t))

(define-obsolete-function-alias 'mu4e-view-message-with-msgid
  'mu4e-view-message-with-message-id "0.9.17")

(defun mu4e~view-custom-field (msg field)
  "Show some custom header field, or raise an error if it is not
found."
  (let* ((item (or (assoc field mu4e-header-info-custom)
                   (mu4e-error "field %S not found" field)))
         (func (or (plist-get (cdr-safe item) :function)
                   (mu4e-error "no :function defined for field %S %S"
                               field (cdr item)))))
    (funcall func msg)))

(defun mu4e~view-embedded-winbuf ()
  "Get a buffer (shown in a window) for the embedded message."
  (let* ((buf (get-buffer-create mu4e~view-embedded-buffer-name))
         (win (or (get-buffer-window buf) (split-window-vertically))))
    (select-window win)
    (switch-to-buffer buf)))

(defun mu4e~delete-all-overlays ()
  "`delete-all-overlays' with compatibility fallback."
  (if (functionp 'delete-all-overlays)
      (delete-all-overlays)
    (remove-overlays)))


;; remember the mime-handles, so we can clean them up when
;; we quit this buffer.
(defvar-local mu4e~gnus-article-mime-handles nil)
(put 'mu4e~gnus-article-mime-handles 'permanent-local t)

(defun mu4e~view-gnus (msg)
  "View MSG using Gnus' article mode. Experimental."
  (require 'gnus-art)
  (let ((path (mu4e-message-field msg :path))
        (inhibit-read-only t)
        (mm-decrypt-option 'known)
        (gnus-article-emulate-mime t)
        (gnus-buttonized-mime-types (append (list "multipart/signed"
                                                  "multipart/encrypted")
                                            gnus-buttonized-mime-types)))
    (switch-to-buffer (get-buffer-create mu4e~view-buffer-name))
    (buffer-disable-undo)
    (insert-file-contents-literally path nil nil nil t)
    (mm-enable-multibyte)
    (setq
     gnus-summary-buffer (get-buffer-create " *appease-gnus*")
     gnus-original-article-buffer (current-buffer))
    (let* ((ct (mail-fetch-field "Content-Type"))
           (ct (and ct (mail-header-parse-content-type ct)))
           (charset (mail-content-type-get ct 'charset))
           (charset (and charset (intern charset)))
           (gnus-newsgroup-charset
            (if (and charset (coding-system-p charset)) charset
              (detect-coding-region (point-min) (point-max) t))))
      (run-hooks 'gnus-article-decode-hook))
    (let ((mu4e~view-rendering t) ; customize gnus in mu4e
          (max-specpdl-size mu4e-view-max-specpdl-size)
          (gnus-blocked-images ".") ;; don't load external images.
          ;; Possibly add headers (before "Attachments")
          (gnus-display-mime-function (mu4e~view-gnus-display-mime msg))
          (gnus-icalendar-additional-identities
           (mu4e-personal-addresses 'no-regexp)))
      (gnus-article-prepare-display))
    (setq mu4e~gnus-article-mime-handles gnus-article-mime-handles)
    (setq mu4e~view-message msg)
    ;; `mu4e-view-mode' derive from `gnus-article-mode'.
    (mu4e-view-mode)
    (setq gnus-article-decoded-p gnus-article-decode-hook)
    (set-buffer-modified-p nil)
    (add-hook 'kill-buffer-hook #'mu4e~view-kill-buffer-hook-fn)))

(defun mu4e~view-kill-buffer-hook-fn ()
  ;; cleanup the mm-* buffers that the view spawns
  (when mu4e~gnus-article-mime-handles
    (mm-destroy-parts mu4e~gnus-article-mime-handles)
    (setq mu4e~gnus-article-mime-handles nil)))

(defun mu4e~view-gnus-display-mime (msg)
  "Same as `gnus-display-mime' but add a mu4e headers to MSG."
  (lambda (&optional ihandles)
    (gnus-display-mime ihandles)
    (unless ihandles
      (save-restriction
        (article-goto-body)
        (forward-line -1)
        (narrow-to-region (point) (point))
        (dolist (field mu4e-view-fields)
          (let ((fieldval (mu4e-message-field msg field)))
            (cl-case field
              ((:path :maildir :user-agent :mailing-list :message-id)
               (mu4e~view-gnus-insert-header field fieldval))
              ((:flags :tags)
               (let ((flags (mapconcat (lambda (flag)
                                         (if (symbolp flag)
	                                     (symbol-name flag)
	                                   flag)) fieldval ", ")))
                 (mu4e~view-gnus-insert-header field flags)))
              (:size (mu4e~view-gnus-insert-header
                      field (mu4e-display-size fieldval)))
              ((:subject :to :from :cc :bcc :from-or-to :date :attachments
                         :signature :decryption)) ; handled by Gnus
              (t
               (mu4e~view-gnus-insert-header-custom msg field))
              )))
        (let ((gnus-treatment-function-alist
               '((gnus-treat-highlight-headers
                  gnus-article-highlight-headers))))
          (gnus-treat-article 'head))))))

(defun mu4e~view-gnus-insert-header (field val)
  "Insert a header FIELD with value VAL in Gnus article view."
  (let* ((info (cdr (assoc field mu4e-header-info)))
	 (key (plist-get info :name))
         (help (plist-get info :help)))
    (if (and val (> (length val) 0))
        (insert (propertize (concat key ":") 'help-echo help)
                " " val "\n"))))

(defun mu4e~view-gnus-insert-header-custom (msg field)
  "Insert the custom FIELD in Gnus article view."
  (let* ((info (cdr-safe (or (assoc field mu4e-header-info-custom)
                             (mu4e-error "custom field %S not found" field))))
	 (key (plist-get info :name))
         (func (or (plist-get info :function)
                   (mu4e-error "no :function defined for custom field %S %S"
		               field info)))
         (val (funcall func msg))
         (help (plist-get info :help)))
    (when (and val (> (length val) 0))
      (insert (propertize (concat key ":") 'help-echo help) " " val "\n"))))

(define-advice gnus-icalendar-event-from-handle
    (:filter-args (handle-attendee) mu4e~view-fix-missing-charset)
  "Do not trigger an error when displaying an ical attachment
with no charset."
  (if (and (boundp 'mu4e~view-rendering) mu4e~view-rendering)
      (let* ((handle (car handle-attendee))
             (attendee (cadr handle-attendee))
             (buf (mm-handle-buffer handle))
             (ty (mm-handle-type handle))
             (rest (cddr handle)))
        ;; Put the fallback at the end:
        (setq ty (append ty '((charset . "utf-8"))))
        (setq handle (cons buf (cons ty rest)))
        (list handle attendee))
  handle-attendee))

(defvar mu4e-view-mode-map nil
  "Keymap for \"*mu4e-view*\" buffers.")
(unless mu4e-view-mode-map
  (setq mu4e-view-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map  (kbd "C-S-u") 'mu4e-update-mail-and-index)
          (define-key map  (kbd "C-c C-u") 'mu4e-update-mail-and-index)

          (define-key map "q" 'mu4e~view-quit-buffer)

          ;; note, 'z' is by-default bound to 'bury-buffer'
          ;; but that's not very useful in this case
          (define-key map "z" 'ignore)

          (define-key map "s" 'mu4e-headers-search)
          (define-key map "S" 'mu4e-view-search-edit)
          (define-key map "/" 'mu4e-view-search-narrow)

          (define-key map (kbd "<M-left>")  'mu4e-headers-query-prev)
          (define-key map (kbd "<M-right>") 'mu4e-headers-query-next)

          (define-key map "b" 'mu4e-headers-search-bookmark)
          (define-key map "B" 'mu4e-headers-search-bookmark-edit)

          (define-key map "%" 'mu4e-view-mark-pattern)
          (define-key map "t" 'mu4e-view-mark-subthread)
          (define-key map "T" 'mu4e-view-mark-thread)

          (define-key map "v" 'mu4e-view-verify-msg-popup)

          (define-key map "j" 'mu4e~headers-jump-to-maildir)

          (define-key map "g" 'ignore)
          (define-key map "k" 'ignore)
          (define-key map "f" 'ignore)

          (define-key map "F" 'mu4e-compose-forward)
          (define-key map "R" 'mu4e-compose-reply)
          (define-key map "C" 'mu4e-compose-new)
          (define-key map "E" 'mu4e-compose-edit)

          ;; some gnus things we do not support
          (define-key map "G" 'ignore)
          (define-key map "I" 'ignore)
          (define-key map "J" 'ignore)
          (define-key map "K" 'ignore)
          (define-key map "L" 'ignore)
          (define-key map "N" 'ignore)
          (define-key map "V" 'ignore)
          (define-key map "X" 'ignore)
          (define-key map "Y" 'ignore)
          (define-key map "Z" 'ignore)

          (define-key map "." 'mu4e-view-raw-message)
          (define-key map "|" 'mu4e-view-pipe)
          (define-key map "a" 'mu4e-view-action)

          (define-key map ";" 'mu4e-context-switch)

          ;; toggle header settings
          (define-key map "O" 'mu4e-headers-change-sorting)
          (define-key map "P" 'mu4e-headers-toggle-threading)
          (define-key map "Q" 'mu4e-headers-toggle-full-search)
          (define-key map "W" 'mu4e-headers-toggle-include-related)

          ;; change the number of headers
          (define-key map (kbd "C-+") 'mu4e-headers-split-view-grow)
          (define-key map (kbd "C--") 'mu4e-headers-split-view-shrink)
          (define-key map (kbd "<C-kp-add>") 'mu4e-headers-split-view-grow)
          (define-key map (kbd "<C-kp-subtract>") 'mu4e-headers-split-view-shrink)

          ;; intra-message navigation
          (define-key map (kbd "SPC") 'mu4e-view-scroll-up-or-next)
          (define-key map (kbd "<home>") 'beginning-of-buffer)
          (define-key map (kbd "<end>") 'end-of-buffer)
          (define-key map (kbd "RET")
            (lambda()
              (interactive)
              (if (eq (get-text-property (point) 'gnus-callback) 'gnus-button-push)
                  (widget-button-press (point))
                (mu4e-scroll-up))))
          (define-key map (kbd "<backspace>") 'mu4e-scroll-down)

          ;; navigation between messages
          (define-key map "p" 'mu4e-view-headers-prev)
          (define-key map "n" 'mu4e-view-headers-next)
          ;; the same
          (define-key map (kbd "<M-down>") 'mu4e-view-headers-next)
          (define-key map (kbd "<M-up>") 'mu4e-view-headers-prev)

          (define-key map (kbd "[") 'mu4e-view-headers-prev-unread)
          (define-key map (kbd "]") 'mu4e-view-headers-next-unread)

          ;; switching to view mode (if it's visible)
          (define-key map "y" 'mu4e-select-other-view)

          ;; attachments
          (define-key map "e" 'mu4e-view-save-attachment)
          (define-key map "o" 'ignore)
          (define-key map "A" 'ignore)

          ;; marking/unmarking
          (define-key map "d" 'mu4e-view-mark-for-trash)
          (define-key map (kbd "<delete>") 'mu4e-view-mark-for-delete)
          (define-key map (kbd "<deletechar>") 'mu4e-view-mark-for-delete)
          (define-key map (kbd "D") 'mu4e-view-mark-for-delete)
          (define-key map (kbd "m") 'mu4e-view-mark-for-move)
          (define-key map (kbd "r") 'mu4e-view-mark-for-refile)

          (define-key map (kbd "?") 'mu4e-view-mark-for-unread)
          (define-key map (kbd "!") 'mu4e-view-mark-for-read)

          (define-key map (kbd "+") 'mu4e-view-mark-for-flag)
          (define-key map (kbd "-") 'mu4e-view-mark-for-unflag)
          (define-key map (kbd "=") 'mu4e-view-mark-for-untrash)
          (define-key map (kbd "&") 'mu4e-view-mark-custom)

          (define-key map (kbd "*")             'mu4e-view-mark-for-something)
          (define-key map (kbd "<kp-multiply>") 'mu4e-view-mark-for-something)
          (define-key map (kbd "<insert>")     'mu4e-view-mark-for-something)
          (define-key map (kbd "<insertchar>") 'mu4e-view-mark-for-something)

          (define-key map (kbd "#") 'mu4e-mark-resolve-deferred-marks)

          ;; misc
          (define-key map "w" 'visual-line-mode)
          (define-key map "#" 'ignore)
          (define-key map "h" 'ignore)
          (define-key map (kbd "M-q") 'article-fill-long-lines)

          ;; next 3 only warn user when attempt in the message view
          (define-key map "u" 'mu4e-view-unmark)
          (define-key map "U" 'mu4e-view-unmark-all)
          (define-key map "x" 'mu4e-view-marked-execute)

          (define-key map "$" 'mu4e-show-log)
          (define-key map "H" 'mu4e-display-manual)

          ;; menu
          ;;(define-key map [menu-bar] (make-sparse-keymap))
          (let ((menumap (make-sparse-keymap)))
            (define-key map [menu-bar headers] (cons "Mu4e" menumap))

            (define-key menumap [quit-buffer]
              '("Quit view" . mu4e~view-quit-buffer))
            (define-key menumap [display-help] '("Help" . mu4e-display-manual))

            (define-key menumap [sepa0] '("--"))
            (define-key menumap [wrap-lines]
              '("Toggle wrap lines" . visual-line-mode))
            (define-key menumap [raw-view]
              '("View raw message" . mu4e-view-raw-message))
            (define-key menumap [pipe]
              '("Pipe through shell" . mu4e-view-pipe))

            (define-key menumap [sepa1] '("--"))
            (define-key menumap [mark-delete]
              '("Mark for deletion" . mu4e-view-mark-for-delete))
            (define-key menumap [mark-untrash]
              '("Mark for untrash" .  mu4e-view-mark-for-untrash))
            (define-key menumap [mark-trash]
              '("Mark for trash" .  mu4e-view-mark-for-trash))
            (define-key menumap [mark-move]
              '("Mark for move" . mu4e-view-mark-for-move))

            (define-key menumap [sepa2] '("--"))
            (define-key menumap [resend]  '("Resend" . mu4e-compose-resend))
            (define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
            (define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
            (define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
            (define-key menumap [sepa3] '("--"))

            (define-key menumap [query-next]
              '("Next query" . mu4e-headers-query-next))
            (define-key menumap [query-prev]
              '("Previous query" . mu4e-headers-query-prev))
            (define-key menumap [narrow-search]
              '("Narrow search" . mu4e-headers-search-narrow))
            (define-key menumap [bookmark]
              '("Search bookmark" . mu4e-headers-search-bookmark))
            (define-key menumap [jump]
              '("Jump to maildir" . mu4e~headers-jump-to-maildir))
            (define-key menumap [search]
              '("Search" . mu4e-headers-search))

            (define-key menumap [sepa4]     '("--"))
            (define-key menumap [next]      '("Next" . mu4e-view-headers-next))
            (define-key menumap [previous]  '("Previous" . mu4e-view-headers-prev)))
          map))

  (fset 'mu4e-view-mode-map mu4e-view-mode-map))

(defcustom mu4e-view-mode-hook nil
  "Hook run when entering Mu4e-View mode."
  :options '(turn-on-visual-line-mode)
  :type 'hook
  :group 'mu4e-view)

(defvar mu4e-view-mode-abbrev-table nil)

(defun mu4e~view-mode-body ()
  "Body of the mode-function."
  (use-local-map mu4e-view-mode-map)
  (mu4e-context-in-modeline)
  (setq buffer-undo-list t);; don't record undo info
  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t)))

;;  "Define the major-mode for the mu4e-view."
(define-derived-mode mu4e-view-mode gnus-article-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e, based on
Gnus' article-mode."
  ;; remove some gnus stuff that does not apply
  (define-key mu4e-view-mode-map [menu-bar Treatment] nil)
  (define-key mu4e-view-mode-map [menu-bar Article] nil)
  (define-key mu4e-view-mode-map [menu-bar post] nil)
  (define-key mu4e-view-mode-map [menu-bar commands] nil)
  ;; Restore C-h b default behavior
  (define-key mu4e-view-mode-map (kbd "C-h b") 'describe-bindings)
  (setq mu4e~view-buffer-name gnus-article-buffer)
  (mu4e~view-mode-body))

(defun mu4e-view-save-attachment (&optional arg)
  "Save mime parts from current mu4e gnus view buffer.

When helm-mode is enabled provide completion on attachments and
possibility to mark candidates to save, otherwise completion on
attachments is done with `completing-read-multiple', in this case use
\",\" to separate candidate, completion is provided after each \",\"."
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let ((handles '())
        (files '())
        (helm-comp-read-use-marked t)
        (compfn (if helm-mode
                    #'completing-read
                  ;; Fallback to `completing-read-multiple' with poor
                  ;; completion systems.
                  #'completing-read-multiple))
        dir)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((handle (get-text-property (point) 'gnus-data)))
          (when (consp handle)
            (let ((fname (cdr (assoc 'filename (assoc "attachment" (cdr handle))))))
              (when fname
                (push `(,fname . ,handle) handles)
                (push fname files)))))
        (forward-line 1)))
    (if files
        (progn
          (setq files (funcall compfn "Save part(s): " files)
                dir (if arg
                        (read-directory-name "Save to directory: ")
                      mu4e-attachment-dir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h (expand-file-name f dir))))
      (message "No attached files found"))))

;;; Various commands

;;;
(provide 'mu4e-view-gnus)
;;; mu4e-view.el ends here
