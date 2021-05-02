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

(require 'mu4e-view-common)
(require 'calendar)
(require 'gnus-art)

;;; Variables

(defvar gnus-icalendar-additional-identities)
(defvar helm-comp-read-use-marked)
(defvar-local mu4e~view-rendering nil)

(make-obsolete-variable 'mu4e-view-blocked-images 'gnus-blocked-images
                        "1.5.12")
(make-obsolete-variable 'mu4e-view-inhibit-images 'gnus-inhibit-images
                        "1.5.12")
;;; Main

;; remember the mime-handles, so we can clean them up when
;; we quit this buffer.
(defvar-local mu4e~gnus-article-mime-handles nil)
(put 'mu4e~gnus-article-mime-handles 'permanent-local t)

(defun mu4e~view-gnus (msg)
  "View MSG using Gnus' article mode."
  (when (bufferp gnus-article-buffer)
    (kill-buffer gnus-article-buffer))
  (with-current-buffer (get-buffer-create gnus-article-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents-literally
       (mu4e-message-field msg :path) nil nil nil t)))
  (switch-to-buffer gnus-article-buffer)
  (setq mu4e~view-message msg)
  (mu4e~view-render-buffer msg))

(defun mu4e-view-message-text (msg)
  "Return the pristine message as a string, for replying/forwarding
etc."
  ;; we need this for replying/forwarding, since the mu4e-compose
  ;; wants it that way.
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-field msg :path) nil nil nil t)
    (mu4e~view-render-buffer msg)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mu4e-action-view-in-browser (msg)
  "Show current message MSG in browser, if it contains an html body."
;;  (with-temp-buffer
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-field msg :path) nil nil nil t)
    (run-hooks 'gnus-article-decode-hook)
    (let ((header (cl-loop for field in '("from" "to" "cc" "date" "subject")
                           when (message-fetch-field field)
                           concat (format "%s: %s\n" (capitalize field) it)))
          (parts (mm-dissect-buffer t t)))
      ;; If singlepart, enforce a list.
      (when (and (bufferp (car parts))
                 (stringp (car (mm-handle-type parts))))
        (setq parts (list parts)))
      ;; Process the list
      (unless (gnus-article-browse-html-parts parts header)
        (mu4e-warn "Message does not contain a \"text/html\" part"))
      (mm-destroy-parts parts))))


(defun mu4e~view-render-buffer (msg)
  "Render current buffer with MSG using Gnus' article mode in
buffer BUF."
  (setq gnus-summary-buffer (get-buffer-create " *appease-gnus*"))
  (let* ((inhibit-read-only t)
         (max-specpdl-size mu4e-view-max-specpdl-size)
         (mm-decrypt-option 'known)
         (ct (mail-fetch-field "Content-Type"))
         (ct (and ct (mail-header-parse-content-type ct)))
         (charset (mail-content-type-get ct 'charset))
         (charset (and charset (intern charset)))
         (mu4e~view-rendering t); Needed if e.g. an ics file is buttonized
         (gnus-article-emulate-mime t)
         (gnus-unbuttonized-mime-types '(".*/.*"))
         (gnus-buttonized-mime-types
            (append (list "multipart/signed" "multipart/encrypted")
                    gnus-buttonized-mime-types))
         (gnus-newsgroup-charset
          (if (and charset (coding-system-p charset)) charset
            (detect-coding-region (point-min) (point-max) t)))
         ;; Possibly add headers (before "Attachments")
         (gnus-display-mime-function (mu4e~view-gnus-display-mime msg))
         (gnus-icalendar-additional-identities
          (mu4e-personal-addresses 'no-regexp)))
    (mm-enable-multibyte)
    (mu4e-view-mode)
    (run-hooks 'gnus-article-decode-hook)
    (gnus-article-prepare-display)
    (mu4e~view-activate-urls)
    (setq mu4e~gnus-article-mime-handles gnus-article-mime-handles
          gnus-article-decoded-p gnus-article-decode-hook)
    (set-buffer-modified-p nil)
    (add-hook 'kill-buffer-hook #'mu4e~view-kill-mime-handles)))

(defun mu4e~view-kill-mime-handles ()
  "Kill cached MIME-handles, if any."
  (when mu4e~gnus-article-mime-handles
    (mm-destroy-parts mu4e~gnus-article-mime-handles)
    (setq mu4e~gnus-article-mime-handles nil)))

(defun mu4e~view-gnus-display-mime (msg)
  "Same as `gnus-display-mime' but include mu4e headers to MSG."
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
               (mu4e~view-gnus-insert-header-custom msg field)))))
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


(defun mu4e~view-mode-p ()
  (or (eq major-mode 'mu4e-view-mode)
      (derived-mode-p '(mu4e-view-mode))))

(defun mu4e~view-nop (func &rest args)
  "Do nothing when in mu4e-view-mode. This is useful for advising
some Gnus-functionality that does not work in mu4e."
  (unless (mu4e~view-mode-p)
    (apply func args)))

(defun mu4e~view-button-reply (func &rest args)
  "Advice to make `gnus-button-reply' links work in mu4e."
  (if (mu4e~view-mode-p)
      (mu4e-compose-reply)
    (apply func args)))

(defun mu4e~view-msg-mail (func &rest args)
  "Advice to make `gnus-msg-mail' links compose with mu4e."
  (if (mu4e~view-mode-p)
      (apply 'mu4e~compose-mail args)
    (apply func args)))

(defvar mu4e-view-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map  (kbd "C-S-u") 'mu4e-update-mail-and-index)
    (define-key map  (kbd "C-c C-u") 'mu4e-update-mail-and-index)

    (define-key map "q" 'mu4e~view-quit-buffer)

    ;; note, 'z' is by-default bound to 'bury-buffer'
    ;; but that's not very useful in this case
    (define-key map "z" 'ignore)

    (define-key map "s" #'mu4e-headers-search)
    (define-key map "S" #'mu4e-view-search-edit)
    (define-key map "/" #'mu4e-view-search-narrow)

    (define-key map (kbd "<M-left>")  #'mu4e-headers-query-prev)
    (define-key map (kbd "<M-right>") #'mu4e-headers-query-next)

    (define-key map "b" #'mu4e-headers-search-bookmark)
    (define-key map "B" #'mu4e-headers-search-bookmark-edit)

    (define-key map "%" #'mu4e-view-mark-pattern)
    (define-key map "t" #'mu4e-view-mark-subthread)
    (define-key map "T" #'mu4e-view-mark-thread)
    (define-key map "j" 'mu4e~headers-jump-to-maildir)

    (define-key map "g" #'mu4e-view-go-to-url)
    (define-key map "k" #'mu4e-view-save-url)
    (define-key map "f" #'mu4e-view-fetch-url)

    (define-key map "F" #'mu4e-compose-forward)
    (define-key map "R" #'mu4e-compose-reply)
    (define-key map "C" #'mu4e-compose-new)
    (define-key map "E" #'mu4e-compose-edit)

    (define-key map "." #'mu4e-view-raw-message)
    (define-key map "|" #'mu4e-view-pipe)
    (define-key map "a" #'mu4e-view-action)
    (define-key map "A" #'mu4e-view-mime-part-action)
    (define-key map "e" #'mu4e-view-save-attachments)

    (define-key map ";" #'mu4e-context-switch)

          ;; toggle header settings
    (define-key map "O" #'mu4e-headers-change-sorting)
    (define-key map "P" #'mu4e-headers-toggle-threading)
    (define-key map "Q" #'mu4e-headers-toggle-full-search)
    (define-key map "W" #'mu4e-headers-toggle-include-related)

    ;; change the number of headers
    (define-key map (kbd "C-+") #'mu4e-headers-split-view-grow)
    (define-key map (kbd "C--") #'mu4e-headers-split-view-shrink)
    (define-key map (kbd "<C-kp-add>") #'mu4e-headers-split-view-grow)
    (define-key map (kbd "<C-kp-subtract>") #'mu4e-headers-split-view-shrink)

    ;; intra-message navigation
    (define-key map (kbd "SPC") #'mu4e-view-scroll-up-or-next)
    (define-key map (kbd "<home>") 'beginning-of-buffer)
    (define-key map (kbd "<end>") 'end-of-buffer)
    (define-key map (kbd "RET")  #'mu4e-scroll-up)
    (define-key map (kbd "<backspace>") #'mu4e-scroll-down)

    ;; navigation between messages
    (define-key map "p" #'mu4e-view-headers-prev)
    (define-key map "n" #'mu4e-view-headers-next)
    ;; the same
    (define-key map (kbd "<M-down>") #'mu4e-view-headers-next)
    (define-key map (kbd "<M-up>") #'mu4e-view-headers-prev)

    (define-key map (kbd "[") #'mu4e-view-headers-prev-unread)
    (define-key map (kbd "]") #'mu4e-view-headers-next-unread)

    ;; switching from view <-> headers (when visible)
    (define-key map "y" #'mu4e-select-other-view)

    ;; marking/unmarking
    (define-key map "d" #'mu4e-view-mark-for-trash)
    (define-key map (kbd "<delete>") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "<deletechar>") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "D") #'mu4e-view-mark-for-delete)
    (define-key map (kbd "m") #'mu4e-view-mark-for-move)
    (define-key map (kbd "r") #'mu4e-view-mark-for-refile)

    (define-key map (kbd "?") #'mu4e-view-mark-for-unread)
    (define-key map (kbd "!") #'mu4e-view-mark-for-read)

    (define-key map (kbd "+") #'mu4e-view-mark-for-flag)
    (define-key map (kbd "-") #'mu4e-view-mark-for-unflag)
    (define-key map (kbd "=") #'mu4e-view-mark-for-untrash)
    (define-key map (kbd "&") #'mu4e-view-mark-custom)

    (define-key map (kbd "*")             #'mu4e-view-mark-for-something)
    (define-key map (kbd "<kp-multiply>") #'mu4e-view-mark-for-something)
    (define-key map (kbd "<insert>")     #'mu4e-view-mark-for-something)
    (define-key map (kbd "<insertchar>") #'mu4e-view-mark-for-something)

    (define-key map (kbd "#") #'mu4e-mark-resolve-deferred-marks)

    ;; misc
    (define-key map "M" #'mu4e-view-massage)

    (define-key map "w" 'visual-line-mode)
    (define-key map (kbd "M-q") 'article-fill-long-lines)

    ;; next 3 only warn user when attempt in the message view
    (define-key map "u" #'mu4e-view-unmark)
    (define-key map "U" #'mu4e-view-unmark-all)
    (define-key map "x" #'mu4e-view-marked-execute)

    (define-key map "$" #'mu4e-show-log)
    (define-key map "H" #'mu4e-display-manual)

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

    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for mu4e-view mode")

(set-keymap-parent mu4e-view-mode-map button-buffer-map)
(suppress-keymap mu4e-view-mode-map)

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
  ;; Restore C-h b default behavior
  (define-key mu4e-view-mode-map (kbd "C-h b") 'describe-bindings)
  ;; ;; turn off gnus modeline changes and menu items
  (advice-add 'gnus-set-mode-line :around #'mu4e~view-nop)
  (advice-add 'gnus-button-reply :around #'mu4e~view-button-reply)
  (advice-add 'gnus-msg-mail :around #'mu4e~view-msg-mail)
  (mu4e~view-mode-body))

;;; Massaging the message view

(defcustom mu4e-view-massage-options
  '( ("ctoggle citations" . gnus-article-hide-citation)
     ("htoggle headers"   . gnus-article-hide-headers)
     ("ytoggle crypto"    . gnus-article-hide-pem))
"Various options for 'massaging' the message view. See `(gnus)
Article Treatment' for more options."
  :group 'mu4e-view
  :type '(alist :key-type string :value-type function))

(defun mu4e-view-massage()
  "Massage current message view as per `mu4e-view-massage-options'."
  (interactive)
  (funcall (mu4e-read-option "Massage: " mu4e-view-massage-options)))

;;; MIME-parts


(defun mu4e~view-gather-mime-parts ()
  "Gather all MIME parts as an alist that uniquely maps the number
to the gnus-part."
  (let ((parts '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((part (get-text-property (point) 'gnus-data))
              (index (get-text-property (point) 'gnus-part)))
          (when (and part (numberp index) (not (assoc index parts))
            (push `(,index . ,part) parts)))
          (goto-char (or (next-single-property-change (point) 'gnus-part)
                         (point-max))))))
    parts))


(defun mu4e-view-save-attachments (&optional arg)
  "Save mime parts from current mu4e gnus view buffer.

When helm-mode is enabled provide completion on attachments and
possibility to mark candidates to save, otherwise completion on
attachments is done with `completing-read-multiple', in this case
use \",\" to separate candidate, completion is provided after
each \",\".

Note, currently this does not work well with file names
containing commas."
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((parts (mu4e~view-gather-mime-parts))
         (handles '())
         (files '())
         (helm-comp-read-use-marked t)
         (compfn (if (and (boundp 'helm-mode) helm-mode)
                     #'completing-read
                   ;; Fallback to `completing-read-multiple' with poor
                   ;; completion
                   #'completing-read-multiple))
        dir)
    (dolist (part parts)
      (let ((fname (cdr (assoc 'filename (assoc "attachment" (cdr part))))))
        (when fname
          (push `(,fname . ,(cdr part)) handles)
          (push fname files))))
    (if files
        (progn
          (setq files (funcall compfn "Save part(s): " files)
                dir (if arg (read-directory-name "Save to directory: ") mu4e-attachment-dir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h (expand-file-name f dir))))
      (mu4e-message "No attached files found"))))


(defvar mu4e-view-mime-part-actions
  '(
    ;;
    ;; some basic ones
    ;;

    ;; save mime-part to a file
    (:name "save"  :handler gnus-article-save-part :receives index)
    ;; pipe mime part to some arbitrary shell command
    (:name "|pipe" :handler gnus-article-pipe-part :receives index)
    ;; open with the default handler, if any
    (:name "open" :handler mu4e~view-open-file :receives temp)
    ;; open with some custom file.
    (:name "wopen-with" :handler (lambda (file)(mu4e~view-open-file file t))
           :receives temp)

    ;;
    ;; some more examples
    ;;

    ;; import GPG key
    (:name "gpg" :handler epa-import-keys :receives temp)
    ;; count the number of lines in a MIME-part
    (:name "line-count" :handler "wc -l" :receives pipe)
    ;; open in this emacs instance; tries to use the attachment name,
    ;; so emacs can use specific modes etc.
    (:name "emacs" :handler find-file :receives temp)
    ;; open in this emacs instance, "raw"
    (:name "raw" :handler (lambda (str)
                            (let ((tmpbuf (get-buffer-create " *mu4e-raw-mime*")))
                                  (with-current-buffer tmpbuf
                                    (insert str)
                                    (view-mode)
                                    (goto-char (point-min)))
                                  (switch-to-buffer tmpbuf)))  :receives pipe))

  "Actions for MIME-parts. Each is a plist with keys
`(:name <name>         ;; name of the action; shortcut is first letter of name

  :handler             ;; one of:
                       ;; - a function receiving the index/temp/pipe
                       ;; - a string, which is taken as a shell command

  :receives            ;;  a symbol specifying what the handler receives
                       ;; - index: the index number of the mime part (default)
                       ;; - temp: the full path to the mime part in a
                       ;;         temporary file, which is deleted immediately
                       ;;         after invoking handler
                       ;; - pipe:  the attachment is piped to some shell command
                       ;;          or as a string parameter to a function
).")


(defun mu4e~view-mime-part-to-temp-file (handle)
  "Write mime-part N to a temporary file and return the file name.
The filename is deduced from the MIME-part's filename, or
otherwise random; the result is placed in temporary directory
with a unique name.  Returns the full path for the file
created. The directory and file are self-destructed."
  (let* ((tmpdir (make-temp-file "mu4e-temp-" t))
         (fname (cdr-safe (assoc 'filename (assoc "attachment" (cdr handle)))))
         (fname (if fname
                    (concat tmpdir "/" (replace-regexp-in-string "/" "-" fname))
                  (let ((temporary-file-directory tmpdir))
                    (make-temp-file "mimepart")))))
    (mm-save-part-to-file handle fname)
    (run-at-time "30 sec" nil (lambda () (ignore-errors (delete-directory tmpdir t))))
    fname))


(defun mu4e~view-open-file (file &optional force-ask)
  "Open FILE with default handler, if any. Otherwise, or if FORCE_ASK is set,
ask user for the program to open with."
  (let* ((opener
          (pcase system-type
            (`darwin "open")
            ((or 'gnu 'gnu/linux 'gnu/kfreebsd) "xdg-open")))
         (prog (if (or force-ask (not opener))
                   (read-shell-command "Open MIME-part with: ")
                 opener)))
    (call-process prog nil 0 nil file)))

(defun mu4e-view-mime-part-action (&optional n)
  "Apply some action on mime-part N in the current messsage.
If N is not specified, ask for it. N can be supplied as a
prefix-argument, and note that one does not need to prefix that
with C-u.

I.e., '3 A o' opens the third MIME-part."
  (interactive "NNumber of MIME-part: ")
  (let* ((parts (mu4e~view-gather-mime-parts))
         (options (mapcar (lambda (action) `(,(plist-get action :name) . ,action))
                          mu4e-view-mime-part-actions))
         (handle (or (cdr-safe (cl-find-if (lambda (part) (eq (car part) n)) parts))
                     (mu4e-error "MIME-part %s not found" n)))
         (action (or (and options (mu4e-read-option "Action on mime-part: " options))
                     (mu4e-error "No such action")))
         (handler (or (plist-get action :handler)
                      (mu4e-error "No :handler found for action %S" action)))
         (receives (or (plist-get action :receives)
                       (mu4e-error "No :receives found for action %S" action))))
    (save-excursion
      (cond
       ((functionp handler)
        (cond
         ((eq receives 'index) (funcall handler n))
         ((eq receives 'pipe)  (funcall handler (mm-with-unibyte-buffer
                                                  (mm-insert-part handle)
                                                  (buffer-string))))
         ((eq receives 'temp)
          (funcall handler (mu4e~view-mime-part-to-temp-file handle)))
         (t (mu4e-error "Invalid :receive for %S" action))))
       ((stringp handler)
        (cond
         ((eq receives 'index) (shell-command (concat handler " " (shell-quote-argument n))))
         ((eq receives 'pipe)  (mm-pipe-part handle handler))
         ((eq receives 'temp)
          (shell-command (shell-command (concat handler " "
                                                (shell-quote-argument
                                                 (mu4e~view-mime-part-to-temp-file handle))))))
         (t (mu4e-error "Invalid action %S" action))))))))

;;;
(provide 'mu4e-view-gnus)
;;; mu4e-view.el ends here
