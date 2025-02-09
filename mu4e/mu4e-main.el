;;; mu4e-main.el --- The Main interface for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

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

;;; Code:

(require 'smtpmail)
(require 'mu4e-helpers)
(require 'mu4e-context)
(require 'mu4e-compose)
(require 'mu4e-bookmarks)
(require 'mu4e-folders)
(require 'mu4e-update)
(require 'mu4e-contacts)
(require 'mu4e-search)
(require 'mu4e-vars)     ;; mu-wide variables
(require 'mu4e-window)
(require 'mu4e-query-items)

(declare-function mu4e-compose-new  "mu4e-compose")
(declare-function mu4e-quit "mu4e")

(require 'cl-lib)

;; Configuration

(defcustom mu4e-main-hide-personal-addresses nil
  "Whether to hide the personal address in the main view.

  This can be useful to avoid the noise when there are many, and
also hides the warning if your `user-mail-address' is not part of
the personal addresses."
  :type 'boolean
  :group 'mu4e-main)

(defcustom mu4e-main-hide-fully-read nil
  "Whether to hide bookmarks or maildirs without unread messages."
  :type 'boolean
  :group 'mu4e-main)

(defcustom mu4e-main-rendered-hook nil
  "Hook run after the main-view has been rendered."
  :type 'hook
  :group 'mu4e-main)

;;; Mode
(define-derived-mode mu4e-org-mode org-mode "mu4e:org"
  "Major mode for mu4e documents.")

(defun mu4e-info (path)
  "Show a buffer with the information (an org-file) at PATH."
  (unless (file-exists-p path)
    (mu4e-error "Cannot find %s" path))
  (let ((curbuf (current-buffer)))
    (find-file path)
    (mu4e-org-mode)
    (setq buffer-read-only t)
    (define-key mu4e-org-mode-map (kbd "q")
      `(lambda ()
         (interactive)
         (bury-buffer)
         (switch-to-buffer ,curbuf)))))

(defun mu4e-about ()
  "Show the mu4e \"About\" page."
  (interactive)
  (mu4e-info (mu4e-join-paths mu4e-doc-dir "mu4e-about.org")))

(defun mu4e-news ()
  "Show news for the current version of mu and mu4e."
  (interactive)
  (mu4e-info (mu4e-join-paths mu4e-doc-dir "NEWS.org")))

(defun mu4e-ideas ()
  "Show development ideas for mu and mu4e."
  (interactive)
  (mu4e-info (mu4e-join-paths mu4e-doc-dir "IDEAS.org")))

(defun mu4e-baseline-time ()
  "Show the baseline time."
  (interactive)
  (mu4e-message "Baseline time: %s" (mu4e--baseline-time-string)))

(defun mu4e--baseline-time-string ()
  "Calculate the baseline time string."
  (let* ((baseline-t mu4e--query-items-baseline-tstamp)
         (updated-t (plist-get mu4e-index-update-status :tstamp))
         (delta-t (and baseline-t updated-t
                       (float-time (time-subtract updated-t baseline-t)))))
    (if (and delta-t (> delta-t 0))
        (format-seconds  "%Y %D %H %M %z%S since latest" delta-t)
      (if baseline-t
          (current-time-string baseline-t)
        "Never"))))

(defvar mu4e-main-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" #'mu4e-quit)
    (define-key map "C" #'mu4e-compose-new)

    (define-key map "m" #'mu4e--main-toggle-mail-sending-mode)
    (define-key map "f" #'smtpmail-send-queued-mail)
    ;;
    (define-key map  (kbd "C-S-u")   #'mu4e-update-mail-and-index)
    ;; for terminal users
    (define-key map  (kbd "C-c C-u") #'mu4e-update-mail-and-index)
    (define-key map "U" #'mu4e-update-mail-and-index)
    (define-key map "S" #'mu4e-kill-update-mail)
    (define-key map ";" #'mu4e-context-switch)
    (define-key map "$" #'mu4e-show-log)
    (define-key map "A" #'mu4e-about)
    (define-key map "N" #'mu4e-news)
    (define-key map "H" #'mu4e-display-manual)
    map)
  "Keymap for the *mu4e-main* buffer.")

(easy-menu-define mu4e-main-mode-menu
  mu4e-main-mode-map "Menu for mu4e's main view."
  (append
   '("Mu4e" ;;:visible mu4e-headers-mode
     "--"
     ["Update mail and index" mu4e-update-mail-and-index]
     ["Flush queued mail"     smtpmail-send-queued-mail]
     "--"
     ["Show debug log" mu4e-show-log]
     )
   mu4e--compose-menu-items
   mu4e--search-menu-items
   '(
     "--"
     ["Quit" mu4e-quit :help "Quit mu4e"])))

(declare-function mu4e--server-bookmarks-queries "mu4e")

(define-derived-mode mu4e-main-mode special-mode "mu4e:main"
  "Major mode for the mu4e main screen.

This mode is a bit special when it comes to keybinding, since it
shows those keybindings.

For the rebinding the mu4e functions (such as
`mu4e-search-bookmark' and `mu4e-search-maildir') to different
keys, note that mu4e determines the bindings when drawing the
screen, which is *after* we enable the mode. Thus, the
keybindings must be known when this happens.

Binding the existing bindings (such as \='s') to different
functions, is *not* really supported, and we still display the
default binding for the original function; which should still do
the reasonable thing in most cases.

Still, such a rebinding *only* affects the key, and not e.g. the
mouse-bindings."
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (mu4e-context-minor-mode)
  (mu4e-search-minor-mode)
  (mu4e-update-minor-mode)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                ;; reset the baseline and get updated results.
                (mu4e--query-items-refresh 'reset-baseline))))


(defun mu4e--main-action (title cmd &optional bindstr alt)
  "Produce main view action string with TITLE.

When activated, invoke interactive function CMD.

In the result, used the TITLE string, with the first occurrence
of [@] replaced by a textual replacement of a binding to CMD as
per `mu4e-key-description', or, if specified, BINDSTR.

If a string ALT is specified, and BINDSTR is longer than a single
character, use ALT as a substitute. ALT should be a string of
length 1.

If the first letter after the [@] is equal to the last letter of the
binding representation, remove that first letter."
  (let* ((bindstr (or bindstr (mu4e-key-description cmd) alt
                      (mu4e-error "No binding for %s" cmd)))
         (bindstr
          (if (and alt (> (length bindstr) 1)) alt bindstr))
         (title ;; remove first letter afrer [] if it equal last of binding
          (mu4e-string-replace
           (concat "[@]" (substring bindstr -1)) "[@]" title))
         (title ;; insert binding in [@]
          (mu4e-string-replace
           "[@]" (format "[%s]" (propertize bindstr 'face 'mu4e-highlight-face))
           title))
         (map (make-sparse-keymap)))
    (define-key map [mouse-2] cmd)
    (define-key map (kbd "RET") cmd)
    (propertize title 'keymap map)))

(defun mu4e--main-items (item-type max-length)
  "Produce the string with menu-items for ITEM-TYPE.
ITEM-TYPE is a symbol, either `bookmarks' or `maildirs'.

MAX-LENGTH is the maximum length of the item titles; this is used
for aligning them."
  (mapconcat
   (lambda (item)
     (cl-destructuring-bind
         (&key hide name key favorite query &allow-other-keys) item
       ;; hide items explicitly hidden, without key or wrong category.
       (if hide
           ""
         (let ((item-info
                ;; note, we have a function for the binding,
                ;; and perhaps a different one for the lambda.
                (cond
                 ((eq item-type 'maildirs)
                  (list #'mu4e-search-maildir #'mu4e-search
                        query))
                 ((eq item-type 'bookmarks)
                 (list #'mu4e-search-bookmark #'mu4e-search-bookmark
                        (mu4e-get-bookmark-query key)))
                 (t
                  (mu4e-error "Invalid item-type %s" item-type)))))
           (concat
            (mu4e--main-action
             ;; main title
             (format "\t* [@] %s "
                     (propertize
                      name
                      'face (if favorite 'mu4e-header-key-face nil)
                      'help-echo query))
             ;; function to call when activated
             (lambda () (interactive)
               (funcall (nth 1 item-info)
                        (nth 2 item-info)))
             ;; custom key binding string
             (concat (mu4e-key-description (nth 0 item-info)) (string key)))
            ;; counts
            (format "%s%s\n"
                    (make-string (- max-length (string-width name)) ?\s)
                    (mu4e--query-item-display-counts item)))))))
   ;; only items which have a single-character :key
   (mu4e-filter-single-key (mu4e-query-items item-type)) ""))

(defun mu4e--key-val (key val &optional unit)
  "Show a KEY / VAL pair, with optional UNIT."
  (concat
   "\t* "
   (propertize (format "%-20s" key) 'face 'mu4e-header-title-face)
   ": "
   (propertize val 'face 'mu4e-header-key-face)
   (if unit
       (propertize (concat " " unit) 'face 'mu4e-header-title-face)
     "")
   "\n"))

(defun mu4e--main-baseline-time-string ()
  "Calculate the baseline time string for use in the main-view."
  (let* ((baseline-t mu4e--query-items-baseline-tstamp)
         (updated-t (plist-get mu4e-index-update-status :tstamp))
         (delta-t (and baseline-t updated-t
                       (float-time (time-subtract updated-t baseline-t)))))
    (if (and delta-t (> delta-t 0))
        (format-seconds  "%Y %D %H %M %z%S ago" delta-t)
      (if baseline-t
          (current-time-string baseline-t)
        "Never"))))

(defun mu4e--main-redraw ()
  "Redraw the main buffer if there is one.
Otherwise, do nothing."
  (when-let* ((buffer (get-buffer mu4e-main-buffer-name))
              (buffer (and (buffer-live-p buffer) buffer)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (pos (point))
             (addrs (mu4e-personal-addresses))
             (max-length (seq-reduce (lambda (a b)
                                       (max a (length (plist-get b :name))))
                                     (mu4e-query-items) 0)))
        (mu4e-main-mode)
        (erase-buffer)
        (insert
         "* "
         (propertize "mu4e" 'face 'mu4e-header-key-face)
         (propertize " - mu for emacs version " 'face 'mu4e-title-face)
         (propertize  mu4e-mu-version 'face 'mu4e-header-key-face)
         "\n\n"
         (propertize "  Basics\n\n" 'face 'mu4e-title-face)
         (mu4e--main-action
          "\t* [@]jump to some maildir\n" #'mu4e-search-maildir nil "j")
         (mu4e--main-action
          "\t* enter a [@]search query\n" #'mu4e-search nil "s")
         (mu4e--main-action
          "\t* [@]Compose a new message\n" #'mu4e-compose-new nil "C")
         "\n"
         (propertize "  Bookmarks\n\n" 'face 'mu4e-title-face)
         (mu4e--main-items 'bookmarks max-length)
         "\n"
         (propertize "  Maildirs\n\n" 'face 'mu4e-title-face)
         (mu4e--main-items 'maildirs max-length)
         "\n"
         (propertize "  Misc\n\n" 'face 'mu4e-title-face)
         (mu4e--main-action "\t* [@]Choose query\n"
                            #'mu4e-search-query nil "c")
         (mu4e--main-action "\t* [@]Switch context\n"
                            #'mu4e-context-switch nil ";")
         (mu4e--main-action "\t* [@]Update email & database\n"
                            #'mu4e-update-mail-and-index nil "U")
         ;; show the queue functions if `smtpmail-queue-dir' is defined
         (if (file-directory-p smtpmail-queue-dir)
             (mu4e--main-view-queue)
           "")
         "\n"
         (mu4e--main-action "\t* [@]News\n" #'mu4e-news nil "N")
         (mu4e--main-action "\t* [@]About mu4e\n" #'mu4e-about nil "A")
         (mu4e--main-action "\t* [@]Help\n" #'mu4e-display-manual nil "H")
         (mu4e--main-action "\t* [@]quit\n" #'mu4e-quit nil "q")
         "\n"
         (propertize "  Info\n\n" 'face 'mu4e-title-face)
         (mu4e--key-val "last updated"
                        (current-time-string
                         (plist-get mu4e-index-update-status :tstamp)))
         (mu4e--key-val "database-path" (mu4e-database-path))
         (mu4e--key-val "maildir" (mu4e-root-maildir))
         (mu4e--key-val "in store"
                        (format "%d" (plist-get mu4e--server-props :doccount))
                        "messages")
         (if mu4e-main-hide-personal-addresses ""
           (mu4e--key-val "personal addresses"
                          (if addrs (mapconcat #'identity addrs ", "  ) "none"))))

        (if mu4e-main-hide-personal-addresses ""
          (unless (mu4e-personal-address-p user-mail-address)
            (mu4e-message (concat
                           "Tip: `user-mail-address' ('%s') is not part "
                           "of mu's addresses; add it with 'mu init
                        --my-address='") user-mail-address)))
        (goto-char pos)))))

(defun mu4e--main-view-queue ()
  "Display queue-related actions in the main view."
  (concat
   (mu4e--main-action "\t* toggle [@]mail sending mode "
                      #'mu4e--main-toggle-mail-sending-mode)
   "(currently "
   (propertize (if smtpmail-queue-mail "queued" "direct")
               'face 'mu4e-header-key-face)
   ")\n"
   (let ((queue-size (mu4e--main-queue-size)))
     (if (zerop queue-size)
         ""
       (mu4e--main-action
        (format "\t* [@]flush %s queued %s\n"
                (propertize (int-to-string queue-size)
                            'face 'mu4e-header-key-face)
                (if (> queue-size 1) "mails" "mail"))
        'smtpmail-send-queued-mail)))))

(defun mu4e--main-queue-size ()
  "Return, as an int, the number of emails in the queue."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents (expand-file-name smtpmail-queue-index-file
                                                smtpmail-queue-dir))
        (count-lines (point-min) (point-max)))
    (error 0)))

(declare-function mu4e--start "mu4e")

(defun mu4e--main-view ()
  "(Re)create the mu4e main-view, and switch to it.

If `mu4e-split-view' equals \\='single-window, show a mu4e menu
instead."
  (if (eq mu4e-split-view 'single-window)
      (mu4e--main-menu)
    (let ((buf (get-buffer-create mu4e-main-buffer-name))
          (inhibit-read-only t))
      (with-current-buffer buf
        (mu4e--main-redraw))
      (mu4e-display-buffer buf t)
      (run-hooks 'mu4e-main-rendered-hook)))
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;; Toggle mail sending mode without switching
(defun mu4e--main-toggle-mail-sending-mode ()
  "Toggle sending mail mode, either queued or direct."
  (interactive)
  (unless (file-directory-p smtpmail-queue-dir)
    (mu4e-error "`smtpmail-queue-dir' does not exist"))
  (setq smtpmail-queue-mail (not smtpmail-queue-mail))
  (message (concat "Outgoing mail will now be "
                   (if smtpmail-queue-mail "queued" "sent directly")))
  (unless (or (eq mu4e-split-view 'single-window)
              (not (buffer-live-p (get-buffer mu4e-main-buffer-name))))
    (mu4e--main-redraw)))

(defun mu4e--main-menu ()
  "The mu4e main menu in the mini-buffer."
  (let ((func (mu4e-read-option
               "Do: "
               '(("jump"	    . mu4e~headers-jump-to-maildir)
                 ("search"	    . mu4e-search)
                 ("Compose"	    . mu4e-compose-new)
                 ("bookmarks"	    . mu4e-search-bookmark)
                 (";Switch context" . mu4e-context-switch)
                 ("Update"	    . mu4e-update-mail-and-index)
                 ("News"	    . mu4e-news)
                 ("About"	    . mu4e-about)
                 ("Help "	    . mu4e-display-manual)))))
    (call-interactively func)
    (when (eq func 'mu4e-context-switch)
      (sit-for 1)
      (mu4e--main-menu))))

(provide 'mu4e-main)
;;; mu4e-main.el ends here
