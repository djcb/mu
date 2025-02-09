;;; mu4e-actions.el --- Actions for messages/attachments -*- lexical-binding: t -*-

;; Copyright (C) 2011-2023 Dirk-Jan C. Binnema

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

;; Example actions for messages, attachments (see chapter 'Actions' in the
;; manual)

;;; Code:

(require 'ido)
(require 'browse-url)

(require 'mu4e-helpers)
(require 'mu4e-message)
(require 'mu4e-search)
(require 'mu4e-contacts)
(require 'mu4e-lists)

;;; Count lines

(defun mu4e-action-count-lines (msg)
  "Count the number of lines in the e-mail MSG.
Works for headers view and message-view."
  (message "Number of lines: %s"
           (shell-command-to-string
            (concat "wc -l < "
                    (shell-quote-argument (mu4e-message-field msg :path))))))

;;; Org Helpers

(defvar mu4e-captured-message nil
  "The most recently captured message.")

(defun mu4e-action-capture-message (msg)
  "Remember MSG.
Later, we can create an attachment based on this message with
`mu4e-compose-attach-captured-message'."
  (setq mu4e-captured-message msg)
  (message "Message has been captured"))


(defun mu4e-action-copy-message-file-path (msg)
  "Save the full path for the current MSG to the kill ring."
  (kill-new (mu4e-message-field msg :path)))

(defvar mu4e-org-contacts-file nil
  "File to store contact information for org-contacts.
Needed by `mu4e-action-add-org-contact'.")

(eval-when-compile ;; silence compiler warning about free variable
  (unless (require 'org-capture nil 'noerror)
    (defvar org-capture-templates nil)))

(defun mu4e-action-add-org-contact (msg)
  "Add an org-contact based on the sender ddress of the current MSG.
You need to set `mu4e-org-contacts-file' to the full path to the
file where you store your org-contacts."
  (unless (require 'org-capture nil 'noerror)
    (mu4e-error "Feature org-capture is not available"))
  (unless mu4e-org-contacts-file
    (mu4e-error "Variable `mu4e-org-contacts-file' is nil"))
  (let* ((sender (car-safe (mu4e-message-field msg :from)))
         (name (mu4e-contact-name sender))
         (email (mu4e-contact-email sender))
         (blurb
          (format
           (concat
            "* %%?%s\n"
            ":PROPERTIES:\n"
            ":EMAIL: %s\n"
            ":NICK:\n"
            ":BIRTHDAY:\n"
            ":END:\n\n")
           (or name email "")
           (or email "")))
         (key "mu4e-add-org-contact-key")
         (org-capture-templates
          (append org-capture-templates
                  (list (list key "contacts" 'entry
                              (list 'file mu4e-org-contacts-file) blurb)))))
    (when (fboundp 'org-capture)
      (org-capture nil key))))

;;; Patches

(defvar mu4e--patch-directory-history nil
  "History of directories we have applied patches to.")

;; This essentially works around the fact that read-directory-name
;; can't have custom history.
(defun mu4e--read-patch-directory (&optional prompt)
  "Read a `PROMPT'ed directory name via `completing-read' with history."
  (unless prompt
    (setq prompt "Target directory:"))
  (file-truename
   (completing-read prompt 'read-file-name-internal #'file-directory-p
                    nil nil 'mu4e--patch-directory-history)))

(defun mu4e-action-git-apply-patch (msg)
  "Apply `MSG' as a git patch."
  (let ((path (mu4e--read-patch-directory "Target directory: ")))
    (let ((default-directory path))
      (shell-command
       (format "git apply %s"
               (shell-quote-argument (mu4e-message-field msg :path)))))))

(defun mu4e-action-git-apply-mbox (msg &optional signoff)
  "Apply `MSG' a git patch with optional `SIGNOFF'.

If the `default-directory' matches the most recent history entry don't
bother asking for the git tree again (useful for bulk actions)."

  (let ((cwd (substring-no-properties
              (or (car mu4e--patch-directory-history)
                  "not-a-dir"))))
    (unless (and (stringp cwd) (string= default-directory cwd))
      (setq cwd (mu4e--read-patch-directory "Target directory: ")))
    (let ((default-directory cwd))
      (shell-command
       (format "git am %s %s"
               (if signoff "--signoff" "")
               (shell-quote-argument (mu4e-message-field msg :path)))))))

;;; Tagging

(defvar mu4e-action-tags-header "X-Keywords"
  "Header where tags are stored.
Used by `mu4e-action-retag-message'. Make sure it is one of the
headers mu recognizes for storing tags: X-Keywords, X-Label,
Keywords. Also note that changing this setting on already tagged
messages can lead to messages with multiple tags headers.")

(defvar mu4e-action-tags-completion-list '()
  "List of tags for completion in `mu4e-action-retag-message'.")

(defun mu4e--contains-line-matching (regexp path)
  "Return non-nil if the file at PATH contain a line matching REGEXP.
Otherwise return nil."
  (with-temp-buffer
    (insert-file-contents path)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun mu4e--replace-first-line-matching (regexp to-string path)
  "Replace first line matching REGEXP in PATH with TO-STRING."
  (with-temp-file path
    (insert-file-contents path)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (replace-match to-string t nil)))))

(declare-function mu4e--server-add "mu4e-server")
(defun mu4e--refresh-message (path)
  "Re-parse message at PATH.
if this works, we will
receive (:info add :path <path> :docid <docid>) as well as (:update
<msg-sexp>)."
  (mu4e--server-add path))

(defun mu4e-action-retag-message (msg &optional retag-arg)
  "Change tags of MSG with RETAG-ARG.

RETAG-ARG is a comma-separated list of additions and removals.

Example: +tag,+long tag,-oldtag
would add \"tag\" and \"long tag\", and remove \"oldtag\"."
  (let* (
         (path (mu4e-message-field msg :path))
         (oldtags (mu4e-message-field msg :tags))
         (tags-completion
          (append
           mu4e-action-tags-completion-list
           (mapcar (lambda (tag) (format "+%s" tag))
                   mu4e-action-tags-completion-list)
           (mapcar (lambda (tag) (format "-%s" tag))
                   oldtags)))
         (retag (if retag-arg
                    (split-string retag-arg ",")
                  (completing-read-multiple "Tags: " tags-completion)))
         (header  mu4e-action-tags-header)
         (sep     (cond ((string= header "Keywords") ", ")
                        ((string= header "X-Label") " ")
                        ((string= header "X-Keywords") ", ")
                        (t ", ")))
         (taglist (if oldtags (copy-sequence oldtags) '()))
         tagstr)
    (dolist (tag retag taglist)
      (cond
       ((string-match "^\\+\\(.+\\)" tag)
        (setq taglist (push (match-string 1 tag) taglist)))
       ((string-match "^\\-\\(.+\\)" tag)
        (setq taglist (delete (match-string 1 tag) taglist)))
       (t
        (setq taglist (push tag taglist)))))

    (setq taglist (delete "" (sort (delete-dups taglist) 'string<)))
    (setq tagstr (mapconcat 'identity taglist sep))

    (setq tagstr (replace-regexp-in-string "[\\&]" "\\\\\\&" tagstr))
    (setq tagstr (replace-regexp-in-string "[/]"   "\\&" tagstr))

    (if (not (mu4e--contains-line-matching (concat header ":.*") path))
        ;; Add tags header just before the content
        (mu4e--replace-first-line-matching
         "^$" (concat header ": " tagstr "\n") path)

      ;; replaces keywords, restricted to the header
      (mu4e--replace-first-line-matching
       (concat header ":.*")
       (concat header ": " tagstr)
       path))

    (mu4e-message (concat "tagging: " (mapconcat 'identity taglist ", ")))
    (mu4e--refresh-message path)))

(defun mu4e-action-show-thread (msg)
  "Show thread for message at point with point remaining on MSG.
I.e., point remains on the message with the message-id where the
action was invoked. If invoked in view mode, continue to display
the message."
  (let ((msgid (mu4e-message-field msg :message-id)))
    (when msgid
      (let ((mu4e-search-threads t)
            (mu4e-search-include-related t))
        (mu4e-search
         (format "msgid:%s" msgid)
         nil nil nil
         msgid (and (eq major-mode 'mu4e-view-mode)
                    (not (eq mu4e-split-view 'single-window))))))))


;;; Mailing list URLS

(defun mu4e-action-browse-list-archive (msg)
  "Browse the archive for a mailing list message MSG.
See `mu4e-mailing-list-archive-url'."
  (interactive (list (mu4e-message-at-point)))
  (if-let* ((url (mu4e-mailing-list-archive-url msg)))
    (browse-url url)
    (mu4e-warn "No archive available for this message")))

(defun mu4e-action-copy-list-archive-url (msg)
  "Copy the archive url for a mailing list message MSG.
See `mu4e-mailing-list-archive-url'."
  (interactive (list (mu4e-message-at-point)))
  (let ((url (mu4e-mailing-list-archive-url msg)))
    (if (stringp url)
        (kill-new url)
      (mu4e-warn "Cannot get archive URL for this message"))))

;;;
(provide 'mu4e-actions)
;;; mu4e-actions.el ends here
