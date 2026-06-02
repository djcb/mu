;;; mu4e-org --- Org-links to mu4e messages/queries -*- lexical-binding: t -*-

;; Copyright (C) 2012-2026 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of 1the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The expect version here is org 9.x.

;;; Code:

(require 'org)
(require 'tabulated-list)
(require 'mu4e-view)
(require 'mu4e-contacts)

(defgroup mu4e-org nil
  "Settings for the Org mode related functionality in mu4e."
  :group 'mu4e
  :group 'org)

(defcustom mu4e-org-link-desc-func
  (lambda (msg) (or (plist-get msg :subject) "No subject"))
  "Function that takes a msg and returns a description.
This can be used in org capture templates and storing links.

Example usage:

  (defun my-link-descr (msg)
    (let ((subject (or (plist-get msg :subject)
                       \"No subject\"))
          (date (or (format-time-string mu4e-headers-date-format
                    (mu4e-msg-field msg :date))
                    \"No date\")))
      (concat subject \" \" date)))

  (setq mu4e-org-link-desc-func \\='my-link-descr)"
  :type '(function)
  :group 'mu4e-org)

(defvar mu4e-org-link-query-in-headers-mode nil
  "Prefer linking to the query rather than to the message.
If non-nil, `org-store-link' in `mu4e-headers-mode' links to the
the current query; otherwise, it links to the message at point.")

;; backward compat until org >= 9.3 is universal.
(defalias 'mu4e--org-link-store-props
  (if (fboundp 'org-link-store-props)
      #'org-link-store-props
    (with-no-warnings
      #'org-store-link-props)))

(defun mu4e--org-store-link-query ()
  "Store a link to a mu4e query."
  (setq org-store-link-plist nil)       ; reset
  (mu4e--org-link-store-props
   :type        "mu4e"
   :query       (mu4e-last-query)
   :date        (format-time-string "%FT%T") ;; avoid error
   :link        (concat "mu4e:query:" (mu4e-last-query))
   :description (format "[%s]" (mu4e-last-query))))

(defun mu4e--org-store-link-message (&optional msg)
  "Store a link to a mu4e message.
If MSG is non-nil, store a link to MSG, otherwise use `mu4e-message-at-point'."
  (setq org-store-link-plist nil)
  (let* ((msg      (or msg (mu4e-message-at-point)))
         (from     (car-safe (plist-get msg :from)))
         (to       (car-safe (plist-get msg :to)))
         (date     (format-time-string "%FT%T" (plist-get msg :date)))
         (msgid    (or (plist-get msg :message-id)
                       (mu4e-error "Cannot link message without message-id")))
         (props `(:type  "mu4e"
                  :date              ,date
                  :from              ,(mu4e-contact-full from)
                  :fromname          ,(mu4e-contact-name from)
                  :fromnameoraddress ,(or (mu4e-contact-name from)
                                        (mu4e-contact-email from)) ;; mu4e-specific
                  :maildir           ,(plist-get msg :maildir)
                  :message-id        ,msgid
                  :path              ,(plist-get msg :path)
                  :subject           ,(plist-get msg :subject)
                  :to                ,(mu4e-contact-full to)
                  :tonameoraddress   ,(or (mu4e-contact-name to)
                                        (mu4e-contact-email to)) ;; mu4e-specific
                  :link              ,(concat "mu4e:msgid:" msgid)
                  :description       ,(funcall mu4e-org-link-desc-func msg))))
    (apply #'mu4e--org-link-store-props props)))

;;;###autoload
(defun mu4e-org-store-link ()
  "Store a link to a mu4e message or query.
It links to the last known query when in `mu4e-headers-mode' with
`mu4e-org-link-query-in-headers-mode' set; otherwise it links to
a specific message, based on its message-id, so that links stay
valid even after moving the message around."
  (cond
   ((derived-mode-p 'mu4e-view-mode) (mu4e--org-store-link-message))
   ((derived-mode-p 'mu4e-headers-mode)
    (if mu4e-org-link-query-in-headers-mode
        (mu4e--org-store-link-query)
      (mu4e--org-store-link-message)))))

(declare-function mu4e "mu4e")

;;;###autoload
(defun mu4e-org-open (link)
  "Open the org LINK.
Open the mu4e message (for links starting with \"msgid:\") or run
the query (for links starting with \"query:\")."
  (require 'mu4e)
  (mu4e 'background)
  (cond
   ((string-match "^msgid:\\(.+\\)" link)
    (mu4e-view-message-with-message-id (match-string 1 link)))
   ((string-match "^query:\\(.+\\)" link)
    (mu4e-search (match-string 1 link) current-prefix-arg))
   (t (mu4e-error "Unrecognized link type '%s'" link))))

;;;###autoload
(defun mu4e-org-store-and-capture ()
  "Store a link to the current message or query.
\(depending on `mu4e-org-link-query-in-headers-mode', and capture
it with org)."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture))

;; install mu4e-link support.
(org-link-set-parameters "mu4e"
                         :follow #'mu4e-org-open
                         :store  #'mu4e-org-store-link)


(defun mu4e-org-links-in-buffer (&optional file)
  "List all mu4e:msgid links in the current `org-mode' buffer.

The list elements are plists with:
 :link    the org-link
 :msgid   the message-id it refers to
 :context a string describing the org item
 :file    the FILE in which this link was found

If FILE is non-nil:
  :start the start position of the link
plists."
  (save-excursion
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-element-map (org-element-parse-buffer) 'link
       (lambda (link)
         (let* ((type (org-element-property :type link))
                (raw-link (org-element-property :raw-link link))
                (start (org-element-property :contents-begin link))
                (path (org-element-property :path link))
                (msgid "msgid:"))
           ;; a little hacky; seems org-mode doesn't have something built-in for
           ;; this.
           (when (string= type "mu4e")
             (when-let* ((msgid (when (string-prefix-p msgid path)
                                  (substring path (length msgid)))))
               (let* ((begin (org-element-property :begin link))
                      (context-line
                       (save-excursion
                         (goto-char begin)
                         (org-link-display-format
                          (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))))
                      (info (list :link raw-link
                                  :msgid msgid
                                  :context context-line
                                  :start start))
                      (info
                       (if file
                           (plist-put info :file file)
                         info)))
                 info)))))
       nil nil nil nil t))))

(defun mu4e-org-links-in-agenda-files ()
  "List all mu4e:msgid: links in `org-agenda-files'.

The list elements are plists as per `mu4e-org-links-in-buffer'.."
  (let ((msgid-links '()))
    (dolist (file org-agenda-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (when-let* ((buflinks (mu4e-org-links-in-buffer file)))
            (setq msgid-links (append msgid-links buflinks))))))
    msgid-links))

(defcustom mu4e-org-agenda-links-context-length 60
  "Maximum length of the Context column in `mu4e-org-agenda-links'.
Longer context strings are ellipsized to fit."
  :type 'integer
  :group 'mu4e-org)

(defvar-keymap mu4e-org-agenda-links-mode-map
  :doc "Keymap for `mu4e-org-agenda-links-mode'."
  :parent tabulated-list-mode-map
  "RET" #'mu4e-org-agenda-links-follow
  "o"   #'mu4e-org-agenda-links-visit-file)

(define-derived-mode mu4e-org-agenda-links-mode tabulated-list-mode
  "mu4e-org-agenda-links"
  "Major mode for listing mu4e:msgid links found in `org-agenda-files'."
  (setq tabulated-list-format
        (vector
         (list "Context" mu4e-org-agenda-links-context-length nil)
         (list "File"    25 t)))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun mu4e-org-agenda-links--entry-at-point ()
  "Return the link entry at point, or signal an error."
  (or (tabulated-list-get-id)
      (mu4e-error "No entry at point")))

(defun mu4e-org-agenda-links-follow (&optional other-window)
  "Follow the mu4e link at point.
With prefix arg OTHER-WINDOW, display the result in another window."
  (interactive "P")
  (let ((link (plist-get (mu4e-org-agenda-links--entry-at-point) :link)))
    (if other-window
        (let ((display-buffer-overriding-action
               '((display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (inhibit-same-window . t))))
          (org-link-open-from-string link))1
      (org-link-open-from-string link))))

(defun mu4e-org-agenda-links-visit-file (&optional other-window)
  "Visit the org file for the link at point, at its source location.
With prefix arg OTHER-WINDOW, visit in another window."
  (interactive "P")
  (let* ((entry (mu4e-org-agenda-links--entry-at-point))
         (file  (plist-get entry :file))
         (start (plist-get entry :start)))
    (unless (and file (file-exists-p file))
      (mu4e-error "File not found: %s" file))
    (if other-window
        (find-file-other-window file)
      (find-file file))
    (when start (goto-char start))))

;;;###autoload
(defun mu4e-org-agenda-links ()
  "Show mu4e:msgid links found in `org-agenda-files'.
Display a tabulated-list buffer with Context and File columns."
  (interactive)
  (let ((buf (get-buffer-create "*mu4e-org-agenda-links*")))
    (with-current-buffer buf
      (mu4e-org-agenda-links-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (entry)
               (let* ((file    (or (plist-get entry :file) ""))
                      (context (or (plist-get entry :context) ""))
                      (short   (truncate-string-to-width
                                context
                                mu4e-org-agenda-links-context-length
                                nil nil t)))
                 (list entry
                       (vector
                        (propertize short 'face 'mu4e-link-face)
                        (propertize (file-name-nondirectory file)
                                    'face 'mu4e-title-face)))))
             (mu4e-org-links-in-agenda-files)))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(provide 'mu4e-org)
;;; mu4e-org.el ends here
