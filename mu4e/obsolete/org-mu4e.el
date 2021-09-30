;;; org-mu4e -- support for links to mu4e messages and writing org-mode messages -*- lexical-binding: t -*-

;; Copyright (C) 2012-2019 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail
;; Version: 0.0

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

;; OBSOLETE, UNSUPPORTED.

;; Support for links to mu4e messages/queries from within org-mode,
;; and for writing message in org-mode, sending them as rich-text.

;; At least version 8.x of Org mode is required.

;;; Code:

(require 'org)
(require 'mu4e-compose)

(declare-function mu4e-last-query                   "mu4e-headers")
(declare-function mu4e-message-at-point             "mu4e-message")
(declare-function mu4e-view-message-with-message-id "mu4e-view")
(declare-function mu4e-headers-search               "mu4e-headers")
(declare-function mu4e-error                        "mu4e-helpers")
(declare-function mu4e-message                      "mu4e-message")
(declare-function mu4e-compose-mode                 "mu4e-compose")


;;; Editing with org-mode
;;
;; below, some functions for the org->html conversion
;; based on / inspired by Eric Schulte's org-mime.el
;; Homepage: http://orgmode.org/worg/org-contrib/org-mime.php
;;
;; EXPERIMENTAL

(defvar org-export-skip-text-before-1st-heading)
(defvar org-export-htmlize-output-type)
(defvar org-export-preserve-breaks)
(defvar org-export-with-LaTeX-fragments)

(defun org~mu4e-mime-file (ext path id)
  "Create a file of type EXT at PATH with ID for an attachment."
  (format (concat "<#part type=\"%s\" filename=\"%s\" "
                  "disposition=inline id=\"<%s>\">\n<#/part>\n")
          ext path id))

(defun org~mu4e-mime-multipart (plain html &optional images)
  "Create a multipart/alternative with PLAIN and HTML alternatives.
If the html portion of the message includes IMAGES, wrap the html
and images in a multipart/related part."
  (concat "<#multipart type=alternative><#part type=text/plain>"
          plain
          (when images "<#multipart type=related>")
          "<#part type=text/html>"
          html
          images
          (when images "<#/multipart>\n")
          "<#/multipart>\n"))

(defun org~mu4e-mime-replace-images (str current-file)
  "Replace images in html files STR in CURRENT-FILE with cid links."
  (let (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"cid:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (expand-file-name
                       url (file-name-directory current-file)))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
           (cl-pushnew (org~mu4e-mime-file
                        (concat "image/" ext) path id)
                       html-images
                       :test 'equal)
           id)))
      str)
     html-images)))

(defun org~mu4e-mime-convert-to-html ()
  "Convert the current body to html."
  (unless (fboundp 'org-export-string-as)
    (mu4e-error "Required function 'org-export-string-as not found"))
  (let* ((begin
          (save-excursion
            (goto-char (point-min))
            (search-forward mail-header-separator)))
         (end (point-max))
         (raw-body (buffer-substring begin end))
         (tmp-file (make-temp-name (expand-file-name "mail"
                                                     temporary-file-directory)))
         ;; because we probably don't want to skip part of our mail
         (org-export-skip-text-before-1st-heading nil)
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks t)
         ;; dvipng for inline latex because MathJax doesn't work in mail
         (org-export-with-LaTeX-fragments
          (if (executable-find "dvipng") 'dvipng
            (mu4e-message "Cannot find dvipng, ignore inline LaTeX") nil))
         ;; to hold attachments for inline html images
         (html-and-images
          (org~mu4e-mime-replace-images
           (org-export-string-as raw-body 'html t)
           tmp-file))
         (html-images (cdr html-and-images))
         (html (car html-and-images)))
    (delete-region begin end)
    (save-excursion
      (goto-char begin)
      (newline)
      (insert (org~mu4e-mime-multipart
               raw-body html (mapconcat 'identity html-images "\n"))))))

;; next some functions to make the org/mu4e-compose-mode switch as smooth as
;; possible.
(defun org~mu4e-mime-decorate-headers ()
  "Make the headers visually distinctive (org-mode)."
  (save-excursion
    (goto-char (point-min))
    (let* ((eoh (when (search-forward mail-header-separator)
                  (match-end 0)))
           (olay (make-overlay (point-min) eoh)))
      (when olay
        (overlay-put olay 'face 'font-lock-comment-face)))))

(defun org~mu4e-mime-undecorate-headers ()
  "Don't make the headers visually distinctive.
\(well, mu4e-compose-mode will take care of that)."
  (save-excursion
    (goto-char (point-min))
    (let* ((eoh (when (search-forward mail-header-separator)
                  (match-end 0))))
      (remove-overlays (point-min) eoh))))

(defvar org-mu4e-convert-to-html nil
  "Whether to do automatic `org-mode' => html conversion when sending messages.")

(defun org~mu4e-mime-convert-to-html-maybe ()
  "Convert to html if `org-mu4e-convert-to-html' is non-nil.
This function is called when sending a message (from
`message-send-hook') and, if non-nil, sends the message as the
rich-text version of what is assumed to be an org mode body."
  (when org-mu4e-convert-to-html
    (mu4e-message "Converting to html")
    (org~mu4e-mime-convert-to-html)))

(defun org~mu4e-mime-switch-headers-or-body ()
  "Switch the buffer to either mu4e-compose-mode (when in headers)
or org-mode (when in the body)."
  (interactive)
  (let* ((sepapoint
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp mail-header-separator nil t))))
    ;; only do stuff when the sepapoint exist; note that after sending the
    ;; message, this function maybe called on a message with the sepapoint
    ;; stripped. This is why we don't use `message-point-in-header'.
    (when sepapoint
      (cond
       ;; we're in the body, but in mu4e-compose-mode?
       ;; if so, switch to org-mode
       ((and (> (point) sepapoint) (eq major-mode 'mu4e-compose-mode))
        (org-mode)
        (add-hook 'before-save-hook
                  #'org~mu4e-error-before-save-hook-fn
                  nil t)
        (org~mu4e-mime-decorate-headers)
        (local-set-key (kbd "M-m")
                       (lambda (keyseq)
                         (interactive "kEnter mu4e-compose-mode key sequence: ")
                         (let ((func (lookup-key mu4e-compose-mode-map keyseq)))
                           (if func (funcall func) (insert keyseq))))))
       ;; we're in the headers, but in org-mode?
       ;; if so, switch to mu4e-compose-mode
       ((and (<= (point) sepapoint) (eq major-mode 'org-mode))
        (org~mu4e-mime-undecorate-headers)
        (mu4e-compose-mode)
        (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe nil t)))
      ;; and add the hook
      (add-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t t))))

(defun org~mu4e-error-before-save-hook-fn ()
  (mu4e-error "Switch to mu4e-compose-mode (M-m) before saving"))

(defun org-mu4e-compose-org-mode ()
  "Defines a pseudo-minor mode for mu4e-compose-mode.
Edit the message body using org mode. DEPRECATED."
  (interactive)
  (unless (member major-mode '(org-mode mu4e-compose-mode))
    (mu4e-error "Need org-mode or mu4e-compose-mode"))
  ;; we can check if we're already in org-mu4e-compose-mode by checking if the
  ;; post-command-hook is set; hackish...but a buffer-local variable does not
  ;; seem to survive buffer switching
  (if (not (member 'org~mu4e-mime-switch-headers-or-body post-command-hook))
      (progn
        (org~mu4e-mime-switch-headers-or-body)
        (mu4e-message
         (concat
          "org-mu4e-compose-org-mode enabled; "
          "press M-m before issuing message-mode commands")))
    (progn ;; otherwise, remove crap
      (remove-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t)
      (org~mu4e-mime-undecorate-headers) ;; shut off org-mode stuff
      (mu4e-compose-mode)
      (message "org-mu4e-compose-org-mode disabled"))))

;;; _
(provide 'org-mu4e)
;;; org-mu4e.el ends here
