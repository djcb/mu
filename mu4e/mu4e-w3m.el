;;; mu4e-w3m.el -- part of mu4e, the mu mail user agent
;;
;; Copyright © 2013-2013 David Leatherman, Dirk-Jan C. Binnema

;; Author: David Leatherman <leathekd@gmail.com>
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

;; In this file we define an alternative renderer for mu4e-view
;; To use this renderer add the following to your mu4e config
;; (require 'mu4e-w3m)
;; (setq mu4e-view-render-func 'mu4e-w3m-render-handler)
;;
;; You might also consider setting `mu4e-view-prefer-html' or
;; `mu4e-view-html-plaintext-ratio-heuristic' to better allow w3m to
;; do the rendering.

;;; Code:
(require 'mu4e-view)

(require 'w3m)
(require 'w3m-lnum)

(defvar mu4e-w3m-fill-column -1
  "*Integer used as the value for `fill-column' in mu4e-w3m buffers.
See the documentation for `w3m-fill-column' for more information.")

(defun mu4e-w3m-body-text (msg)
  "Get the body in text form for this message.  This is either
:body-html or :body-txt.  If html is not available, non-nil, it will
use the text part. Normally, function prefers the html part, unless
the html part is \"too small\", but this can be changed by setting
`mu4e-view-prefer-html'."
  (let* ((txt (mu4e-message-field msg :body-txt))
         (html (mu4e-message-field msg :body-html))
         (body (cond
                ;; does it look like some text? ie., 10x the length of the text
                ;; should be longer than the html, an heuristic to guard against
                ;; 'This messages requires html' text bodies.
                ((and (> (* mu4e-view-html-plaintext-ratio-heuristic
                            (length txt))
                         (length html))
                      ;; use html if it's prefered, unless there is no html
                      (or (not mu4e-view-prefer-html) (not html)))
                 txt)
                ;; it there some html?
                (html html)
                (t ;; otherwise, an empty body
                 ""))))
    (mu4e-message-clean-body-text body)))

(defun mu4e-w3m-message-text (msg)
  "Return the message to display (as a string), based on the MSG plist."
  (concat (mu4e-view-header-text msg)
          "\n"
          (mu4e-w3m-body-text msg)))

(defun mu4e-w3m-replace-in-buffer (old-str new-str &optional start-pt)
  "Utility function to replace strings in a buffer, optionally
starting at START-PT rather than (point-min)"
  (let ((start-pt (or start-pt (point-min))))
    (goto-char start-pt)
    (while (search-forward-regexp old-str nil t)
      (replace-match new-str nil t))))

(defun mu4e-w3m-render-body ()
  "Given a buffer populated by the rendered headers and unrendered
body, render the body with w3m but leave the headers alone."
  (save-excursion
    (goto-char (point-min))
    (let ((w3m-display-inline-images mu4e-view-show-images)
          (w3m-fill-column mu4e-w3m-fill-column)
          ;; search the end of the headers
          (start-pt (search-forward-regexp "^\n")))
      (when start-pt
        ;; &nbsp renders incorrectly, swap them out with space
        (mu4e-w3m-replace-in-buffer " " " ")
        (w3m-region start-pt (point-max))
        (mu4e~view-fontify-cited)))))

(defun mu4e-w3m-render-handler (msg)
  "Render the given message.  This function is called inside the view buffer."
  (mu4e-w3m-view-mode)
  (insert (mu4e-w3m-message-text msg))
  (goto-char (point-min))
  (mu4e-w3m-render-body))

(defun mu4e-w3m-browse-current-url ()
  (interactive)
  (funcall (mu4e~view-browse-url-func (w3m-anchor))))

(defun mu4e-w3m-go-to-url ()
  (interactive)
  (let ((url (w3m-url-valid (car (w3m-lnum-get-action
                                  "Visit url with number: " 1)))))
    (if url
        (funcall (mu4e~view-browse-url-func url))
      (message "No URL selected"))))

(defvar mu4e-w3m-view-mode-map nil
  "A composed keymap containing all of the mappings in
`mu4e-view-mode-map', `w3m-minor-mode-map', and a few
mu4e-w3m-specific bindings.")
(unless mu4e-w3m-view-mode-map
  (let ((map (make-composed-keymap (w3m-make-minor-mode-keymap)
                                   mu4e-w3m-view-mode-map)))
    ;; define "u" here to override w3m-minor-mode
    (define-key map "u" 'mu4e-view-unmark)
    (define-key map (kbd "RET") 'mu4e-scroll-up)
    (define-key map "g" 'mu4e-w3m-go-to-url)
    (define-key map (kbd "M-RET") 'mu4e-w3m-browse-current-url)
    (setq mu4e-w3m-view-mode-map map)))

(define-derived-mode mu4e-w3m-view-mode mu4e-view-mode "mu4e:w3m"
  "Major mode for viewing an e-mail message in mu4e.
\\{mu4e-w3m-view-mode-map}.")

(provide 'mu4e-w3m)
