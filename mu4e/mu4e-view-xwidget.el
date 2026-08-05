;;; mu4e-view-xwidget.el -- part of mu4e  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dirk-Jan C. Binnema

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

;; Xwidget-based mu4e message view. We try to make it behave like a "normal"
;; message view buffer (or shr), though do not completely succeed.

;;; Code:

(require 'gnus-art)
(require 'mm-decode)
(require 'mu4e-view-html)

(when (featurep 'xwidget-internal)
  (require 'xwidget))

;; Keep byte-compiler happy
(declare-function xwidget-webkit-new-session "xwidget" (url))
(declare-function set-xwidget-query-on-exit-flag "xwidget"
  (xwidget flag))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-perform-lispy-event "xwidget.c"
  (xwidget event &optional frame))
(declare-function xwidget-webkit-zoom-in "xwidget" ())
(declare-function xwidget-webkit-zoom-out "xwidget" ())

(defvar xwidget-webkit-disable-javascript)
(defvar xwidget-webkit-last-session-buffer)

;; Forward decls
(defvar mu4e-view-html-renderer)
(defvar mu4e-view-prefer-plain-text)
(defvar mu4e-view-rendered-hook)
(defvar mu4e--view-message)
(defvar mu4e--view-renderer-override nil
  "Override the renderer chosen by `mu4e-view-html-renderer'.")
(defvar-local mu4e--view-xwidget-tmp-file nil "Temp HTML file.")
(defvar-local mu4e--view-xwidget-buffer nil "Xwidget webkit buffer")
(defvar-local mu4e--view-xwidget-view-buffer nil
  "Mu4e view buffer associated with xwidget view.")
(defvar mu4e--view-gnus-article-mime-handles)
(declare-function mu4e-message "mu4e-helpers" (frm &rest args))
(declare-function mu4e-warn "mu4e-helpers" (frm &rest args))
(declare-function mu4e-xwidget-usable-p "mu4e-helpers" (&optional ignore-display))
(declare-function mu4e-view-refresh "mu4e-view" ())
(declare-function mu4e-view-quit "mu4e-view" ())
(declare-function mu4e-view-toggle-html "mu4e-view" ())
(declare-function mu4e-view-action "mu4e-view" (&optional msg))
(declare-function mu4e-view-save-attachments "mu4e-mime-parts" (&optional ask-dir))
(declare-function mu4e--view-render-buffer "mu4e-view" (msg))
(declare-function mu4e--view-raw-plain-preferred-p "mu4e-view-html" ())
(declare-function mu4e--view-html-insert-headers "mu4e-view" (msg))
(declare-function mu4e--view-buffer-cleanup "mu4e-mime-parts" ())

(defun mu4e--view-raw-html-p ()
  "Return non-nil if the raw message in the current buffer has html."
  (save-excursion
    (goto-char (point-min))
    (let* ((ct (mail-fetch-field "Content-Type"))
           (ct (and ct (mail-header-parse-content-type ct))))
      (or (equal (car ct) "text/html")
          (and (stringp (car ct))
               (string-prefix-p "multipart/" (car ct))
               (re-search-forward
                (rx bol "content-type:" (* blank) "text/html") nil t))))))

(defun mu4e--view-use-xwidget-p ()
  "Return non-nil if the current message should use xwidget rendering.
Call while raw message is in the current buffer."
  (pcase mu4e--view-renderer-override
    ('text nil)
    ('xwidget (mu4e-xwidget-usable-p))
    (_ (and (eq mu4e-view-html-renderer 'xwidget)
            (mu4e-xwidget-usable-p)
            (mu4e--view-raw-html-p)
            (not (and mu4e-view-prefer-plain-text
                      (mu4e--view-raw-plain-preferred-p)))))))

;;; MIME handle helpers

(defun mu4e--view-xwidget-build-handle-alist (handles &optional index)
  "Build a `gnus-article-mime-handle-alist' from HANDLES.
INDEX is the starting index (default 1).
Returns an alist of (INDEX . HANDLE) pairs."
  (let ((idx (or index 1)))
    (cond
     ((not (listp handles)) nil)
     ((bufferp (car handles))
      (list (cons idx handles)))
     (t (let (result)
          (dolist (h (cdr handles))
            (let ((sub (mu4e--view-xwidget-build-handle-alist h idx)))
              (setq result (nconc result sub))
              (setq idx (+ idx (length sub)))))
          result)))))

(defun mu4e--view-xwidget-setup-mime-handles ()
  "Dissect the raw message in the current buffer for attachment operations.
Populates `gnus-article-mime-handle-alist' and
`mu4e--view-gnus-article-mime-handles', and arranges for cleanup."
  (let ((handles (mm-dissect-buffer t t)))
    (setq gnus-article-mime-handle-alist
          (mu4e--view-xwidget-build-handle-alist handles))
    (setq mu4e--view-gnus-article-mime-handles
          (if (listp (car handles)) handles (list handles)))
    (add-hook 'kill-buffer-hook #'mu4e--view-buffer-cleanup nil t)))

;;; Keys

;; The xwidget scroll commands work by running JavaScript in the page, but we
;; disable javascript... so try to work around that by sending keys.

(defun mu4e--view-xwidget-send-key (key)
  "Send KEY, a lispy event such as `next', to this buffer's WebKit widget."
  (when-let* ((xw (xwidget-at (point-min))))
    (xwidget-perform-lispy-event xw key)))

(defun mu4e--view-xwidget-page-down ()
  "Scroll message down by a page."
  (interactive)
  (mu4e--view-xwidget-send-key 'next))

(defun mu4e--view-xwidget-page-up ()
  "Scroll message up by a page."
  (interactive)
  (mu4e--view-xwidget-send-key 'prior))

(defun mu4e--view-xwidget-line-down ()
  "Scroll message down by a line."
  (interactive)
  (mu4e--view-xwidget-send-key 'down))

(defun mu4e--view-xwidget-line-up ()
  "Scroll message up by a line."
  (interactive)
  (mu4e--view-xwidget-send-key 'up))

;; The keys above are the ones that need xwidget-specific handling. The rest of
;; `mu4e-view-mode-map' + minor modes are forwarded to that buffer instead

(defun mu4e--view-xwidget-run-in-view-buffer (cmd)
  "Run CMD interactively with the mu4e-view buffer as current."
  (if-let* ((view-buf (mu4e--view-xwidget-view-buffer)))
      (with-current-buffer view-buf
        (call-interactively cmd))
    (mu4e-warn "No message view")))

(defun mu4e--view-xwidget-delegate (cmd)
  "Return a command that runs CMD in the mu4e-view buffer.
See `mu4e--view-xwidget-forward-keys'."
  (lambda ()
    (interactive)
    (mu4e--view-xwidget-run-in-view-buffer cmd)))

(defun mu4e--view-xwidget-forward-keys (map source)
  "Bind, in MAP, the simple keys SOURCE binds that MAP does not yet."
  (map-keymap-internal
   (lambda (event cmd)
     (unless (or (keymapp cmd) (not (commandp cmd)))
       (dolist (ev (if (consp event) ;; a range of chars sharing a binding
                      (number-sequence (car event) (cdr event))
                    (list event)))
         (let ((key (vector ev)))
           (unless (lookup-key map key)
             (define-key map key (mu4e--view-xwidget-delegate cmd)))))))
   source))

(defun mu4e--view-xwidget-setup-keys ()
  "Bind the mu4e view keys we support in the current xwidget buffer."
  (let ((map (make-sparse-keymap))
        (webkit-map (current-local-map)))
    ;; not `mu4e-view-toggle-html'; that depends on `mu4e-view-html-renderer'.
    (define-key map "h" #'mu4e--view-xwidget-toggle)
    (define-key map "q" #'mu4e--view-xwidget-quit)
    (dolist (key '("SPC" "<remap> <scroll-up-command>"))
      (define-key map (kbd key) #'mu4e--view-xwidget-page-down))
    (dolist (key '("S-SPC" "DEL" "<remap> <scroll-down-command>"))
      (define-key map (kbd key) #'mu4e--view-xwidget-page-up))
    (define-key map [remap next-line] #'mu4e--view-xwidget-line-down)
    (define-key map [remap previous-line] #'mu4e--view-xwidget-line-up)
    ;; `mu4e-scroll-up' would scroll the (hidden) view buffer
    (define-key map (kbd "RET") #'mu4e--view-xwidget-line-down)
    ;; uncomment to keep zooming, rather than flag/unflag
    ;; (define-key map "+" #'xwidget-webkit-zoom-in)
    ;; (define-key map "-" #'xwidget-webkit-zoom-out)
    ;; override some bindings...
    (define-key map "g" (mu4e--view-xwidget-delegate #'mu4e-view-go-to-url))
    (define-key map "k" (mu4e--view-xwidget-delegate #'mu4e-view-save-url))
    (define-key map "a" (mu4e--view-xwidget-delegate #'mu4e-view-action))
    (define-key map "e" (mu4e--view-xwidget-delegate #'mu4e-view-save-attachments))
    ;; ... and defuse some unneeded/confusing ones
    (define-key map "H" #'ignore) ;; browse history
    (define-key map "w" #'ignore) ;; copy url
    (when-let* ((view-buf (mu4e--view-xwidget-view-buffer)))
      ;; `current-active-maps' is in precedence order, highest first; keep that
      ;; order so e.g. an active search or compose minor mode wins over
      ;; `mu4e-view-mode-map' the same way it would in the view buffer itself.
      ;; Leave out the global map: we only want to add mu4e-specific keys, not
      ;; e.g. self-insert-command or mouse bindings for every key we do not
      ;; otherwise handle.
      (dolist (source (delq (current-global-map)
                            (with-current-buffer view-buf (current-active-maps))))
        (mu4e--view-xwidget-forward-keys map source)))
    ;; only now set the parent, so the forwarded mu4e keys take precedence over
    (set-keymap-parent map webkit-map)
    (use-local-map map)))

;;; Displaying

(defun mu4e--view-xwidget-display (url view-buf)
  "Display URL in an xwidget session in the window of VIEW-BUF.
Do nothing if VIEW-BUF is not displayed."
  (when-let* ((win (get-buffer-window view-buf)))
    (let ((xwidget-webkit-disable-javascript t)
          ;; don't hi-jack user's "last session"
          (xwidget-webkit-last-session-buffer
           xwidget-webkit-last-session-buffer)
          xw-buf)
      ;; this shows the new session in the selected window, and sizes
      ;; the xwidget accordingly.
      (with-selected-window win
        (xwidget-webkit-new-session url)
        (setq xw-buf (current-buffer)))
      (with-current-buffer view-buf
        (setq mu4e--view-xwidget-buffer xw-buf))
      (with-current-buffer xw-buf
        (setq mu4e--view-xwidget-view-buffer view-buf
              ;; so `mu4e-message-at-point' and friends also work here
              mu4e--view-message (buffer-local-value 'mu4e--view-message
                                                      view-buf))
        (mu4e--view-xwidget-setup-keys)
        (when-let* ((xw (xwidget-at (point-min))))
          (set-xwidget-query-on-exit-flag xw nil))))))

(defun mu4e--view-xwidget-show ()
  "Show the current view buffer's message in an xwidget."
  (remove-hook 'mu4e-view-rendered-hook #'mu4e--view-xwidget-show t)
  (when mu4e--view-xwidget-tmp-file
    (with-demoted-errors "mu4e xwidget: %S"
      (mu4e--view-xwidget-display
       (concat "file://" mu4e--view-xwidget-tmp-file) (current-buffer)))))

;;; Main rendering function

(defun mu4e--view-render-buffer-xwidget (msg)
  "Render current buffer with MSG using an embedded xwidget.
The buffer must contain the raw message."
  (let ((html (mu4e-view-html-text
               msg (mu4e--view-html-insert-headers msg))))
    (if (not html)
        ;; Fallback to normal rendering
        (mu4e--view-render-buffer msg)
      ;; Decode and strip CRs (same as normal path)
      (article-remove-cr)
      (mm-enable-multibyte)
      (ignore-errors (run-hooks 'gnus-article-decode-hook))
      ;; Populate gnus-original-article-buffer with headers
      (save-restriction
        (message-narrow-to-headers-or-head)
        (let ((headers (buffer-string)))
          (widen)
          (with-current-buffer
              (get-buffer-create gnus-original-article-buffer 'no-hooks)
            (erase-buffer)
            (insert headers))))
      (let ((inhibit-read-only t))
        (mu4e--view-xwidget-setup-mime-handles)
        ;; populate `mu4e--view-link-map', for `mu4e-view-go-to-url' and
        ;; friends
        (mu4e--view-register-html-links html)
        (setq-local mu4e--view-xwidget-tmp-file
                    (mu4e--view-html-temp-file html))
        (add-hook 'kill-buffer-hook #'mu4e--view-xwidget-cleanup nil t)
        ;; display once the view buffer is in its window.
        (add-hook 'mu4e-view-rendered-hook #'mu4e--view-xwidget-show 90 t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

;;; Cleanup

(defun mu4e--view-xwidget-cleanup ()
  "Clean up xwidget resources when the view buffer is killed."
  ;; Note: deleting the selected window makes another buffer current, so
  ;; get what we need from the buffer-local variables first.
  (let ((xw-buf mu4e--view-xwidget-buffer)
        (tmp-file mu4e--view-xwidget-tmp-file))
    (setq mu4e--view-xwidget-buffer nil
          mu4e--view-xwidget-tmp-file nil)
    (when (buffer-live-p xw-buf)
      ;; Delete the xwidget window if visible; unless it is the
      ;; selected one, since then the caller (e.g.,
      ;; `kill-buffer-and-window') takes care of that.
      (when-let* ((win (get-buffer-window xw-buf))
                  ((not (eq win (selected-window)))))
        (ignore-errors (delete-window win)))
      (kill-buffer xw-buf))
    (when (and tmp-file (file-exists-p tmp-file))
      (delete-file tmp-file))))

;;; Toggling

(defun mu4e--view-xwidget-shown-p ()
  "Return non-nil if the current view buffer is showing an xwidget."
  (buffer-live-p mu4e--view-xwidget-buffer))

(defun mu4e--view-xwidget-view-buffer ()
  "Return the live mu4e view buffer for the current buffer, or nil.
This is either the current buffer, or, if we are in an xwidget
buffer, the view buffer it belongs to."
  (let ((buf (if (derived-mode-p 'mu4e-view-mode)
                 (current-buffer)
               mu4e--view-xwidget-view-buffer)))
    (and (buffer-live-p buf) buf)))

(defun mu4e--view-xwidget-restore-window ()
  "Show the current view buffer in the window of its xwidget buffer.
Select that window. This gives `mu4e-view-refresh' the same window
layout as without xwidget."
  (when-let* ((view-buf (current-buffer))
              ((mu4e--view-xwidget-shown-p))
              (win (get-buffer-window mu4e--view-xwidget-buffer)))
    (set-window-buffer win view-buf)
    (select-window win)))

(defun mu4e--view-xwidget-toggle ()
  "Toggle between the xwidget and the text rendering of the message.
This works from the view buffer as well as from its xwidget buffer."
  (interactive)
  (unless (mu4e-xwidget-usable-p)
    (mu4e-warn "Cannot use xwidget; see M-x mu4e-xwidget-usable-p"))
  (with-current-buffer (or (mu4e--view-xwidget-view-buffer)
                           (mu4e-warn "No message view"))
    (let ((mu4e--view-renderer-override
           (if (mu4e--view-xwidget-shown-p) 'text 'xwidget)))
      (mu4e--view-xwidget-restore-window)
      (mu4e-view-refresh))))

(defun mu4e--view-xwidget-quit ()
  "Quit the message view, from its xwidget buffer.
This closes the xwidget buffer as well as the underlying view
buffer."
  (interactive)
  (with-current-buffer (or (mu4e--view-xwidget-view-buffer)
                           (mu4e-warn "No message view"))
    (mu4e--view-xwidget-restore-window)
    (mu4e-view-quit)))

(provide 'mu4e-view-xwidget)
;;; mu4e-view-xwidget.el ends here
