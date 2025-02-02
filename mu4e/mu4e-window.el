;;; mu4e-window.el --- Window management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mickey Petersen
;; Copyright (C) 2023-2025 Dirk-Jan C. Binnema

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Buffer names for internal use

(defconst mu4e--sexp-buffer-name "*mu4e-sexp-at-point*"
  "Name for the buffer which shows the sexp for the message-at-point.")

(defconst mu4e--last-query-buffer-name "*mu4e-last-query*"
  "Name for the buffer which shows the last server-query.")

(defvar mu4e-main-buffer-name "*mu4e-main*"
  "Name of the mu4e main buffer.")

(defvar mu4e-embedded-buffer-name " *mu4e-embedded*"
  "Name for the embedded message view buffer.")

;; Buffer names for public use

(defvar mu4e-headers-buffer-name "*mu4e-headers*"
  "Name of the buffer for message headers.")

(defvar mu4e-view-buffer-name "*mu4e-article*"
  "Name of the view buffer.")

(defvar mu4e-headers-buffer-name-func nil
  "Function used to name the headers buffers.")

(defvar mu4e-view-buffer-name-func nil
  "Function used to name the view buffers.

The function is given one argument, the headers buffer it is
linked to.")

(defvar-local mu4e-linked-headers-buffer nil
  "Holds the headers buffer object that ties it to a view.")

(defcustom mu4e-split-view 'horizontal
  "How to show messages / headers.
A symbol which is either:
 * `horizontal':    split horizontally (headers on top)
 * `vertical':      split vertically (headers on the left).
 * `single-window': view and headers in one window (mu4e will try not to
        touch your window layout), main view in minibuffer
 * anything else:   don't split (show either headers or messages,
        not both).

Also see `mu4e-headers-visible-lines' and
`mu4e-headers-visible-columns'.

Note that in older mu4e version, the value could also be
function; this is no longer supported; instead you can use
`display-buffer-alist'."
  :type '(choice (const :tag "Split horizontally" horizontal)
                 (const :tag "Split vertically" vertical)
                 (const :tag "Single window" single-window)
                 (const :tag "Don't split" nil))
  :group 'mu4e-headers)

(defcustom mu4e-headers-visible-lines 10
  "Number of lines to display in the headers view (horizontal split-view).
horizontal split-view. This includes the header-line at the top,
and the mode-line."
  :type 'integer
  :group 'mu4e-headers)

(defcustom mu4e-headers-visible-columns 30
  "Number of columns to display for the header view (vertical split-view).
vertical split-view."
  :type 'integer
  :group 'mu4e-headers)

(defcustom mu4e-compose-switch nil
  "Where to display the new message?
A symbol:
- nil           : default (new buffer)
- window        : compose in new window
- frame or t    : compose in new frame
- display-buffer: use `display-buffer' / `display-buffer-alist'
  (for fine-tuning).

For backward compatibility with `mu4e-compose-in-new-frame', t is
treated as =\\'frame."
  :type 'symbol
  :group 'mu4e-compose)

(declare-function mu4e-view-mode "mu4e-view")
(declare-function mu4e-error     "mu4e-helpers")
(declare-function mu4e-warn      "mu4e-helpers")
(declare-function mu4e-message   "mu4e-helpers")

(defun mu4e-get-headers-buffer (&optional buffer-name create)
  "Return a related headers buffer optionally named BUFFER-NAME.

If CREATE is non-nil, the headers buffer is created if the
generated name does not already exist."
  (let* ((buffer-name
          (or
           ;; buffer name generator func. If a user wants
           ;; to supply its own naming scheme, we use that
           ;; in lieu of our own heuristic.
           (and mu4e-headers-buffer-name-func
                (funcall mu4e-headers-buffer-name-func))
           ;; if we're supplied a buffer name for a
           ;; headers buffer then try to use that one.
           buffer-name
           ;; if we're asking for a headers buffer from a
           ;; view, then we get our linked buffer. If
           ;; there is no such linked buffer -- it is
           ;; detached -- raise an error.
           (and (mu4e-current-buffer-type-p 'view)
                mu4e-linked-headers-buffer)
           ;; if we're already in a headers buffer then
           ;; that is the one we use.
           (and (mu4e-current-buffer-type-p 'headers)
                (current-buffer))
           ;; default name to use if all other checks fail.
           mu4e-headers-buffer-name))
         (buffer (get-buffer buffer-name)))
    (when (and (not (buffer-live-p buffer)) create)
      (setq buffer (get-buffer-create buffer-name)))
    ;; This may conceivably return a non-existent buffer if `create'
    ;; and `buffer-live-p' are nil.
    ;;
    ;; This is seemingly "OK" as various parts of the code check for
    ;; buffer liveness themselves.
    buffer))

(defun mu4e-get-view-buffers (pred)
  "Filter all known view buffers and keep those where PRED return non-nil.

The PRED function is called from inside the buffer that is being
tested."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (and (mu4e-current-buffer-type-p 'view)
            (and pred (funcall pred buf)))))
   (buffer-list)))

(defun mu4e--view-detached-p (buffer)
  "Return non-nil if BUFFER is a detached view buffer."
  (with-current-buffer buffer
    (unless (mu4e-current-buffer-type-p 'view)
      (mu4e-error "Buffer `%s' is not a valid mu4e view buffer" buffer))
    (null mu4e-linked-headers-buffer)))

(defun mu4e--get-current-buffer-type ()
  "Return an internal symbol that corresponds to each mu4e major mode."
  (cond ((or (derived-mode-p 'mu4e-view-mode)
             (derived-mode-p 'mu4e-raw-view-mode)) 'view)
        ((derived-mode-p 'mu4e-headers-mode) 'headers)
        ((derived-mode-p 'mu4e-compose-mode) 'compose)
        ((derived-mode-p 'mu4e-main-mode) 'main)
        (t 'unknown)))

(defun mu4e-current-buffer-type-p (type)
  "Return non-nil if the current buffer is a mu4e buffer of TYPE.

Where TYPE is `view', `headers', `compose', `main' or `unknown'.

Checks are performed using `derived-mode-p' and the current
buffer's major mode."
  (eq (mu4e--get-current-buffer-type) type))


;; backward-compat; buffer-local-boundp was introduced in emacs 28.
(defun mu4e--buffer-local-boundp (symbol buffer)
  "Return non-nil if SYMBOL is bound in BUFFER.
Also see `local-variable-p'."
  (condition-case nil
      (buffer-local-value symbol buffer)
    (:success t)
    (void-variable nil)))


(defun mu4e-get-view-buffer (&optional headers-buffer create)
  "Return a view buffer belonging optionally to HEADERS-BUFFER.

If HEADERS-BUFFER is nil, the most likely (and available) headers
buffer is used.

Detached view buffers are ignored; that may result in a new view buffer
being created if CREATE is non-nil."
  ;; If `headers-buffer' is nil, then the caller does not have a
  ;; headers buffer preference.
  ;;
  ;; In that case, we request the most plausible headers buffer from
  ;; `mu4e-get-headers-buffer'.
  (when (setq headers-buffer (or headers-buffer (mu4e-get-headers-buffer)))
    (let ((buffer)
          ;; If `mu4e-view-buffer-name-func' is non-nil, then use that
          ;; to source the name of the view buffer to create or re-use.
          (buffer-name
           (or (and mu4e-view-buffer-name-func
                    (funcall mu4e-view-buffer-name-func headers-buffer))
               ;; If the variable is nil, use the default
               ;; name
               mu4e-view-buffer-name))
          ;; Search all view buffers and return those that are linked to
          ;; `headers-buffer'.
          (linked-buffer
           (mu4e-get-view-buffers
            (lambda (buf)
              (and (mu4e--buffer-local-boundp 'mu4e-linked-headers-buffer buf)
                   (eq mu4e-linked-headers-buffer headers-buffer))))))
      ;; If such a linked buffer exists and its buffer is live, we use that
      ;; buffer.
      (if (and linked-buffer (buffer-live-p (car linked-buffer)))
          ;; NOTE: It's possible for there to be more than one linked view
          ;; buffer.
          ;;
          ;; What, if anything, should the heuristic be to pick the
          ;; one to use? Presently `car' is used, but there are better
          ;; ways, no doubt. Perhaps preferring those with live windows?
          (setq buffer (car linked-buffer))
        (setq buffer (get-buffer buffer-name))
        ;; check if `buffer' is already live *and* detached. If it is,
        ;; we'll generate a new, unique name.
        (when (and (buffer-live-p buffer) (mu4e--view-detached-p buffer))
          (setq buffer (generate-new-buffer-name buffer-name)))
        (when (and (not (buffer-live-p buffer)) create)
          (setq buffer (get-buffer-create (or buffer buffer-name)))
          (with-current-buffer buffer
            (mu4e-view-mode))))
      (when (and buffer (buffer-live-p buffer))
        ;; Required. Callers expect the view buffer to be set.
        (set-buffer buffer)
        ;; Required. The call chain of `mu4e-view-mode' ends up
        ;; calling `kill-all-local-variables', which destroys the
        ;; local binding.
        (set (make-local-variable 'mu4e-linked-headers-buffer) headers-buffer))
      buffer)))

;; backward compat: `display-buffer-full-frame' only appears in emacs 29.
(unless (fboundp 'display-buffer-full-frame)
  (defun display-buffer-full-frame (buffer alist)
    "Display BUFFER in the current frame, taking the entire frame.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
    (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                           (display-buffer-same-window buffer alist)
                           (display-buffer-pop-up-window buffer alist)
                           (display-buffer-use-some-window buffer alist))))
      (delete-other-windows window)
      window)))


(defun mu4e-display-buffer (buffer-or-name &optional select)
  "Display BUFFER-OR-NAME as per `mu4e-split-view'.

If SELECT is non-nil, the final window (and thus BUFFER-OR-NAME)
is selected.

This function internally uses `display-buffer' (or
`pop-to-buffer' if SELECT is non-nil).

It is therefore possible to change the display behavior by
modifying `display-buffer-alist'.

If `mu4e-split-view' is a function, then it must return a live window
for BUFFER-OR-NAME to be displayed in."
  ;; For now, using a function for mu4e-split-view is not behaving well
  ;; Turn off.
  (when (functionp mu4e-split-view)
    (mu4e-message "Function for `mu4e-split-view' not supported; fallback")
    (setq mu4e-split-view 'horizontal))

  (let* ((buffer-name (or (get-buffer buffer-or-name)
                          (mu4e-error "Buffer `%s' does not exist"
                                      buffer-or-name)))
         (buffer-type
          (with-current-buffer buffer-name (mu4e--get-current-buffer-type)))
         (direction (cons 'direction
                          (pcase (cons buffer-type mu4e-split-view)
                            ;; views or headers can display
                            ;; horz/vert depending on the value of
                            ;; `mu4e-split-view'
                            (`(,(or 'view 'headers) . horizontal) 'below)
                            (`(,(or 'view 'headers) . vertical) 'right)
                            (`(,_ . t) nil))))
         (window-size
          (pcase (cons buffer-type mu4e-split-view)
            ;; views or headers can display
            ;; horz/vert depending on the value of
            ;; `mu4e-split-view'
            ('(view . horizontal)
             '((window-height . shrink-window-if-larger-than-buffer)))
            ('(view . vertical)
             '((window-min-width . fit-window-to-buffer)))
            (`(,_ . t) nil)))
         (window-action (cond
                         ;; main-buffer
                         ((eq buffer-type 'main)
                          '(display-buffer-reuse-window
                            display-buffer-reuse-mode-window
                            display-buffer-full-frame))
                         ;; compose-buffer
                         ((eq buffer-type 'compose)
                          (pcase mu4e-compose-switch
                            ('window          #'display-buffer-pop-up-window)
                            ((or 'frame 't)   #'display-buffer-pop-up-frame)
                            (_                '(display-buffer-reuse-window
                                                display-buffer-reuse-mode-window
                                                display-buffer-same-window))))
                         ;; headers buffer
                         ((memq buffer-type '(headers))
                          '(display-buffer-reuse-window
                            display-buffer-reuse-mode-window
                            display-buffer-same-window))

                         ((memq mu4e-split-view '(horizontal vertical))
                          '(display-buffer-in-direction))

                         ((memq mu4e-split-view '(single-window))
                          '(display-buffer-reuse-window
                            display-buffer-reuse-mode-window
                            display-buffer-same-window))
                         ;; I cannot discern a difference between
                         ;; `single-window' and "anything else" in
                         ;; `mu4e-split-view'.
                         (t '(display-buffer-reuse-window
                              display-buffer-reuse-mode-window
                              display-buffer-same-window))))
         (arg `((,@window-action)
                ,@window-size
                ,direction)))
    (funcall (if select #'pop-to-buffer #'display-buffer)
             buffer-name
             arg)))

(defun mu4e-resize-linked-headers-window ()
  "Resizes the linked headers window belonging to a view.

Resizes the current headers view according to `mu4e-split-view'
and `mu4e-headers-visible-lines' or
`mu4e-headers-visible-columns'.

This function is best called from the hook
`mu4e-view-rendered-hook'."
  (unless (mu4e-current-buffer-type-p 'view)
    (mu4e-error "Cannot resize as this is not a valid view buffer"))
  (when-let* ((win (and mu4e-linked-headers-buffer
                        (get-buffer-window mu4e-linked-headers-buffer))))
    ;; This can fail for any number of reasons. If it does, we do
    ;; nothing. If the user has customized the window display we may
    ;; find it impossible to resize the window, and that should not be
    ;; cause for error.
    (ignore-errors
      (cond ((eq mu4e-split-view 'vertical)
             (window-resize win (- mu4e-headers-visible-columns
                                   (window-width win nil))
                            t t nil))
            ((eq mu4e-split-view 'horizontal)
             (set-window-text-height win mu4e-headers-visible-lines))))))

(provide 'mu4e-window)
;;; mu4e-window.el ends here
