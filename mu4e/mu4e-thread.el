;;; mu4e-thread.el --- Thread folding support -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mu4e-thread.el is a library that allows to fold and unfold threads in mu4e
;; headers mode. Folding works by creating an overlay over thread children that
;; display a summary (number of hidden messages and possibly number of unread
;; messages).

;; Folding is performed just-in-time such that it is quite fast to
;; fold/unfold threads. When a thread has unread messages, the folding stops at
;; the first unread message unless `mu4e-thread-fold-unread` has been set to t.

;; Similarly, when a thread has marked messages, the folding stops at the first
;; marked message.

;; Note, you can only use these functions when threads are available, roughly
;; when `mu4e-search-threads' in non-nil.

;;; Usage example:
;;
;; After a search, mu4e-thread-mode will be enable when threads
;; are available; so, to automatically sort them:
;;  (add-hook 'mu4e-thread-mode-hook #'mu4e-thread-fold-apply-all)

;;; Code:

(require 'mu4e-vars)
(require 'mu4e-message)
(require 'mu4e-mark)

(defcustom mu4e-thread-fold-unread nil
  "Whether to fold unread messages in a thread."
  :type 'boolean
  :group 'mu4e-headers)

(defcustom mu4e-thread-fold-single-children nil
  "When non-nil, fold a thread even if there is only a single child.
Otherwise, do not not fold single children since would simply
hide the single child."
  :type 'boolean
  :group 'mu4e-headers)

(defface mu4e-thread-fold-face
  `((t :inherit mu4e-highlight-face))
  "Face for the information line of a folded thread."
  :group 'mu4e-faces)

(defvar-local mu4e-thread--fold-status nil
  "Global folding status.")

(defvar-local mu4e-thread--docids nil
  "Thread list whose folding has been set individually.")

(defvar mu4e-headers-fields) ;; defined in mu4e-headers.el
(defun mu4e-thread-fold-info (count unread)
  "Text to be displayed for a folded thread.
There are COUNT hidden and UNREAD messages overall."
  (let ((size  (+ 2 (apply #'+ (mapcar (lambda (item) (or (cdr item) 0))
                                       mu4e-headers-fields))))
        (msg (concat (format"[%d hidden messages%s]\n" count
                            (if (> unread 0)
                                (format ", %d unread" unread)
                              "")))))
    (propertize (concat "  " (make-string size ?•) " " msg))))

(defun mu4e-thread-message-folded-p ()
  "Is point in a folded area?"
  (when-let* ((overlay (mu4e-thread-is-folded))
              (beg (overlay-start overlay))
              (end (overlay-end overlay)))
    (and (>= (point) beg) (< (point) end))))

(declare-function 'mu4e~headers-thread-root-p "mu4e-headers")
(defalias  'mu4e-thread-is-root 'mu4e~headers-thread-root-p)

(defun mu4e-thread-goto-root ()
  "Go to the root of the current thread."
  (interactive)
  (goto-char (mu4e-thread-root))
  (beginning-of-line))

(defun mu4e-thread-root ()
  "Get the root of the current thread."
  (interactive)
  (let ((point))
    (save-excursion
      (while (and (not (bobp))
                  (not (mu4e-thread-is-root)))
        (forward-line -1))
      (setq point (point)))
    point))

(declare-function 'mu4e-headers-prev-thread "mu4e-headers")
(declare-function 'mu4e-headers-next-thread "mu4e-headers")

(defalias 'mu4e-thread-goto-prev 'mu4e-headers-prev-thread)
(defalias 'mu4e-thread-goto-next 'mu4e-headers-next-thread)

(defun mu4e-thread-prev ()
  "Get the root of the previous thread (if any)."
  (save-excursion
    (when (mu4e-thread-goto-prev)
      (mu4e-thread-root))))

(defun mu4e-thread-next()
  "Get the root of the next thread (if any)."
  (save-excursion
    (when (mu4e-thread-goto-next)
      (mu4e-thread-root))))

(defun mu4e-thread-is-folded ()
  "Test if thread at point is folded."
  (interactive)
  (let* ((thread-beg (mu4e-thread-root))
         (thread-end (or (mu4e-thread-next) (point-max)))
         (overlays (overlays-in thread-beg thread-end)))
    (catch 'folded
      (dolist (overlay overlays)
        (when (overlay-get overlay 'mu4e-thread-folded)
          (throw 'folded overlay))))))

(defun mu4e-thread-fold-toggle-all ()
  "Toggle all threads folding unconditionally.
Reset individual folding states."
  (interactive)
  (setq mu4e-thread--docids nil)
  (if mu4e-thread--fold-status
      (mu4e-thread-unfold-all)
    (mu4e-thread-fold-all)))

(defun mu4e-thread-fold-apply-all ()
  "Apply global folding status to all threads not set individually."
  (interactive)
  ;; Global fold status
  (if mu4e-thread--fold-status
      (mu4e-thread-fold-all)
    (mu4e-thread-unfold-all))
  ;; Individual fold status
  (save-excursion
    (goto-char (point-min))
    (catch 'end-search
      (while (not (eobp))
        (when-let* ((msg (get-text-property (point) 'msg))
                    (docid (mu4e-message-field msg :docid))
                    (state (cdr (assoc docid mu4e-thread--docids))))
          (if (eq state 'folded)
              (mu4e-thread-fold)
            (mu4e-thread-unfold)))
        (unless (mu4e-thread-next)
          (throw 'end-search t))
        (mu4e-thread-goto-next)))))

(defun mu4e-thread-fold-all ()
  "Fold all threads unconditionally."
  (interactive)
  (setq mu4e-thread--fold-status t)

  (save-excursion
    (goto-char (point-min))
    (catch 'done
      (while (not (eobp))
        (mu4e-thread-fold t)
        (unless (mu4e-thread-goto-next)
          (throw 'done t))))))

(defun mu4e-thread-unfold-all ()
  "Unfold all threads unconditionally."
  (interactive)
  (setq mu4e-thread--fold-status nil)
  (remove-overlays (point-min) (point-max) 'mu4e-thread-folded t))

(defun mu4e-thread-fold-toggle ()
  "Toggle folding for thread at point."
  (interactive)
  (if (mu4e-thread-is-folded)
      (mu4e-thread-unfold)
    (mu4e-thread-fold)))

(defun mu4e-thread-fold-toggle-goto-next ()
  "Toggle folding for thread at point and go to next thread."
  (interactive)
  (if (mu4e-thread-is-folded)
      (mu4e-thread-unfold-goto-next)
    (mu4e-thread-fold-goto-next)))

(defun mu4e-thread-unfold (&optional no-save)
  "Unfold thread at point and store state unless NO-SAVE is t."
  (interactive)
  (unless (eq (line-end-position) (point-max))
    (when-let* ((overlay (mu4e-thread-is-folded)))
      (unless no-save
        (mu4e-thread--save-state 'unfolded))
      (delete-overlay overlay))))

(defun mu4e-thread--save-state (state)
  "Save the folding STATE of thread at point."
  (save-excursion
    (mu4e-thread-goto-root)
    (when-let* ((msg (get-text-property (point) 'msg))
                (docid (mu4e-message-field msg :docid)))
      (setf (alist-get docid mu4e-thread--docids) state))))

(defun mu4e-thread-fold (&optional no-save)
  "Fold thread at point and store state unless NO-SAVE is t."
  (interactive)
  (unless (eq (line-end-position) (point-max))
    (let* ((thread-beg (mu4e-thread-root))
           (thread-end (mu4e-thread-next))
           (thread-end (if thread-end (1- thread-end) (point-max)))
           (unread-count 0)
           (fold-beg (save-excursion
                       (goto-char thread-beg)
                       (forward-line)
                       (point)))
           (fold-end (save-excursion
                       (goto-char thread-beg)
                       (forward-line)
                       (catch 'fold-end
                         (while (and (not (eobp))
                                     (get-text-property (point) 'msg)
                                     (and thread-end (< (point) thread-end)))
                           (let* ((msg (get-text-property (point) 'msg))
                                  (docid (mu4e-message-field msg :docid))
                                  (flags (mu4e-message-field msg :flags))
                                  (unread (memq 'unread flags)))
                             (when (mu4e-mark-docid-marked-p docid)
                               (throw 'fold-end (point)))
                             (when unread
                               (unless mu4e-thread-fold-unread
                                 (throw 'fold-end (point)))
                               (setq unread-count (+ 1 unread-count))))
                           (forward-line)))
                       (point))))
      (unless no-save
        (mu4e-thread--save-state 'folded))
      (let ((child-count (count-lines fold-beg fold-end))
            (unread-count (if mu4e-thread-fold-unread unread-count 0)))
        (when (> child-count (if mu4e-thread-fold-single-children 0 1))
          (let ((inhibit-read-only t)
                (overlay (make-overlay fold-beg fold-end))
                (info (mu4e-thread-fold-info child-count unread-count)))
            (add-text-properties fold-beg (+ fold-beg 1)
                                 '(face mu4e-thread-fold-face))
            (overlay-put overlay 'mu4e-thread-folded t)
            (overlay-put overlay 'display info)))))))

(defun mu4e-thread-fold-goto-next ()
  "Fold the thread at point and go to next thread."
  (interactive)
  (unless (eq (line-end-position) (point-max))
    (mu4e-thread-fold)
    (mu4e-thread-goto-next)))

(defun mu4e-thread-unfold-goto-next ()
  "Unfold the thread at point and go to next thread."
  (interactive)
  (unless (eq (line-end-position) (point-max))
    (mu4e-thread-unfold)
    (mu4e-thread-goto-next)))

(define-minor-mode mu4e-thread-mode
  "Mode for thread-support."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-left>")  #'mu4e-thread-goto-root)
    (define-key map (kbd "<tab>")     #'mu4e-thread-fold-toggle-goto-next)
    (define-key map (kbd "<C-tab>")   #'mu4e-thread-fold-toggle-goto-next)
    (define-key map (kbd "<backtab>") #'mu4e-thread-fold-toggle-all)
    map))

(provide 'mu4e-thread)
;;; mu4e-thread.el ends here
