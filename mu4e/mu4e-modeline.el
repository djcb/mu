;;; mu4e-modeline.el --- Modeline for mu4e  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Dirk-Jan C. Binnema

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

;; This file contains functionality for putting mu4e-related information in the
;; Emacs modeline, both buffer-specific and globally.

;;; Code:

(require 'cl-lib)


(defcustom mu4e-modeline-max-width 42
  "Determines the maximum length of the local modeline string.
If the string exceeds this limit, it will be truncated to fit.

Note: this only affects the local modeline items (such as the
context, the search properties and the last query), not the
global items (such as the favorite bookmark results)."
  :type 'integer
  :group 'mu4e-modeline)

(defcustom mu4e-modeline-prefer-bookmark-name t
  "Show bookmark name rather than query in modeline.

If non-nil, if the current search query matches some bookmark,
display the bookmark name rather than the query."
  :type 'boolean
  :group 'mu4e-modeline)

(defcustom mu4e-modeline-show-global t
  "Whether to populate global modeline segments.

If non-nil, show both buffer-specific and global modeline items,
otherwise only present buffer-specific information."
  :type 'boolean
  :group 'mu4e-modeline)

(defvar-local mu4e--modeline-buffer-items nil
  "List of buffer-local items for the mu4e modeline.
Each element is function that evaluates to a string.")

(defvar mu4e--modeline-global-items nil
  "List of items for the global modeline.
Each element is function that evaluates to a string.")

(defun mu4e--modeline-register (func &optional global)
  "Register FUNC for calculating some mu4e modeline part.
If GLOBAL is non-nil, add to the global-modeline; otherwise use
the buffer-local one."
  (add-to-list
   (if global
       'mu4e--modeline-global-items
     'mu4e--modeline-buffer-items)
   func 'append))

(defun mu4e--modeline-quote-and-truncate (str)
  "Quote STR to be used literally in the modeline.
The string is truncated to fit if its length exceeds
`mu4e-modeline-max-width'."
  (replace-regexp-in-string
   "%" "%%"
   (truncate-string-to-width str mu4e-modeline-max-width 0 nil t)))

(defvar mu4e--modeline-item nil
  "Mu4e item for the global-mode-line.")

(defvar mu4e--modeline-global-string-cached nil
  "Cached version of the _global_ modeline string.
Note that we don't cache the local parts, so that the modeline
gets updated when we leave the buffer from which the local parts
originate.")

(defun mu4e--modeline-string ()
  "Get the current mu4e modeline string."
  (let* ((collect
          (lambda (lst)
            (mapconcat
             (lambda (func) (or (funcall func) "")) lst " ")))
         (global-string ;; global string is _cached_ as it may be expensive.
          (and
           mu4e-modeline-show-global
           (or mu4e--modeline-global-string-cached
               (setq mu4e--modeline-global-string-cached
                     (funcall collect mu4e--modeline-global-items))))))
    (concat
     ;; (local) buffer items are _not_ cached, so they'll get update
     ;; automatically when leaving the buffer.
     (mu4e--modeline-quote-and-truncate
      (funcall collect mu4e--modeline-buffer-items))
     (and global-string " ")
     global-string)))

(define-minor-mode mu4e-modeline-mode
  "Minor mode for showing mu4e information on the modeline."
  ;; This is a bit special 'global' mode, since it consists of both
  ;; buffer-specific parts (mu4e--modeline-buffer-items) and global items
  ;; (mu4e--modeline-global-items).
  :global t
  :group 'mu4e
  :lighter nil
  (if mu4e-modeline-mode
      (progn
        (setq mu4e--modeline-item '(:eval (mu4e--modeline-string)))
        (add-to-list 'global-mode-string mu4e--modeline-item)
        (mu4e--modeline-update))
    (progn
      (setq global-mode-string
            (seq-remove (lambda (item) (equal item mu4e--modeline-item))
                        global-mode-string)))
    (force-mode-line-update)))

(defun mu4e--modeline-update ()
  "Recalculate and force-update the modeline."
  (when mu4e-modeline-mode
    (setq mu4e--modeline-global-string-cached nil)
    (force-mode-line-update)))

(provide 'mu4e-modeline)

;;; mu4e-modeline.el ends here
