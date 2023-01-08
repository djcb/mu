;;; mu4e-modeline.el -- part of mu4e  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dirk-Jan C. Binnema

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
   func))

(defvar mu4e--modeline-item nil
  "Mu4e item for the global-mode-line.")

(defvar mu4e--modeline-string-cached nil
  "Cached version of the modeline string.")

(defun mu4e--modeline-string ()
  "Get the current mu4e modeline string."
  (or mu4e--modeline-string-cached
      (setq mu4e--modeline-string-cached
            (mapconcat
             (lambda (func) (or (funcall func) ""))
             (append mu4e--modeline-buffer-items
                     mu4e--modeline-global-items)
             " "))))

(defun mu4e--modeline-update ()
  "Recalculate and force-update the modeline."
  (setq mu4e--modeline-string-cached nil)
  (force-mode-line-update))

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

(provide 'mu4e-modeline)

;;; mu4e-modeline.el ends here
