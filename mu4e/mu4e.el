;;; mu4e.el --- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2011-2019 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

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

(require 'mu4e-vars)
(require 'mu4e-headers)  ;; headers view
(require 'mu4e-view)     ;; message view
(require 'mu4e-main)     ;; main screen
(require 'mu4e-compose)  ;; message composition / sending
(require 'mu4e-proc)     ;; communication with backend
(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-context)  ;; support for contexts

(when mu4e-speedbar-support
  (require 'mu4e-speedbar)) ;; support for speedbar
(when mu4e-org-support
  (require 'mu4e-org))      ;; support for org-mode links

;; We can't properly use compose buffers that are revived using
;; desktop-save-mode; so let's turn that off.
(with-eval-after-load 'desktop
  (eval '(add-to-list 'desktop-modes-not-to-save 'mu4e-compose-mode)))


;;;###autoload
(defun mu4e (&optional background)
  "If mu4e is not running yet, start it. Then, show the main
window, unless BACKGROUND (prefix-argument) is non-nil."
  (interactive "P")
  ;; start mu4e, then show the main view
  (mu4e~start (unless background 'mu4e~main-view)))

(defun mu4e-quit()
  "Quit the mu4e session."
  (interactive)
  (if mu4e-confirm-quit
      (when (y-or-n-p (mu4e-format "Are you sure you want to quit?"))
        (mu4e~stop))
    (mu4e~stop)))

;;; _
(provide 'mu4e)
;;; mu4e.el ends here
