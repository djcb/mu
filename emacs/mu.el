;;; mu.el -- use `mu' from emacs
;; Copyright (C) 2011 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

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

;;; Code:
(require 'mu-find)
(require 'mu-view)
(require 'mu-message)

(define-key mu-find-mode-map "q" 'mu-quit-buffer)
(define-key mu-find-mode-map "f" 'mu-find)
(define-key mu-find-mode-map (kbd "<up>") 'mu-find-prev)
(define-key mu-find-mode-map (kbd "<down>") 'mu-find-next)
(define-key mu-find-mode-map (kbd "RET") 'mu-find-view)
(define-key mu-find-mode-map "n" 'mu-find-next)
(define-key mu-find-mode-map "p" 'mu-find-prev)
(define-key mu-find-mode-map "o" 'mu-find-change-sort)
(define-key mu-find-mode-map "g" 'mu-find-refresh)
(define-key mu-find-mode-map "m" 'mu-find-mark-for-move)
(define-key mu-find-mode-map "d" 'mu-find-mark-for-thrash)
(define-key mu-find-mode-map "D" 'mu-find-mark-for-deletion)
(define-key mu-find-mode-map "u" 'mu-find-unmark)
(define-key mu-find-mode-map "r" 'mu-reply)
(define-key mu-view-mode-map "f" 'mu-forward)

(define-key mu-view-mode-map "q" 'mu-view-quit-buffer)
(define-key mu-view-mode-map "f" 'mu-view-find)
(define-key mu-view-mode-map "n" 'mu-view-next)
(define-key mu-view-mode-map "p" 'mu-view-prev)
(define-key mu-view-mode-map "r" 'mu-reply)
(define-key mu-view-mode-map "f" 'mu-forward)
  

(provide 'mu)
  
