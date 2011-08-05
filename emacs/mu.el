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
(require 'mu-view)
(require 'mu-headers)
(require 'mu-message)

(define-key mu-headers-mode-map "q" 'mu-quit-buffer)
(define-key mu-headers-mode-map "f" 'mu-headers)
(define-key mu-headers-mode-map (kbd "<up>") 'mu-headers-prev)
(define-key mu-headers-mode-map (kbd "<down>") 'mu-headers-next)
(define-key mu-headers-mode-map (kbd "RET") 'mu-headers-view)
(define-key mu-headers-mode-map "n" 'mu-headers-next)
(define-key mu-headers-mode-map "p" 'mu-headers-prev)
(define-key mu-headers-mode-map "o" 'mu-headers-change-sort)
(define-key mu-headers-mode-map "g" 'mu-headers-refresh)
(define-key mu-headers-mode-map "m" 'mu-headers-mark-for-move)
(define-key mu-headers-mode-map "d" 'mu-headers-mark-for-trash)
(define-key mu-headers-mode-map "D" 'mu-headers-mark-for-deletion)
(define-key mu-headers-mode-map "u" 'mu-headers-unmark)
(define-key mu-headers-mode-map "U" 'mu-headers-unmark-all)
(define-key mu-headers-mode-map "r" 'mu-headers-reply)
(define-key mu-headers-mode-map "f" 'mu-headers-forward)
(define-key mu-headers-mode-map "x" 'mu-headers-marks-execute)


(define-key mu-view-mode-map "q" 'mu-quit-buffer)
(define-key mu-view-mode-map "f" 'mu-view-find)
(define-key mu-view-mode-map "n" 'mu-view-next)
(define-key mu-view-mode-map "p" 'mu-view-prev)
(define-key mu-view-mode-map "r" 'mu-reply)
(define-key mu-view-mode-map "f" 'mu-forward)
(define-key mu-view-mode-map "x" 'mu-execute)
(define-key mu-view-mode-map "m" 'mu-view-mark-for-move)
(define-key mu-view-mode-map "d" 'mu-view-mark-for-trash)
(define-key mu-view-mode-map "D" 'mu-view-mark-for-deletion)
(define-key mu-view-mode-map "u" 'mu-view-unmark)
(define-key mu-view-mode-map "U" 'mu-view-unmark-all)
(define-key mu-view-mode-map "r" 'mu-view-reply)
(define-key mu-view-mode-map "f" 'mu-view-forward)
(define-key mu-view-mode-map "x" 'mu-view-marked-execute)


(provide 'mu)
  
