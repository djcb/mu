;;; mu4e-gnus -- Mapping between some gnus configurations and mu4e
;;
;; Copyright (C) 2012 Niv Sardi

;; Author: Niv sardi <xaiki@debian.org>
;; Maintainer: Niv sardi <xaiki@debian.org>
;; Keywords: gnus, mail
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

(eval-when-compile (require 'mu4e))

(setq mu4e-has-child-mark    	gnus-sum-thread-tree-single-leaf
      mu4e-empty-parent-mark 	gnus-sum-thread-tree-false-root
      mu4e-first-child-mark  	gnus-sum-thread-tree-single-leaf
      mu4e-tree-root-mark    	gnus-sum-thread-tree-root)

(setq mu4e-draft-flag-mark   	gnus-cached-mark
      mu4e-flagged-flag-mark 	gnus-ticked-mark
      mu4e-new-flag-mark	gnus-unseen-mark
      mu4e-pased-flag-mark	gnus-forwarded-mark
      mu4e-replied-flag-mark 	gnus-replied-mark
      mu4e-seen-flag-mark    	gnus-read-mark
      mu4e-trashed-flag-mark 	gnus-del-mark
      mu4e-unread-flag-mark  	gnus-unread-mark)

(provide 'mu4e-gnus)
