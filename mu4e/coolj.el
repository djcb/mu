;;; coolj.el --- automatically wrap long lines  -*- coding:utf-8 -*-

;; Copyright (C) 2000, 2001, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Authors:    Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;;             Alex Schroeder <alex@gnu.org>
;;             Chong Yidong <cyd@stupidchicken.com>
;; Maintainer: David Edmondson <dme@dme.org>
;; Keywords: convenience, wp

;; This file is not part of GNU Emacs.

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

;;; This is a simple derivative of some functionality from
;;; `longlines.el'. The key difference is that this version will
;;; insert a prefix at the head of each wrapped line. The prefix is
;;; calculated from the originating long line.

;;; No minor-mode is provided, the caller is expected to call
;;; `coolj-wrap-region' to wrap the region of interest.

;;; Code:

(defgroup coolj nil
  "Wrapping of long lines with prefix."
  :group 'fill)

(defcustom coolj-wrap-follows-window-size t
  "Non-nil means wrap text to the window size.
Otherwise respect `fill-column'."
  :group 'coolj
  :type 'boolean)

(defcustom coolj-line-prefix-regexp "^\\(>+ \\)*"
  "Regular expression that matches line prefixes."
  :group 'coolj
  :type 'regexp)

(defvar coolj-wrap-point nil)

(make-variable-buffer-local 'coolj-wrap-point)

(defun coolj-determine-prefix ()
  "Determine the prefix for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward coolj-line-prefix-regexp nil t)
	(buffer-substring (match-beginning 0) (match-end 0))
      "")))

(defun coolj-wrap-buffer ()
  "Wrap the current buffer."
  (coolj-wrap-region (point-min) (point-max)))

(defun coolj-wrap-region (beg end)
  "Wrap each successive line, starting with the line before BEG.
Stop when we reach lines after END that don't need wrapping, or the
end of the buffer."
  (setq fill-column (if coolj-wrap-follows-window-size
			(window-width)
		      fill-column))
  (let ((mod (buffer-modified-p)))
    (setq coolj-wrap-point (point))
    (goto-char beg)
    (forward-line -1)
    ;; Two successful coolj-wrap-line's in a row mean successive
    ;; lines don't need wrapping.
    (while (null (and (coolj-wrap-line)
		      (or (eobp)
			  (and (>= (point) end)
			       (coolj-wrap-line))))))
    (goto-char coolj-wrap-point)
    (set-buffer-modified-p mod)))

(defun coolj-wrap-line ()
  "If the current line needs to be wrapped, wrap it and return nil.
If wrapping is performed, point remains on the line.  If the line does
not need to be wrapped, move point to the next line and return t."
  (let ((prefix (coolj-determine-prefix)))
    (if (coolj-set-breakpoint prefix)
	(progn
	  (insert-before-markers ?\n)
	  (backward-char 1)
	  (delete-char -1)
	  (forward-char 1)
	  (insert-before-markers prefix)
	  nil)
      (forward-line 1)
      t)))

(defun coolj-set-breakpoint (prefix)
  "Place point where we should break the current line, and return t.
If the line should not be broken, return nil; point remains on the
line."
  (move-to-column fill-column)
  (if (and (re-search-forward "[^ ]" (line-end-position) 1)
           (> (current-column) fill-column))
      ;; This line is too long.  Can we break it?
      (or (coolj-find-break-backward prefix)
          (progn (move-to-column fill-column)
                 (coolj-find-break-forward)))))

(defun coolj-find-break-backward (prefix)
  "Move point backward to the first available breakpoint and return t.
If no breakpoint is found, return nil."
  (let ((end-of-prefix (+ (line-beginning-position) (length prefix))))
    (and (search-backward " " end-of-prefix 1)
	 (save-excursion
	   (skip-chars-backward " " end-of-prefix)
	   (null (bolp)))
	 (progn (forward-char 1)
		(if (and fill-nobreak-predicate
			 (run-hook-with-args-until-success
			  'fill-nobreak-predicate))
		    (progn (skip-chars-backward " " end-of-prefix)
			   (coolj-find-break-backward prefix))
		  t)))))

(defun coolj-find-break-forward ()
  "Move point forward to the first available breakpoint and return t.
If no break point is found, return nil."
  (and (search-forward " " (line-end-position) 1)
       (progn (skip-chars-forward " " (line-end-position))
              (null (eolp)))
       (if (and fill-nobreak-predicate
                (run-hook-with-args-until-success
                 'fill-nobreak-predicate))
           (coolj-find-break-forward)
         t)))

(provide 'coolj)
