;;; mu-find.el -- use `mu' from emacs
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

(require 'mu-common)

(defvar mu-find-fields
  '( (:date          .  25)
     (:from-or-to    .  22)
     (:subject       .  40))
  "a list of fields and their widths")

(defvar mu-find-sort-field "date"
  "shortcut of the field to sort on (see mu-find (1))")
(defvar mu-find-sort-descending nil
  "whether to sort in descending order")

;; internal stuff
(defconst mu-find-buffer-name " *mu-find*" "name of the mu
results buffer; name should start with a space")
(defvar mu-find-process nil "the possibly running find process")
(defconst mu-find-process-name "*<mu-find-process>*" "name of the mu
results buffer; name should start with a space")
(defconst mu-eom "\n;;eom\n" "marker for the end of message in
the mu find output")
(defvar mu-find-expression nil
  "search expression for the current find buffer")

(defvar mu-buf "" "buffer for results data")
(defun mu-find-process-filter (proc str)
  "process-filter for the 'mu find --format=sexp output; it
  accumulates the strings into valid sexps by checking of the
  ';;eom' end-of-msg marker, and then evaluating them"
  (with-current-buffer mu-find-buffer-name
    (save-excursion
      (setq mu-buf (concat mu-buf str))
      (let ((eom (string-match mu-eom mu-buf)))
	(while (numberp eom)
	  (let* ((msg (car (read-from-string (substring mu-buf 0 eom))))
		  (inhibit-read-only t))
	    (goto-char (point-max))
	    (save-match-data (insert (mu-find-header msg) ?\n)))
	  (setq mu-buf (substring mu-buf (match-end 0)))
	  (setq eom (string-match mu-eom mu-buf)))))))


(defun mu-find-process-sentinel (proc msg)
  "Check the mu-find process upon completion"
  (let ((status (process-status proc))
	 (exit-status (process-exit-status proc)))
    (if (memq status '(exit signal))
      (let ((inhibit-read-only t)
	     (text
	       (cond
		 ((eq status 'signal)
		   "Search process killed (results incomplete)")
		 ((eq status 'exit)
		   (cond
		     ((= 0 exit-status) "End of search results")
		     ((= 2 exit-status) "No matches found")
		     ((= 4 exit-status) "Database problem; try running 'mu index'")
		     (t (format "Some error occured; mu-find returned %d"
			  exit-status))))
		 (t "Unknown status")))) ;; shouldn't happen
	(when (get-buffer mu-find-buffer-name)
		(with-current-buffer mu-find-buffer-name
		  (save-excursion
		    (goto-char (point-max))
		    (insert (mu-str text)))))))))


;; Note, the 'mu find --format=sexp' sexp is almost the same as the ones that
;; 'mu view --format=sexp' produces (see mu-get-message), with the difference
;; that former may give more than one result, and that mu-find output comes from
;; the database rather than file, and does _not_ contain the message body
(defun mu-find (expr)
  "search in the mu database"
  (interactive "s[mu] match expr: ")
  (let* ((output (get-buffer mu-find-buffer-name)))
    (when output (kill-buffer output))
    (setq output (get-buffer-create mu-find-buffer-name) mu-buf "")
    (let* ((dummy-arg "--fields=\"dummy\"") ;; ignored
	    (proc
	      (start-process mu-find-process-name mu-find-process-name
		mu-binary
		"find"
		(if mu-home
		  (concat "--muhome=" mu-home) dummy-arg)
		(if mu-find-sort-field
		  (concat "--sortfield=" mu-find-sort-field) dummy-arg)
		(if mu-find-sort-descending "--descending" dummy-arg)
		"--format=sexp"
		"--quiet"
	      expr)))
      (set-process-filter   proc 'mu-find-process-filter)
      (set-process-sentinel proc 'mu-find-process-sentinel)
      (setq mu-find-process proc)
      (switch-to-buffer output)
      (setq mu-find-expression expr)
      ;; (make-variable-buffer-local mu-find-expression)
      (mu-find-mode))))

(defun mu-find-display-contact (lst width face)
  "display a list of contacts, truncated for fitting in WIDTH"
  (if lst
    (let* ((len (length lst))
	    (str (if (= len 0) "<none>"
		   ;; try name -> email -> ?
		   (or (car(car lst)) (cdr(car lst)) "?")))		   
	    (others (if (> len 1) (mu-str (format " [+%d]" (- len 1))) "")))
      (truncate-string-to-width 
	(concat(propertize (truncate-string-to-width str
			     (- width (length others)) 0 ?\s "...") 'face face) others)
	width 0 ?\s))
    (make-string width ?\s)))


(defun mu-find-display-from-or-to (fromlst tolst width from-face to-face)
  "return a propertized string for FROM unless TO matches
  mu-own-address, in which case it returns TO, prefixed with To:"
  (if (and fromlst tolst)
    (let ((fromaddr (cdr(car fromlst))))
      (if (and fromaddr (string-match mu-own-address fromaddr))
	(concat (mu-str "To ") (mu-find-display-contact tolst (- width 3) to-face))
	(mu-find-display-contact fromlst width from-face)))
    (make-string width ?\s)))

(defun mu-find-display-size (size width face)
  "return a string for SIZE of WIDTH with FACE"
  (let* ((str
	   (cond
	     ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
	     ((and (>= size 1000) (< size 1000000)) (format "%2.1fK" (/ size 1000.0)))
	     ((< size 1000) (format "%d" size)))))
    (propertize  (truncate-string-to-width str width 0 ?\s) 'face face)))
	  

(defun mu-find-display-str (str width face)
  "print a STR, at WIDTH (truncate or ' '-pad) with FACE"
  (let ((str (if str str "")))
    (propertize (truncate-string-to-width str width 0 ?\s t) 'face face)))

(defun mu-find-display-flags (flags width face)
  (let ((str
	  (mapconcat
	    (lambda(flag)
	      (let ((flagname (symbol-name flag)))
		(cond 
		  ((string= flagname "unread")    "U")
		  ((string= flagname "seen")      "S")
		  ((string= flagname "replied")   "R")
		  ((string= flagname "attach")    "a")
		  ((string= flagname "encrypted") "x")
		  ((string= flagname "signed")    "s")))) flags "")))
    (propertize  (truncate-string-to-width str width 0 ?\s) 'face face)))

(defun mu-find-header (msg)
  "convert a message s-expression into a header for display"
  (let ((hdr (concat "  " (mapconcat
	 (lambda (fieldinfo)
	   (let ((field (car fieldinfo)) (width (cdr fieldinfo)))
	     (case field  
	       (:date
		 (mu-find-display-str (format-time-string mu-date-format-short
					(plist-get msg :date)) width 'mu-date-face))	  
	       (:from
		 (mu-find-display-contact (plist-get msg :from) width 'mu-from-face))
	       (:to
		 (mu-find-display-contact (plist-get msg :to) width 'mu-to-face))
	       (:cc
		 (mu-find-display-contact (plist-get msg :cc) width 'mu-cc-face))	    
	       (:bcc
		 (mu-find-display-contact (plist-get msg :bcc) width 'mu-bcc-face))
	       (:flags
		 (mu-find-display-flags (plist-get msg :flags) width 'mu-flag-face))
	       (:size
		 (mu-find-display-size (plist-get msg :size) width 'mu-size-face))
	       (:from-or-to
		 (mu-find-display-from-or-to (plist-get msg :from)
		   (plist-get msg :to) width 'mu-from-face 'mu-to-face))
	       (:subject
		 (mu-find-display-str (plist-get msg :subject) width
		   'mu-subject-face)))))
	 mu-find-fields " "))))
    (setq hdr (mu-find-set-props-for-flags hdr (plist-get msg :flags)))
    (propertize hdr 'path (plist-get msg :path) 'front-sticky t)))

(defun mu-find-set-props-for-flags (hdr flags)
  "set text properties/faces based on flags"
  (if (memq 'unread flags)
    (add-text-properties 0 (- (length hdr) 1) '(face (:weight bold)) hdr))
    hdr)


(defun mu-find-mode ()
  "major mode for displaying search results"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu-find-mode-map)
  (setq
    major-mode 'mu-find-mode mode-name "*headers*"
    truncate-lines t buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defvar mu-find-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mu-quit-buffer)
    (define-key map "s" 'mu-find-change-sort)
    (define-key map "g" 'mu-find-refresh)

    ;; marking/unmarking
    (define-key map "m" 'mu-find-mark-for-move)
    (define-key map "d" 'mu-find-mark-for-trash)
    (define-key map "D" 'mu-find-mark-for-deletion)
    (define-key map "u" 'mu-find-unmark)

    ;; message composition
    (define-key map "r" 'mu-reply)
    (define-key map "f" 'mu-forward)
    (define-key map (kbd "RET") 'mu-find-view)
    map)
  "Keymap for \"mu-find\" buffers.")
(fset 'mu-find-mode-map mu-find-mode-map)

(defun mu-find-view ()
  "display the message at the current line"
  (interactive)
  (let ((path (mu-get-path)))
    (when path (mu-view path))))

(defun mu-find-next  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (or (/= 0 (forward-line 1)) (not (mu-get-path)))    
    (progn (message "No message after this one") nil)
    t))

(defun mu-find-prev  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (/= 0 (forward-line -1))
    (progn (message "No message before this one") nil)
    t))

(defun mu-find-refresh ()
  "re-run the query for the current search expression"
  (interactive) 
  (unless (and mu-find-process
	    (eq (process-status mu-find-process) 'run))
    (when mu-find-expression
      (mu-find mu-find-expression))))

(defun mu-find-change-sort-order (fieldchar)
  "change the sortfield to FIELDCHAR"
  (interactive"cField to sort by ('d', 's', etc.; see mu-find(1)):\n")
  (let
    ((field
       (case fieldchar
	 (?b "bcc")
	 (?c "cc")
	 (?d "date")
	 (?f "from")
	 (?i "msgid")
	 (?m "maildir")
	 (?p "prio")
	 (?s "subject")
	 (?t "to")
	 (?z "size"))))
    (if field
      (setq mu-find-sort-field field)
      (message "Invalid sort-field; use one of bcdfimpstz (see mu-find(1)"))
    field))

(defun mu-find-change-sort-direction (dirchar)
  "change the sort direction, either [a]scending or [d]escending"
  (interactive
    "cSorting direction ([a]scending or [d]escending):")
  (cond
    (?d (setq mu-find-sort-descending t) t)
    (?a (setq mu-find-sort-descending nil) t)
    (t 	(message
	  "Invalid sort-direction; choose either [a]scending or [d]escending") nil)))

(defun mu-find-mark (what)
 "mark the current msg for 'trash, 'move, 'none"
  (when (mu-get-path)
    (move-beginning-of-line 1)
    (let ((inhibit-read-only t) (overwrite-mode nil))
      (if (and (not (eq what 'none)) (get-text-property (point) 'action))
	(message "Message at pooint is already marked")
	(progn
	  (delete-char 1)
	  (case what
	    ('trash (insert-and-inherit
		      (mu-str (propertize "d" 'action what 'target "/foo/bar"))))
	    ('delete (insert-and-inherit
		      (mu-str (propertize "D" 'action what 'target "/foo/bar"))))
	    ('move  (insert-and-inherit
		      (mu-str (propertize "m" 'action what 'target "/foo/bar"))))
	    ('none  (insert-and-inherit " ")))
	  (forward-line))))))
  
(defun mu-find-mark-for-trash ()
  (interactive)
  (mu-find-mark 'trash))

(defun mu-find-mark-for-deletion ()
  (interactive)
  (mu-find-mark 'delete))

(defun mu-find-mark-for-move ()
  (interactive)
  (mu-find-mark 'move))

(defun mu-find-unmark ()
  (interactive)
  (mu-find-mark 'none))

(defun mu-find-change-sort ()
  "change sort field and direction"
  (interactive)
  (and (call-interactively 'mu-find-change-sort-order)
    (call-interactively 'mu-find-change-sort-direction)))

(provide 'mu-find)
  
