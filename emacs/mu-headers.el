;;; mu-headers.el -- use `mu' from emacs
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


;;; mu-headers has functions for displaying/manipulating a list of headers (ie.,
;;; one line descriptions of an e-mail message), based on the output of 'mu
;;; find'.

(defvar mu-headers-fields
  '( (:date          .  25)
     (:from-or-to    .  22)
     (:subject       .  40))
  "a list of fields and their widths")

(defvar mu-headers-sort-field "date"
  "shortcut of the field to sort on (see mu-headers (1))")
(defvar mu-headers-sort-descending nil
  "whether to sort in descending order")

;; internal stuff
(defconst mu-headers-buffer-name " *mu-headers*" "name of the mu
results buffer; name should start with a space")

(defvar mu-headers-process nil "the possibly running find process")

(defconst mu-eom "\n;;eom\n" "marker for the end of message in
the mu find output")

(defvar mu-headers-expression nil
  "search expression for the current find buffer")

(defvar mu-buf "" "buffer for results data")
(defun mu-headers-process-filter (proc str)
  "process-filter for the 'mu find --format=sexp output; it
  accumulates the strings into valid sexps by checking of the
  ';;eom' end-of-msg marker, and then evaluating them"
  (save-excursion
    (setq mu-buf (concat mu-buf str))
    (let ((eom (string-match mu-eom mu-buf)))
      (while (numberp eom)
	(let* ((msg (car (read-from-string (substring mu-buf 0 eom))))
		(inhibit-read-only t))
	    (goto-char (point-max))
	  (save-match-data (insert (mu-headers-header msg) ?\n)))
	(setq mu-buf (substring mu-buf (match-end 0)))
	(setq eom (string-match mu-eom mu-buf))))))


(defun mu-headers-process-sentinel (proc msg)
  "Check the mu-headers process upon completion"
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
		     (t (format "Some error occured; mu-headers returned %d"
			  exit-status))))
		 (t "Unknown status")))) ;; shouldn't happen
	(save-excursion
	  (goto-char (point-max))
	  (insert (mu-str text)))))))


;; Note, the 'mu find --format=sexp' sexp is almost the same as the ones that
;; 'mu view --format=sexp' produces (see mu-get-message), with the difference
;; that former may give more than one result, and that mu-headers output comes
;; from the database rather than file, and does _not_ contain the message body
(defun mu-headers (expr)
  "search in the mu database"
  (interactive "s[mu] messages to find: ")
  (let* ((buf (mu-get-new-buffer mu-headers-buffer-name))
	  (dummy-arg "--fields=\"dummy\"") ;; ignored
	  (proc (start-process mu-headers-buffer-name buf
		  mu-binary
		  "find"
		  (if mu-home
		    (concat "--muhome=" mu-home) dummy-arg)
		  (if mu-headers-sort-field
		    (concat "--sortfield=" mu-headers-sort-field) dummy-arg)
		  (if mu-headers-sort-descending "--descending" dummy-arg)
		  "--format=sexp"
		  "--quiet"
		  expr)))
    (switch-to-buffer buf)
    (setq
      mu-buf "" ;; if the last query went wrong... 
      mu-headers-expression expr
      mu-headers-process proc)
            
    (set-process-filter   proc 'mu-headers-process-filter)
    (set-process-sentinel proc 'mu-headers-process-sentinel)    
    (mu-headers-mode)))

(defun mu-headers-field-contact (lst width face)
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


(defun mu-headers-field-from-or-to (fromlst tolst width from-face to-face)
  "return a propertized string for FROM unless TO matches
  mu-own-address, in which case it returns TO, prefixed with To:"
  (if (and fromlst tolst)
    (let ((fromaddr (cdr(car fromlst))))
      (if (and fromaddr (string-match mu-own-address fromaddr))
	(concat (mu-str "To ") (mu-headers-field-contact tolst (- width 3) to-face))
	(mu-headers-field-contact fromlst width from-face)))
    (make-string width ?\s)))

(defun mu-headers-field-size (size width face)
  "return a string for SIZE of WIDTH with FACE"
  (let* ((str
	   (cond
	     ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
	     ((and (>= size 1000) (< size 1000000)) (format "%2.1fK" (/ size 1000.0)))
	     ((< size 1000) (format "%d" size)))))
    (propertize  (truncate-string-to-width str width 0 ?\s) 'face face)))

(defun mu-headers-field-str (str width face)
  "print a STR, at WIDTH (truncate or ' '-pad) with FACE"
  (let ((str (if str str "")))
    (propertize (truncate-string-to-width str width 0 ?\s t) 'face face)))

(defun mu-headers-field-flags (flags width face)
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

(defun mu-headers-field (msg fieldinfo)
  "determine a field based on FIELDINFO in the header for MSG"
  (let* ((field (car fieldinfo))
	  (width (cdr fieldinfo))
	  (val (plist-get msg field)) ;; note: header-field maps msg-field in
	  (str (case field            ;; most cases..
		 (:date (mu-headers-field-str (format-time-string mu-date-format-short
						val) width 'mu-date-face))	  
		 (:from    (mu-headers-field-contact val width 'mu-from-face))
		 (:to      (mu-headers-field-contact val width 'mu-to-face))
		 (:cc      (mu-headers-field-contact val width 'mu-cc-face))	    
		 (:bcc     (mu-headers-field-contact val width 'mu-bcc-face))
		 (:flags   (mu-headers-field-flags val width 'mu-flag-face))
		 (:size    (mu-headers-field-size val width 'mu-size-face))
		 (:subject (mu-headers-field-str val width 'mu-subject-face))
		 (:from-or-to ;; this one is special
		   (mu-headers-field-from-or-to (plist-get msg :from)
		     (plist-get msg :to) width 'mu-from-face 'mu-to-face)))))
    str))

(defun mu-headers-header (msg)
  "convert a message s-expression into a header for display, and
set text property 'path"
  (let ((fields (mapconcat
		  (lambda (fieldinfo)
		    (mu-headers-field msg fieldinfo)) mu-headers-fields " ")))	  
    (propertize (concat "  " fields) 'front-sticky t
      'path (plist-get msg :path))))


(defun mu-headers-mode ()
  "major mode for displaying search results"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu-headers-mode-map)
  (make-variable-buffer-local 'mu-parent-buffer)
  (make-variable-buffer-local 'mu-headers-expression)
  (make-variable-buffer-local 'mu-headers-process) 
  (setq
    major-mode 'mu-headers-mode mode-name "*headers*"
    truncate-lines t buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defvar mu-headers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'mu-quit-buffer)
    (define-key map "s" 'mu-headers-change-sort)
    (define-key map "g" 'mu-headers-refresh)
    
    ;; marking/unmarking/executing
    (define-key map "m" 'mu-headers-mark-for-move)
    (define-key map "d" 'mu-headers-mark-for-trash)
    (define-key map "D" 'mu-headers-mark-for-deletion)
    (define-key map "u" 'mu-headers-unmark)
    (define-key map "x" 'mu-headers-marked-execute)
    
    ;; message composition
    (define-key map "r" 'mu-reply)
    (define-key map "f" 'mu-forward)
    (define-key map (kbd "RET") 'mu-headers-view)
    map)
  "Keymap for \"mu-headers\" buffers.")
(fset 'mu-headers-mode-map mu-headers-mode-map)

(defun mu-headers-view ()
  "display the message at the current line"
  (interactive)
  (let ((path (mu-get-path)))
    (when path (mu-view path (current-buffer)))))

(defun mu-headers-next  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (or (/= 0 (forward-line 1)) (not (mu-get-path)))    
    (progn (message "No message after this one") nil)
    t))

(defun mu-headers-prev  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (/= 0 (forward-line -1))
    (progn (message "No message before this one") nil)
    t))

(defun mu-headers-refresh ()
  "re-run the query for the current search expression, but only
if the search process is not already running"
  (interactive)
  (message  "REFRESH %s" mu-headers-expression)
  (if (and mu-headers-process (eq (process-status mu-headers-process) 'run))
    (message "Can't refresh while running")
    (when mu-headers-expression (mu-headers mu-headers-expression))))

;; create a new query based on the old one, but with a changed sort order

(defun mu-headers-change-sort-order (fieldchar)
  "change the sortfield to FIELDCHAR"
  (interactive"cField to sort by ('d', 's', etc.; see mu-headers(1)):\n")
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
      (setq mu-headers-sort-field field)
      (message "Invalid sort-field; use one of bcdfimpstz (see mu-headers(1)"))
    field))

(defun mu-headers-change-sort-direction (dirchar)
  "change the sort direction, either [a]scending or [d]escending"
  (interactive
    "cSorting direction ([a]scending or [d]escending):")
  (cond
    (?d (setq mu-headers-sort-descending t) t)
    (?a (setq mu-headers-sort-descending nil) t)
    (t 	(message
	  "Invalid sort-direction; choose either [a]scending or [d]escending") nil)))

(defun mu-headers-change-sort ()
  "change sort field and direction"
  (interactive)
  (and (call-interactively 'mu-headers-change-sort-order)
    (call-interactively 'mu-headers-change-sort-direction)))


;; message are 'marked' for moving, deletion etc. by have a special propertized
;; character at the start of the line; this propertized character holds an
;; 'action property, which tells what to do with this one (e.g.,'d'-> trash,
;; 'D'->delete, 'm'->'move'). 'u' (unmark) removes this mark, 'U' removes
;; all-marks. 'x'->mu-headers-execute removes all marks

(defun mu-headers-mark (what)
 "mark the current msg for 'trash, 'move, 'none; return t if it
worked, nil otherwise"
  (when (mu-get-path)
    (move-beginning-of-line 1)
    (let ((inhibit-read-only t) (overwrite-mode nil))
      (if (and (not (eq what 'none)) (get-text-property (point) 'action))
	(progn (message "Message at point is already marked") nil)
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
	  t)))))


(defun mu-headers-get-marked ()
  "get all marked messages in the current buffer as a list; each
element is a cell; with 'action', 'source' , 'target'). ie one of
three:
  ('delete <path>)
  ('trash  <path> <target>)
  ('move   <path> <target>)"  
  (let ((lst))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^." nil t)
	(let* ((char0  (match-string 0))
		(action (get-text-property 0 'action char0))
		(path   (get-text-property 0 'path   char0))
		(target (get-text-property 0 'target char0)))
	  (cond
	    ((eq action 'trash)
	      (setq lst (cons (list 'trash path target) lst)))
	    ((eq action 'delete)
	      (setq lst (cons (list 'delete path) lst)))
	    ((eq action 'move)
	      (setq lst (cons (list 'move path target) lst)))))))
    lst))

(defun mu-headers-marked-execute ()
  "execute marked actions on messages in the current buffer"
  (interactive)
  (let* ((markedcount (mu-headers-count-marked))
	  (movenum (nth 0 markedcount)) (trashnum (nth 1 markedcount))
	  (deletenum (nth 2 markedcount)))
    (if (= 0 (apply '+ markedcount))
      (message "No messages are marked")
      (if (and (< 0 movenum)
	    (y-or-n-p (format "Do you want to move %d message(s)?" movenum)))
	(message "Moving message(s)"))      
      (if (and (< 0  trashnum)
	    (y-or-n-p (format "Do you want to move %d message(s) to trash?" trashnum)))
	(message "Trashing message(s)"))
      (if (and (< 0 deletenum)
	    (yes-or-no-p (format "Do you want to permanently delete %d message(s)?"
			   deletenum)))
	(let ((failed (mu-headers-executed-marked 'delete)))
	  (if (/= 0 failed)
	    (message "Failed to delete %d of %d message(s)" failed deletenum)
	    (message "%d message(s) deleted" deletenum)
	    (mu-headers-refresh)))))))
	  

(defun mu-headers-executed-marked (execute-action)
  "handle marked headers for action; return the number of failed
actions"
  (let ((failed 0))
    (mu-headers-foreach-marked
      (lambda (cell)
	(let ((action (nth 0 cell)) (src (nth 1 cell)) (target (nth 2 cell)))
	  (when (eq action execute-action)
	    (unless
	      (case action
		('delete (mu-message-delete src))
		(t (message "Unsupported action")))
	      (setq failed (+ 1 failed)))))))
    failed)) 
 

(defun mu-headers-foreach-marked (func)
  "call FUNC for each marked message in BUFFER; the argument
to FUNC is a list, either: with 'action', 'source' ,
'target'). ie one of three:
  ('delete <path>)
  ('trash  <path> <target>)
  ('move   <path> <target>)"  
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^." nil t)
      (move-beginning-of-line 1)
      (let* ((char0  (match-string 0))
	      (action (get-text-property 0 'action char0))
	      (path   (get-text-property 0 'path   char0))
	      (target (get-text-property 0 'target char0)))
	(cond
	  ((eq action 'trash)  (funcall func (list 'trash path target)))
	  ((eq action 'delete) (funcall func (list 'delete path)))
	  ((eq action 'move)   (funcall func (list 'move path target)))))
      (move-end-of-line 1))))

(defun mu-headers-count-marked ()
  "return a vector with three items (marked-move marked-trash
marked-delete) which are the number of messages marked for each
of those in the current buffer"
  (let ((result (make-vector 3 0)))
    (mu-headers-foreach-marked
      (lambda (cell)
	(case (car cell)
	  ('move   (aset result 0  (+ 1 (aref result 0))))
	  ('trash  (aset result 1  (+ 1 (aref result 1))))
	  ('delete (aset result 2  (+ 1 (aref result 2)))))))
    (append result nil))) ;; convert to list

(defun mu-headers-unmark-all ()
  "unmark all messages in the current buffer"
  (interactive)
  (let ((marked 0))
    (mu-headers-foreach-marked
      (lambda(cell) (setq marked (+ 1 marked))))
    (if (= 0 marked)
      (message "No messages are marked")
      (when (y-or-n-p (format "Unmark %d message(s)?" marked))
	(mu-headers-foreach-marked
	  (lambda(cell)
	    (let ((inhibit-read-only t))
	      (delete-char 1)
	      (insert-and-inherit " "))))))))

(defun mu-headers-mark-for-trash ()
  (interactive)
  (when (mu-headers-mark 'trash)
    (message "Message marked for trashing")
    (forward-line)))

(defun mu-headers-mark-for-deletion ()
  (interactive)
  (when (mu-headers-mark 'delete)
    (message "Message marked for deletion")
    (forward-line)))

(defun mu-headers-mark-for-move ()
  (interactive)
  (when (mu-headers-mark 'move)
    (message "Message marked for moving")
    (forward-line)))

(defun mu-headers-unmark ()
  (interactive)
  (when (mu-headers-mark 'none)
    (message "Message unmarked")
    (forward-line)))
   

(provide 'mu-headers)
  
