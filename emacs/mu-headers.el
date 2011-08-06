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

;; data is stored like the following: for each header-line, we take the (point)
;; at beginning-of-line (bol) and use that as the key in the mu-headers-hash
;; hash, which does
;;
;;    point-of-bol -> path
;;
;; then, marks are stored in a seperate hash 'mu-headers-marks-hash, using
;;
;;   point-of-bol -> (src . target)
;;
;;  and note both 'delete' (target=/dev/null), trash (target=trash-folder), and
;;  move can be expressed by that
;;
;; after the marks have been 'executed', the lines will be marked a *invisible*
;; instead of deleting them; that way, the 'point-of-bol' stays valid.

(defvar mu-headers-hash nil "internal: buffer-local hash table
which maps bol->path")
(defvar mu-headers-marks-hash nil "internal: buffer-local hash table
which maps bol->(src . target) for marked lines")
  
(defun mu-headers-set-path (path)
  "map the bol of the current header to a path"
  (puthash (line-beginning-position 1) path mu-headers-hash))
  
(defun mu-headers-get-path ()
  "get the path for the header at point"
  (gethash (line-beginning-position 1) mu-headers-hash))

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

(defvar mu-buf "" "internal: buffer for results data")
(defun mu-headers-process-filter (proc str)
  "process-filter for the 'mu find --format=sexp output; it
  accumulates the strings into valid sexps by checking of the
  ';;eom' end-of-msg marker, and then evaluating them"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc) 
      (save-excursion
	(setq mu-buf (concat mu-buf str))
	(let ((eom (string-match mu-eom mu-buf)))
	  (while (numberp eom)
	    (let* ((msg (car (read-from-string (substring mu-buf 0 eom))))
		    (inhibit-read-only t))
	      (goto-char (point-max))
	      (mu-headers-set-path (plist-get msg :path))
	      (save-match-data (insert (mu-headers-header msg) ?\n)))
	    (setq mu-buf (substring mu-buf (match-end 0)))
	    (setq eom (string-match mu-eom mu-buf))))))))
  
(defun mu-headers-process-sentinel (proc msg)
  "Check the mu-headers process upon completion"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
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
	      (insert (mu-str text)))))))))
  

;; Note, the 'mu find --format=sexp' sexp is almost the same as the ones that
;; 'mu view --format=sexp' produces (see mu-get-message), with the difference
;; that former may give more than one result, and that mu-headers output comes
;; from the database rather than file, and does _not_ contain the message body
(defun mu-headers-search (expr)
  "search in the mu database"
  (interactive "s[mu] search for: ")
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
    (mu-log "search: '%s'" expr)
    (switch-to-buffer buf)
    (mu-headers-mode)
    
    (setq
      mu-buf "" ;; if the last query went wrong... 
      mu-headers-expression expr
      mu-headers-process proc

      mu-headers-hash       (make-hash-table :size 256 :rehash-size 2)
      mu-headers-marks-hash (make-hash-table :size 16  :rehash-size 2))
    
    (set-process-filter   proc 'mu-headers-process-filter)
    (set-process-sentinel proc 'mu-headers-process-sentinel)))

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
  (concat "  "
    (mapconcat
      (lambda (fieldinfo)
	(mu-headers-field msg fieldinfo)) mu-headers-fields " ")))


(defun mu-headers-mode ()
  "major mode for displaying search results"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mu-headers-mode-map)

  (make-local-variable 'mu-buf)
  (make-local-variable 'mu-parent-buffer)
  (make-local-variable 'mu-headers-expression)
  (make-local-variable 'mu-headers-process)
  (make-local-variable 'mu-headers-hash)
  (make-local-variable 'mu-headers-marks-hash)

  (setq
    major-mode 'mu-headers-mode mode-name "*headers*"
    mu-buf ""
    truncate-lines t buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defvar mu-headers-mode-map
  (let ((map (make-sparse-keymap)))
    
    (define-key map "s" 'mu-headers-search)
    (define-key map "q" 'mu-quit-buffer)
    (define-key map "s" 'mu-headers-change-sort)
    (define-key map "g" 'mu-headers-refresh)

    ;; navigation
    (define-key map "n" 'mu-headers-next)
    (define-key map "p" 'mu-headers-previous)
    (define-key map "j" 'mu-headers-jump-to-maildir)
    
    ;; marking/unmarking/executing
    (define-key map "m" 'mu-headers-mark-for-move)
    (define-key map "d" 'mu-headers-mark-for-trash)
    (define-key map "D" 'mu-headers-mark-for-deletion)
    (define-key map "u" 'mu-headers-unmark)
    (define-key map "U" 'mu-headers-unmark-all)
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
  (let ((path (mu-headers-get-path)))
    (when path (mu-view path (current-buffer)))))

(defun mu-headers-next  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (or (/= 0 (forward-line 1)) (not (mu-headers-get-path)))    
    (progn (message "No message after this one") nil)
    t))

(defun mu-headers-prev  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive)
  (if (/= 0 (forward-line -1))
    (progn (message "No message before this one") nil)
    t))

(defun mu-headers-jump-to-maildir ()
  "show the messages in one of the standard folders"
  (interactive)
  (let ((fld (mu-ask-maildir "Jump to maildir: ")))
    (mu-headers-search (concat "maildir:" fld))))

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

(defun mu-headers-add-marked (src &optional dst)
  (let ((bol (line-beginning-position 1)))
    (if (gethash bol mu-headers-marks-hash)
      (progn (message "Message is already marked") nil)
      (progn (puthash bol (cons src dst) mu-headers-marks-hash) t))))

(defun mu-headers-remove-marked ()
  (let ((bol (line-beginning-position 1)))
    (if (not (gethash bol mu-headers-marks-hash))
      (progn (message "Message is not marked") nil)
      (progn (remhash bol mu-headers-marks-hash) t))))

(defun mu-headers-set-marker (kar)
  "set the marker at the beginning of this line"
  (beginning-of-line 1)
  (let ((inhibit-read-only t))
    (delete-char 2)
    (insert (if kar kar " ") " ")))

(defun mu-headers-mark (action)
  "mark the current msg for something: move, delete, trash, unmark"
  (let ((target) (src (mu-headers-get-path)))
    (when src
      (case action
	(move
	  (when (mu-headers-add-marked src
		  (mu-ask-maildir "Target maildir: " t))
	    (mu-headers-set-marker ?m)))
	(trash
	  (when (mu-headers-add-marked src
		  (concat mu-maildir mu-trash-folder))
	    (mu-headers-set-marker ?d)))
	(delete
	  (when (mu-headers-add-marked src "/dev/null")
	    (mu-headers-set-marker ?D)))
	(unmark
	  (when (mu-headers-remove-marked)
	    (mu-headers-set-marker nil)))
	(unmark-all
	  (when (y-or-n-p (format "Sure you want to remove all (%d) marks? "
			    (hash-table-count mu-headers-marks-hash)))
	    (save-excursion
	      (maphash (lambda (k v) (goto-char k) (mu-headers-mark 'unmark))
		mu-headers-marks-hash)))
	  (t (message "Unsupported mark type"))))
      (move-beginning-of-line 2))))
    
(defun mu-headers-marks-execute ()
  "execute the actions for all marked messages"
  (interactive)
  (let ((n-marked (hash-table-count mu-headers-marks-hash)))
    (if (= 0 n-marked)
      (message "No marked messages")
      (when (y-or-n-p
	      (format "Execute actions for %d marked message(s)? " n-marked))
	(save-excursion
	  (maphash
	    (lambda(bol v)
	      (let ((src (car v)) (target (cdr v)) (inhibit-read-only t))
		(when (mu-message-move src target)  
		  (goto-char bol)
		  (mu-headers-remove-marked)
		  (put-text-property (line-beginning-position 1)
		    (line-beginning-position 2)
		    'invisible t)))) ;; when it succeedes, hide msg..)
	    mu-headers-marks-hash))
	(message "Done")
))))

(defun mu-headers-mark-for-move () (interactive) (mu-headers-mark 'move))
(defun mu-headers-mark-for-trash () (interactive) (mu-headers-mark 'trash))
(defun mu-headers-mark-for-delete () (interactive) (mu-headers-mark 'delete))
(defun mu-headers-mark-for-deletion () (interactive) (mu-headers-mark 'delete))
(defun mu-headers-unmark () (interactive) (mu-headers-mark 'unmark))
(defun mu-headers-unmark-all () (interactive) (mu-headers-mark 'unmark-all))

(defun mu-headers-reply ()
  "Reply to the message at point"
  (interactive)
  (let ((path (mu-headers-get-path)))
    (if path
      (mu-message-reply path)
      (message "No message at point"))))

(defun mu-headers-forward ()
  "Reply to the message at point"
  (interactive)
  (let ((path (mu-headers-get-path)))
    (if path
      (mu-message-forward path)
      (message "No message at point"))))


(provide 'mu-headers)
  
