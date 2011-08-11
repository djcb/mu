;;; mua-hdrs.el -- part of mua, the mu mail user agent
;;
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

;; mu

;;; Code:
 
(eval-when-compile (require 'cl))

(require 'mua-common)
(require 'mua-msg)

;; note: these next two are *not* buffer-local, so they persist during a session
(defvar mua/hdrs-sortfield nil "field to sort headers by")
(defvar mua/hdrs-sort-descending nil "whether to sort in descending order")

(defvar mua/header-fields
  '( (:date          .  25)
     (:from-or-to    .  22)
     (:subject       .  40))
  "a list of fields and their widths")


;; internal stuff
(defvar mua/buf "" "buffer for results data")
(defvar mua/last-expression "the last search expression")
(defvar mua/hdrs-process "the mu-find process")
(defvar mua/hdrs-hash nil "the bol->path hash")
(defvar mua/hdrs-marks-hash nil "the hash for marked messages")

(defconst mua/eom "\n;;eom\n" "marker for the end of message in
the mu find output")
(defconst mua/hdrs-buffer-name "*mua-headers*"
  "name of the mua headers buffer")

(defun mua/hdrs-proc-filter (proc str)
  "process-filter for the 'mu find --format=sexp output; it
  accumulates the strings into valid sexps by checking of the
  ';;eom' end-of-msg marker, and then evaluating them"
  (let ((procbuf (process-buffer proc)))
    (when (buffer-live-p procbuf)
      (with-current-buffer procbuf
	(save-excursion
	  (setq mua/buf (concat mua/buf str))
	  (let ((eom (string-match mua/eom mua/buf)))
	    (while (numberp eom)
	      (let* ((msg (mua/msg-from-string(substring mua/buf 0 eom))))
		(save-match-data (mua/hdrs-append-message msg))
		(setq mua/buf (substring mua/buf (match-end 0)))
		(setq eom (string-match mua/eom mua/buf))))))))))

(defun mua/hdrs-proc-sentinel (proc msg)
  "Check the process upon completion"
  (let ((procbuf (process-buffer proc))
	 (status (process-status proc))
	 (exit-status (process-exit-status proc)))
    (when (and (buffer-live-p procbuf) (memq status '(exit signal)))
      (let ((msg
	      (case status
		('signal "Search process killed (results incomplete)")
		('exit
		  (if (= 0 exit-status)
		    "End of search results"
		    (mua/mu-error exit-status))))))
		  
	      (with-current-buffer procbuf
		(save-excursion
		  (goto-char (point-max))
		  (mua/message msg)))))))

(defun mua/hdrs-search-execute (expr buf)
  "search in the mu database; output the results in buffer BUF"
  (let ((args `("find" "--format=sexp" ,expr)))
    (when mua/mu-home
      (add-to-list args (concat "--muhome=" mua/mu-home)))
    (when mua/hdrs-sortfield
      (add-to-list args (concat "--sortfield=" mua/hdrs-sortfield)))
    (when mua/hdrs-sort-descending
	(add-to-list args "--descending"))
    (mua/log (concat mua/mu-binary " find " expr
	       (mapconcat 'identity args " ")))
    ;; now, do it!
    (let ((proc (apply 'start-process "*mua-headers*" buf mua/mu-binary args)))
      (setq
	mua/buf ""
	mua/hdrs-process proc)
      (set-process-filter   proc 'mua/hdrs-proc-filter)
      (set-process-sentinel proc 'mua/hdrs-proc-sentinel))))

;; Note, the 'mu find --format=sexp' sexp is almost the same as the ones that
;; 'mu view --format=sexp' produces (see mu-get-message), with the difference
;; that former may give more than one result, and that mu-headers output comes
;; from the database rather than file, and does _not_ contain the message body
(defun mua/hdrs-search (expr)
  "search in the mu database"
  (interactive "s[mu] search for: ")
  (setq debug-on-error t)

  ;; kill running process if needed
  (when (and mua/hdrs-process
	  (eq (process-status mua/hdrs-process) 'run))
    (kill-process mua/hdrs-process))
  
  (let ((buf (mua/new-buffer mua/hdrs-buffer-name)))
    (switch-to-buffer buf)
    (mua/hdrs-mode)
    (setq
      mua/last-expression expr
      mua/hdrs-hash       (make-hash-table :size 256 :rehash-size 2)
      mua/hdrs-marks-hash (make-hash-table :size 16  :rehash-size 2))
    (mua/hdrs-search-execute expr buf)))


(defun mua/hdrs-mode ()
  "major mode for displaying mua search results"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mua/hdrs-mode-map)
  
  (make-local-variable 'mua/buf)
  (make-local-variable 'mua/last-expression)
  (make-local-variable 'mua/hdrs-process)
  (make-local-variable 'mua/hdrs-hash)
  (make-local-variable 'mua/hdrs-marks-hash)

  (setq
    major-mode 'mua/mua-hdrs-mode mode-name "*mua-headers*"
    truncate-lines t buffer-read-only t
    overwrite-mode 'overwrite-mode-binary))

(defun mua/hdrs-line (msg)
  "return line describing a message (ie., a header line)"
  (let
    ((hdr
       (mapconcat
	 (lambda(fieldpair)
	   (let ((field (car fieldpair)) (width (cdr fieldpair)))
	     (case field
	       (:subject (mua/hdrs-header   msg :subject width))
	       (:to      (mua/hdrs-contact  msg field width))
	       (:from    (mua/hdrs-contact  msg field width))
	       ;;(:from-or-to (mua/msg-header-header-from-or-to msg width 'mua/header-face))
	       (:cc      (mua/hdrs-contact  msg field width))
	       (:bcc     (mua/hdrs-contact  msg field width))
	       (:date    (mua/hdrs-date     msg width))
	       (:flags   (mua/hdrs-flags    msg width))
	       (:size    (mua/hdrs-size     msg width))
	       (t        (error "Unsupported field: %S" field))
	       )))
	 mua/header-fields " ")))
    hdr))

;;
;; Note: we maintain a hash table to remember what message-path corresponds to a
;; certain line in the buffer. (mua/hdrs-set-path, mua/hdrs-get-path)
;;
;; data is stored like the following: for each header-line, we
;; take the (point) at beginning-of-line (bol) and use that as the key in the
;; mu-headers-hash hash, which does
;;
;;    point-of-bol -> path
;;

(defun mua/hdrs-set-path (path)
  "map the bol of the current header to a path"
  (puthash (line-beginning-position 1) path mua/hdrs-hash))
  
(defun mua/hdrs-get-path ()
  "get the path for the header at point"
  (gethash (line-beginning-position 1) mua/hdrs-hash))

(defun mua/hdrs-append-message (msg)
  "append a message line to the buffer and register the message"
  (let ((line (mua/hdrs-line msg)) (inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (mua/hdrs-set-path (mua/msg-field msg :path))
      (insert "  " line "\n"))))


;; Now follow a bunch of function to turn some message field in a
;; string for display

(defun mua/hdrs-header (msg field width)
  "get a string at WIDTH (truncate or ' '-pad) for display as a
header"
  (let* ((str (mua/msg-field msg field)) (str (if str str "")))
    (propertize (truncate-string-to-width str width 0 ?\s t)
      'face 'mua/header-face)))

(defun mua/hdrs-contact (msg field width)
  "get display string for a list of contacts in a header, truncated for
fitting in WIDTH"
  (unless (member field '(:to :from :bcc :cc))
    (error "Illegal type for contact"))
  (let* ((lst (mua/msg-field msg field))
	  (str (mapconcat
		 (lambda (ctc)
		   (let ((name (car ctc)) (email (cdr ctc)))
		     (or name email "?"))) lst ",")))
    (propertize (truncate-string-to-width str width 0 ?\s t)
      'face 'mua/contacts-face)))


(defun mua/hdrs-size (msg width)
  "return a string for size of MSG of WIDTH"
  (let* ((size (mua/msg-field msg :size))
	  ((str
	   (cond
	     ((>= size 1000000) (format "%2.1fM" (/ size 1000000.0)))
	     ((and (>= size 1000) (< size 1000000)) (format "%2.1fK" (/ size 1000.0)))
	     ((< size 1000) (format "%d" size)))))
    (propertize  (truncate-string-to-width str width 0 ?\s)
      'face 'mua/header-face))))


(defun mua/hdrs-date (msg width)
  "return a string for the date of MSG of WIDTH"
  (let* ((date (mua/msg-field msg :date)))
    (if date
      (propertize  (truncate-string-to-width (format-time-string "%x %X" date)
		     width 0 ?\s) 'face 'mua/date-face))))

(defun mua/hdrs-flags (msg width)
  (let* ((flags (mua/msg-field msg :flags))
	  (flagstr
	    (mapconcat
	      (lambda(flag)
		(case flag
		  ('unread    "U")
		  ('seen      "S")
		  ('replied   "R")
		  ('attach    "a")
		  ('encrypted "x")
		  ('signed    "s"))) flags "")))
    (propertize  (truncate-string-to-width flagstr width 0 ?\s)
      'face 'mua/header-face)))


;; some keybinding / functions for basic navigation

(defvar mua/hdrs-mode-map
  (let ((map (make-sparse-keymap)))
    
    (define-key map "s" 'mua/hdrs-search)
    (define-key map "q" 'mua/quit-buffer)
    (define-key map "o" 'mua/hdrs-change-sort)
    (define-key map "g" 'mua/hdrs-refresh)
    
    ;; navigation
    (define-key map "n" 'mua/hdrs-next)
    (define-key map "p" 'mua/hdrs-prev)
    (define-key map "j" 'mua/hdrs-jump-to-maildir)
    
    ;; marking/unmarking/executing
    (define-key map "m" (lambda()(interactive)(mua/hdrs-mark 'move)))
    (define-key map "d" (lambda()(interactive)(mua/hdrs-mark 'trash)))
    (define-key map "D" (lambda()(interactive)(mua/hdrs-mark 'delete)))
    (define-key map "u" (lambda()(interactive)(mua/hdrs-mark 'unmark)))
    (define-key map "U" (lambda()(interactive)(mua/hdrs-mark 'unmark-all)))
    (define-key map "x" 'mua/hdrs-marks-execute)
    
    ;; message composition
    (define-key map "r" 'mua/hdrs-reply)
    (define-key map "f" 'mua/hdrs-forward)
    (define-key map "c" 'mua/hdrs-compose)
    
    (define-key map (kbd "RET") 'mua/hdrs-view)
    map)
  "Keymap for *mua-headers* buffers.")
(fset 'mua/hdrs-mode-map mua/hdrs-mode-map)

(defun mua/hdrs-next  ()
  "go to the next line; t if it worked, nil otherwise"
  (interactive) ;; TODO: check if next line has path, if not, don't go there
  (if (or (/= 0 (forward-line 1)) (not (mua/hdrs-get-path)))
    (mua/warn "No message after this one")
    t))

(defun mua/hdrs-prev  ()
  "go to the previous line; t if it worked, nil otherwise"
  (interactive)
  (if (or (/= 0 (forward-line -1)) (not (mua/hdrs-get-path)))
    (mua/warn "No message before this one")
    t))

(defun mua/hdrs-view ()
  (interactive)
  (let ((path (mua/hdrs-get-path)))
    (if path
      (mua/view path (current-buffer))
      (mua/warn "No message at point"))))

(defun mua/hdrs-jump-to-maildir ()
  "Show the messages in one of the standard folders."
  (interactive)
  (let ((fld (mua/ask-maildir "Jump to maildir: ")))
    (mua/hdrs-search (concat "maildir:" fld))))

(defun mua/hdrs-refresh ()
  "Re-run the query for the current search expression, but only
if the search process is not already running"
  (interactive)
  (when mua/last-expression (mua/hdrs-search mua/last-expression)))


;;; functions for sorting
(defun mua/hdrs-change-sort-order (fieldchar)
  "Change the sortfield to FIELDCHAR."
  (interactive "cField to sort by ('d', 's', etc.; see mu-headers(1)):\n")
  (let ((field
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
      (setq mua/hdrs-sortfield field)
      (mua/warn "Invalid sort-field; use one of bcdfimpstz (see mu-headers(1)"))
    field))

(defun mua/hdrs-change-sort-direction (dirchar)
  "Change the sort direction, either [a]scending or [d]escending."
  (interactive)
  (setq mua/hdrs-sort-descending
    (y-or-n-p "Set sorting direction to descending(y) or ascending(n)")))
     

(defun mua/hdrs-change-sort ()
  "Change thee sort field and direction."
  (interactive)
  (and (call-interactively 'mua/hdrs-change-sort-order)
    (call-interactively 'mua/hdrs-change-sort-direction)))



;;; functions for marking

(defun mua/hdrs-add-marked (src &optional dst)
  "Add the message at point to the markings hash"
  (let ((bol (line-beginning-position 1)))
    (if (gethash bol mua/hdrs-marks-hash)
      (mua/warn "Message is already marked")
      (progn (puthash bol (cons src dst) mua/hdrs-marks-hash) t))))

(defun mua/hdrs-remove-marked ()
  "Remove the message at point from the markings hash"
  (let ((bol (line-beginning-position 1)))
    (if (not (gethash bol mua/hdrs-marks-hash))
      (mua/warn "Message is not marked")
      (progn (remhash bol mua/hdrs-marks-hash) t))))

(defun mua/hdrs-set-marker (kar)
  "Set the marker at the beginning of this line."
  (beginning-of-line 1)
  (let ((inhibit-read-only t))
    (delete-char 2)
    (insert (if kar (format "%c " kar) "  "))))
	
(defun mua/hdrs-mark (action)
  "Mark the message at point with one of the symbols: move,
delete, trash, unmark, unmark-all; the latter two are
pseudo-markings."
  (let ((target) (src (mua/hdrs-get-path)))
    (when src
      (case action
	(move
	  (when (mua/hdrs-add-marked src
		  (mua/ask-maildir "Target maildir: " t)) ;; t->return fullpath
	    (mua/hdrs-set-marker ?m)))
	(trash
	  (when (mua/hdrs-add-marked src
		  (concat mua/maildir mua/trash-folder))
	    (mua/hdrs-set-marker ?d)))
	(delete
	  (when (mua/hdrs-add-marked src "/dev/null")
	    (mua/hdrs-set-marker ?D)))
	(unmark
	  (when (mua/hdrs-remove-marked)
	    (mua/hdrs-set-marker nil)))
	(unmark-all
	  (when (y-or-n-p (format "Sure you want to remove all (%d) marks? "
			    (hash-table-count mua/hdrs-marks-hash)))
	    (save-excursion
	      (maphash (lambda (k v) (goto-char k) (mua/hdrs-mark 'unmark))
		mua/hdrs-marks-hash))))
	(t (error "Unsupported mark type")))
      (move-beginning-of-line 2))))
    
(defun mua/hdrs-marks-execute ()
  "execute the actions for all marked messages"
  (interactive)
  (let ((n-marked (hash-table-count mua/hdrs-marks-hash)))
    (if (= 0 n-marked)
      (mua/warn "No marked messages")
      (when (y-or-n-p
	      (format "Execute actions for %d marked message(s)? " n-marked))
	(save-excursion
	  (maphash
	    (lambda(bol v)
	      (let* ((src (car v)) (target (cdr v)) (inhibit-read-only t)
		     (newpath (mua/msg-move src target)))
		(when newpath
		  ;; remember the updated path -- for now not too useful
		  ;; as we're hiding the header, but...
		  (mua/hdrs-set-path newpath) 
		  (goto-char bol)
		  (mua/hdrs-remove-marked)
		  (mua/warn "[%d %d]" (line-beginning-position 1)
		    (line-beginning-position 2))
		  (put-text-property (line-beginning-position 1)
		    (line-beginning-position 2)
		    'invisible t)))) ;; when it succeedes, hide msg..)
	    mua/hdrs-marks-hash))))))



;; functions for creating new message -- reply, forward, and new
(defun mua/hdrs-reply ()
  "Reply to message at point."
  (interactive)
  (let* ((path (mua/hdrs-get-path))
	  (str (when path (mua/mu-view-sexp path)))
	  (msg (and str (mua/msg-from-string str))))
    (if msg
      (mua/msg-reply msg)	  
      (mua/warn "No message at point"))))
      
(defun mua/hdrs-forward ()
  "Forward the message at point."
  (interactive)
  (let* ((path (mua/hdrs-get-path))
	  (msg (when path (mua/msg-from-path path))))
    (if msg
      (mua/msg-forward msg)
      (mua/warn "No message at point"))))

(defun mua/hdrs-compose ()
  "Create a new message."
  (interactive)
  (mua/msg-compose-new))


(provide 'mua-hdrs)
