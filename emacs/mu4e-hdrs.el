;;; mu4e-hdrs.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

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

;; In this file are function related mu4e-hdrs-mode, to creating the list of
;; one-line descriptions of emails, aka 'headers' (not to be confused with
;; headers like 'To:' or 'Subject:')

;; Code:
(require 'hl-line)

(require 'mu4e-proc)
(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-mark)

;;;; internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst mu4e~hdrs-fringe "  " "*internal* The space on the left of
message headers to put marks.")

(defun mu4e~hdrs-clear ()
  "Clear the header buffer and related data structures."
  (when (buffer-live-p mu4e~hdrs-buffer)
    (let ((inhibit-read-only t))
      (with-current-buffer mu4e~hdrs-buffer
	(erase-buffer)
	(mu4e~mark-clear)))))


(defun mu4e-hdrs-search (expr &optional full-search)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results. If FULL-SEARCH is non-nil return all
results, otherwise, limit number of results to
`mu4e-search-results-limit'."
  (let ((buf (get-buffer-create mu4e~hdrs-buffer-name))
	  (inhibit-read-only t))
    (with-current-buffer buf
      (mu4e-hdrs-mode)
      (setq
	global-mode-string (propertize expr 'face 'mu4e-title-face)
	mu4e-last-expr expr
	mu4e~hdrs-buffer buf
	mode-name "mu4e-headers"))
    (switch-to-buffer buf)
    (mu4e~proc-find
      (replace-regexp-in-string "\"" "\\\\\"" expr) ;; escape "\"
      (unless full-search mu4e-search-results-limit))
    ;;; when we're starting a new search, we also kill the
    ;;; view buffer, if any
    (mu4e-view-kill-buffer-and-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler functions
;;
;; next are a bunch of handler functions; those will be called from mu4e~proc in
;; response to output from the server process


(defun mu4e~hdrs-view-handler (msg)
  "Handler function for displaying a message."
  (mu4e-view msg mu4e~hdrs-buffer))

(defun mu4e~hdrs-update-handler (msg is-move)
  "Update handler, will be called when a message has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p mu4e~hdrs-buffer)
    (with-current-buffer mu4e~hdrs-buffer
      (let* ((docid (plist-get msg :docid))
 	      (point (mu4e~docid-pos docid)))
	(when point ;; is the message present in this list?

	  ;; if it's marked, unmark it now
	  (when (mu4e-mark-docid-marked-p docid)
	    (mu4e-mark-set 'unmark))

 	  ;; re-use the thread info from the old one; this is needed because
 	  ;; *update* message don't have thread info by themselves (unlike
 	  ;; search results)
 	  ;; but since we still have the search results, re-use those
 	  (plist-put msg :thread
 	    (mu4e~field-for-docid docid :thread))

	  ;; first, remove the old one (otherwise, we'd have two headers with
	  ;; the same docid...
	  (mu4e~hdrs-remove-handler docid)

	  ;; if we we're actually viewing this message (in mu4e-view mode), we
	  ;; update the `mu4e-current-msg' there as well; that way, the flags can
	  ;; be updated, as well as the path (which is useful for viewing the
	  ;; raw message)
	  (let ((viewbuf (get-buffer mu4e-view-buffer-name)))
	    (when (and viewbuf (buffer-live-p viewbuf))
	      (with-current-buffer viewbuf
		(when (eq docid (plist-get mu4e-current-msg :docid))
		  (mu4e-view msg mu4e~hdrs-buffer)))))

	  ;; now, if this update was about *moving* a message, we don't show it
	  ;; anymore (of course, we cannot be sure if the message really no
	  ;; longer matches the query, but this seem a good heuristic.
	  ;; if it was only a flag-change, show the message with its updated flags.
	  (unless is-move
	    (mu4e~hdrs-header-handler msg point))

	  ;; attempt to highlight the corresponding line and make it visible
	  (mu4e~hdrs-highlight docid))))))


(defun mu4e~hdrs-remove-handler (docid)
  "Remove handler, will be called when a message has been removed
from the database. This function will hide the removed message from
the current list of headers."
  (when (buffer-live-p mu4e~hdrs-buffer)
    (with-current-buffer mu4e~hdrs-buffer
      (mu4e~hdrs-remove-header docid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e~hdrs-contact-str (contacts)
  "Turn the list of contacts CONTACTS (with elements (NAME . EMAIL)
into a string."
  (mapconcat
    (lambda (ct)
      (let ((name (car ct)) (email (cdr ct)))
	(or name email "?"))) contacts ", "))

(defun mu4e-thread-prefix (thread)
  "Calculate the thread prefix based on thread info THREAD."
  (if thread
    (let ( (level        (plist-get thread :level))
	   (first-child  (plist-get thread :first-child))
	   (has-child    (plist-get thread :has-child))
	   (duplicate    (plist-get thread :duplicate))
	   (empty-parent (plist-get thread :empty-parent)))
      (concat
	(make-string (* (if empty-parent 0 2) level) ?\s)
	(cond
	  (has-child    "+ ")
	  (empty-parent "- ")
	  (first-child  "\\ ")
	  (duplicate    "= ")
	  (t            "| "))))))

(defun mu4e~hdrs-header-handler (msg &optional point)
  "Create a one line description of MSG in this buffer, at POINT,
if provided, or at the end of the buffer otherwise."
  (when (buffer-live-p mu4e~hdrs-buffer)
  (let* ((docid (plist-get msg :docid))
	  (line
	   (mapconcat
	     (lambda (f-w)
	       (let* ((field (car f-w)) (width (cdr f-w))
		       (val (plist-get msg field))
		       (str
			 (case field
			   (:subject
			     (concat ;; prefix subject with a thread indicator
			       (mu4e-thread-prefix (plist-get msg :thread))
			       val))
			   ((:maildir :path) val)
			   ((:to :from :cc :bcc) (mu4e~hdrs-contact-str val))
			   ;; if we (ie. `user-mail-address' is the 'From', show
			   ;; 'To', otherwise show From
			   (:from-or-to
			     (let* ((from-lst (plist-get msg :from))
				     (from (and from-lst (cdar from-lst))))
			       (if (and from (string-match
					       mu4e-user-mail-address-regexp from))
				 (concat "To "
				   (mu4e~hdrs-contact-str (plist-get msg :to)))
				 (mu4e~hdrs-contact-str from-lst))))
			   (:date (format-time-string mu4e-headers-date-format val))
			   (:flags (mu4e-flags-to-string val))
			   (:size (mu4e-display-size val))
			   (t (error "Unsupported header field (%S)" field)))))
		 (when str
		   (if (not width)
		     str
		     (truncate-string-to-width str width 0 ?\s t)))))
	     mu4e-headers-fields " "))
	  (flags (plist-get msg :flags))
	  (line	(cond
		  ((member 'draft flags)
		    (propertize line 'face 'mu4e-draft-face 'draft t))
		  ((member 'trashed flags)
		    (propertize line 'face 'mu4e-trashed-face))
		  ((member 'unread flags)
		    (propertize line 'face 'mu4e-unread-face))
		  (t ;; else
		    (propertize line 'face 'mu4e-header-face)))))

    ;; now, append the header line
    (mu4e~hdrs-add-header line docid point msg))))

(defun mu4e~hdrs-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (when (buffer-live-p mu4e~hdrs-buffer)
  (with-current-buffer mu4e~hdrs-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t)
	     (str (if (= 0 count)
		    "No matching messages found"
		    "End of search results")))
	(insert (propertize str 'face 'mu4e-system-face 'intangible t))
	(unless (= 0 count)
	  (message "Found %d matching message%s"
	    count (if (= 1 count) "" "s"))
	  ;; highlight the first message
	  (mu4e~hdrs-highlight (mu4e~docid-at-point (point-min)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 

;;; hdrs-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-hdrs-mode-map nil
  "Keymap for *mu4e-headers* buffers.")
(unless mu4e-hdrs-mode-map
  ;; add some quick funcs so our key descriptions below are shorter
  (defun mu4e~hdrs-mark-trash()(interactive)(mu4e-hdrs-mark-and-next 'trash))
  (defun mu4e~hdrs-mark-delete()(interactive)(mu4e-hdrs-mark-and-next 'delete))
  (defun mu4e~hdrs-mark-unmark()(interactive)(mu4e-hdrs-mark-and-next 'unmark))
  (defun mu4e~hdrs-mark-read()(interactive)(mu4e-hdrs-mark-and-next 'read))
  (defun mu4e~hdrs-mark-unread()(interactive)(mu4e-hdrs-mark-and-next 'unread))
  
  (setq mu4e-hdrs-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "s" 'mu4e-search)

      (define-key map "b" 'mu4e-search-bookmark)
      (define-key map "B" 'mu4e-search-bookmark-edit-first)

      (define-key map "q" 'mu4e~hdrs-kill-buffer-and-window)
      (define-key map "z" 'mu4e~hdrs-kill-buffer-and-window)

      (define-key map "r" 'mu4e-rerun-search)
      (define-key map "g" 'mu4e-rerun-search) ;; for compatibility

      ;; navigation
      (define-key map "n" 'mu4e-next-header)
      (define-key map "p" 'mu4e-prev-header)
      ;; the same
      (define-key map (kbd "<M-down>") 'mu4e-next-header)
      (define-key map (kbd "<M-up>") 'mu4e-prev-header)

      ;; switching to view mode (if it's visible)
      (define-key map "y" 'mu4e-select-other-view)

      ;; marking/unmarking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define-key map (kbd "<backspace>") 'mu4e~hdrs-mark-trash)
      (define-key map (kbd "d") 'mu4e~hdrs-mark-trash)

      (define-key map (kbd "<delete>") 'mu4e~hdrs-mark-delete)
      (define-key map (kbd "<deletechar>") 'mu4e~hdrs-mark-delete)
      (define-key map (kbd "D") 'mu4e~hdrs-mark-delete)
      
      (define-key map (kbd "o") 'mu4e~hdrs-mark-unread)
      (define-key map (kbd "r") 'mu4e~hdrs-mark-read)
      (define-key map (kbd "u") 'mu4e~hdrs-mark-unmark)
 
      (define-key map "m" 'mu4e-hdrs-mark-for-move-and-next)
      
      (define-key map "U" 'mu4e-mark-unmark-all)
      (define-key map "x" 'mu4e-mark-execute-all)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     

      (define-key map "j" 'mu4e-jump-to-maildir)
      (define-key map "a" 'mu4e-hdrs-action) 
      
      ;; message composition
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-compose-edit)

      (define-key map (kbd "RET") 'mu4e-view-message)
      (define-key map [mouse-2]   'mu4e-view-message)

      (define-key map "$" 'mu4e-show-log)
      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Headers")))
	(define-key map [menu-bar headers] (cons "Headers" menumap))

	(define-key menumap [mu4e~hdrs-kill-buffer-and-window]
	  '("Quit view" . mu4e~hdrs-kill-buffer-and-window))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	
	(define-key menumap [execute-marks]  '("Execute marks"
						. mu4e-mark-execute-all))
	(define-key menumap [unmark-all]  '("Unmark all" . mu4e-mark-unmark-all))
	(define-key menumap [unmark]      '("Unmark" . mu4e~hdrs-mark-unmark))

	(define-key menumap [mark-as-read]  '("Mark as read" . mu4e~hdrs-mark-read))
	(define-key menumap [mark-as-unread]
	  '("Mark as unread" .  mu4e~hdrs-mark-unread))

	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mu4e~hdrs-mark-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mu4e~hdrs-mark-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mu4e-hdrs-mark-for-move-and-next))
	(define-key menumap [sepa1] '("--"))

	(define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
	(define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
	(define-key menumap [sepa2] '("--"))

	(define-key menumap [refresh]  '("Refresh" . mu4e-rerun-search))
	(define-key menumap [search]  '("Search" . mu4e-search))

	(define-key menumap [jump]  '("Jump to maildir" . mu4e-jump-to-maildir))
	(define-key menumap [sepa3] '("--"))

	(define-key menumap [view]  '("View" . mu4e-view-message))
	(define-key menumap [next]  '("Next" . mu4e-next-header))
	(define-key menumap [previous]  '("Previous" . mu4e-prev-header))
	(define-key menumap [sepa4] '("--")))
      map)))

(fset 'mu4e-hdrs-mode-map mu4e-hdrs-mode-map)


(define-derived-mode mu4e-hdrs-mode special-mode
    "mu4e:headers"
  "Major mode for displaying mu4e search results.
\\{mu4e-hdrs-mode-map}."
  (use-local-map mu4e-hdrs-mode-map)

  (make-local-variable 'mu4e-last-expr)
  (make-local-variable 'mu4e~hdrs-proc)
  (make-local-variable 'mu4e~highlighted-docid)

  (make-local-variable 'global-mode-string)
  (make-local-variable 'hl-line-face)

  (setq
    truncate-lines t
    buffer-undo-list t ;; don't record undo information
    overwrite-mode 'overwrite-mode-binary
    hl-line-face 'mu4e-header-highlight-face)
  (mu4e~mark-initialize) ;; initialize the marking subsystem
  (hl-line-mode 1)

  (setq header-line-format
    (cons
      (make-string
	(+ (length mu4e~hdrs-fringe) (floor (fringe-columns 'left t))) ?\s)
      (map 'list
	(lambda (item)
	  (let ((field (cdr (assoc (car item) mu4e-header-names)))
		 (width (cdr item)))
	    (concat
	      (propertize
		(if width
		  (truncate-string-to-width field width 0 ?\s t)
		  field)
		'face 'mu4e-header-title-face) " ")))
	mu4e-headers-fields))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; higlighting
(defvar mu4e~highlighted-docid nil
  "*internal* The highlighted docid")

(defun mu4e~hdrs-highlight (docid)
  "Highlight the header with DOCID, or do nothing if it's not
found. Also, unhighlight any previously highlighted headers."
  (with-current-buffer mu4e~hdrs-buffer
    (save-excursion
      ;; first, unhighlight the previously highlighted docid, if any
      (when (and mu4e~highlighted-docid
	      (mu4e~goto-docid mu4e~highlighted-docid))
	(hl-line-unhighlight))
      ;; now, highlight the new one
      (when (mu4e~goto-docid docid)
	(hl-line-highlight)))
    (setq mu4e~highlighted-docid docid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e-select-headers-window-if-visible ()
  "When there is a visible window for the headers buffer, make sure
to select it. This is needed when adding new headers, otherwise
adding a lot of new headers looks really choppy."
  (let ((win (get-buffer-window mu4e~hdrs-buffer)))
    (when win (select-window win))))

;;;; headers in the buffer are prefixed by an invisible string with the docid
;;;; followed by an EOT ('end-of-transmission', \004, ^D) non-printable ascii
;;;; character. this string also has a text-property with the docid. the former
;;;; is used for quickly finding a certain header, the latter for retrieving the
;;;; docid at point without string matching etc.

(defun mu4e~docid-cookie (docid)
  "Create an invisible string containing DOCID; this is to be used
at the beginning of lines to identify headers."
  (propertize (format "%d\004" docid) 'docid docid 'invisible t))

(defun mu4e~docid-at-point (&optional point)
  "Get the docid for the header at POINT, or at current (point) if
nil. Returns the docid, or nil if there is none."
    (save-excursion
      (when point
	(goto-char point))
      (get-text-property (line-beginning-position) 'docid)))

(defun mu4e~goto-docid (docid &optional to-mark)
  "Go to the beginning of the line with the header with docid
DOCID, or nil if it cannot be found. If the optional TO-MARK is
non-nil, go to the point directly *after* the docid-cookie instead
of the beginning of the line."
  (let ((oldpoint (point)) (newpoint))
    (goto-char (point-min))
    (setq newpoint
      (search-forward (format "%d\004" docid) nil t))
    (when (null to-mark)
      (if (null newpoint)
	(goto-char oldpoint) ;; not found; restore old pos
	(progn
	  (beginning-of-line) ;; found, move to beginning of line
	  (setq newpoint (point)))))
    newpoint)) ;; return the point, or nil if not found

(defun mu4e~docid-pos (docid)
  "Return the pos of the beginning of the line with the header with
docid DOCID, or nil if it cannot be found."
  (let ((pos))
    (save-excursion
      (setq pos (mu4e~goto-docid docid)))
    pos))

(defun mu4e~field-for-docid (docid field)
  "Get FIELD (a symbol, see `mu4e-headers-names') for the message
with DOCID which must be present in the headers buffer."
  (save-excursion
    (when (mu4e~goto-docid docid)
      (mu4e-field-at-point field))))

;;;; markers mark headers for
(defun mu4e~mark-header (docid mark)
  "(Visually) mark the header for DOCID with character MARK."
  (with-current-buffer mu4e~hdrs-buffer
    (let ((inhibit-read-only t) (oldpoint (point)))
      (unless (mu4e~goto-docid docid)
	(error "Cannot find message with docid %S" docid))
      ;; now, we're at the beginning of the header, looking at
      ;; <docid>\004
      ;; (which is invisible). jumpp past that…
      (unless (re-search-forward "\004" nil t)
	  (error "Cannot find \004 separator"))
      ;; we found the \004; we move point one to the right for the
      ;; the area to write the marker.
      ;;(forward-char)
      ;; clear old marks, and add the new ones.
      (delete-char (length mu4e~hdrs-fringe))
      (insert (propertize mark 'face 'mu4e-hdrs-marks-face) " ")
      (goto-char oldpoint))))


(defun mu4e~hdrs-add-header (str docid point &optional msg)
  "Add header STR with DOCID to the buffer at POINT if non-nil, or
at (point-max) otherwise. If MSG is not nil, add it as the text-property `msg'."
  (unless docid (error "Invalid message"))
  (when (buffer-live-p mu4e~hdrs-buffer)
    (with-current-buffer mu4e~hdrs-buffer
      (let ((inhibit-read-only t)
	     (is-first-header (= (point-min) (point-max))))
	(save-excursion
	  (goto-char (if point point (point-max)))
	  (insert
	    (propertize
	      (concat
		(mu4e~docid-cookie docid)
		mu4e~hdrs-fringe str "\n") 'docid docid 'msg msg)))))))

(defun mu4e~hdrs-remove-header (docid)
  "Remove header with DOCID at POINT."
  (with-current-buffer mu4e~hdrs-buffer
    (unless (mu4e~goto-docid docid)
      (error "Cannot find message with docid %S" docid))
    (let ((inhibit-read-only t))
      (delete-region (line-beginning-position) (line-beginning-position 2)))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-search (expr)
    "Start a new mu search. If prefix ARG is nil, limit the number of
results to `mu4e-search-results-limit', otherwise show all. In
other words, use the C-u prefix to get /all/ results, otherwise get
up to `mu4e-search-results-limit' much quicker."
    (interactive "s[mu] search for: ")
    (when (mu4e-mark-handle-when-leaving)
      (mu4e-hdrs-search expr current-prefix-arg)))

(defun mu4e-search-bookmark ()
  "Search using some bookmarked query. With C-u prefix, show /all/ results,
otherwise, limit to up to `mu4e-search-results-limit'."
  (interactive)
  (let ((query (mu4e-ask-bookmark "Bookmark: ")))
    (when (and query (mu4e-mark-handle-when-leaving))
      (mu4e-hdrs-search query current-prefix-arg))))

(defun mu4e-search-bookmark-edit-first (expr)
  "Search using some bookmarked query, but allow for editing the
bookmark before submitting it. With C-u prefix, show /all/ results,
otherwise, limit to up to `mu4e-search-results-limit'."
  (interactive
    (list (read-string "[mu] search for: "
	    (concat (or (mu4e-ask-bookmark "Edit bookmark: ") "") " "))))
  (when (and expr (mu4e-mark-handle-when-leaving))
    (mu4e-hdrs-search expr current-prefix-arg)))


(defun mu4e-view-message ()
  "View message at point. If there's an existing window for the
view, re-use that one. If not, create a new one, depending on the
value of `mu4e-split-view': if it's a symbol `horizontal' or
`vertical', split the window accordingly; if it is nil, replace the
current window. "
  (interactive)
  (unless (eq major-mode 'mu4e-hdrs-mode)
    (error "Must be in mu4e-hdrs-mode (%S)" major-mode))
  (let* ((docid (mu4e~docid-at-point))
	  (viewwin (and mu4e-view-buffer
		     (get-buffer-window mu4e-view-buffer))))
    (unless docid (error "No message at point."))
    ;; is there a window already for the message view?
    (unless (window-live-p viewwin)
      ;; no view window yet; create one, based on the split settings etc.
      ;; emacs' use of the terms "horizontally" and "vertically"
      ;; are... suprising. There's a clearer `split-window' in emacs24, but
      ;; it's not compatible with emacs 23
      (setq viewwin
	(cond ;; is there are live window for the message view?
	  ((eq mu4e-split-view 'horizontal) ;; split horizontally
	    (split-window-vertically mu4e-headers-visible-lines))
	  ((eq mu4e-split-view 'vertical) ;; split vertically
	    (split-window-horizontally mu4e-headers-visible-columns))
	  (t ;; no splitting; just use the currently selected one
	    (selected-window)))))
    ;; okay, now we should have a window for the message view
    ;; we select it, and show the messages there.
    (select-window viewwin)
    (switch-to-buffer (get-buffer-create mu4e-view-buffer-name))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Waiting for message..."
		'face 'mu4e-system-face 'intangible t)))
    (mu4e~proc-view docid)))

(defun mu4e~hdrs-kill-buffer-and-window ()
  "Quit the message view and return to the main view."
  (interactive)
  (when  (mu4e-mark-handle-when-leaving)
    (let ((buf mu4e~hdrs-buffer))
      (when (buffer-live-p buf)
	(bury-buffer)
	(delete-windows-on buf) ;; destroy all windows for this buffer
	(kill-buffer buf)))
    (mu4e-main-view)))
 

(defun mu4e-rerun-search ()
  "Rerun the search for the last search expression; if none exists,
do a new search."
  (interactive)
   (when (mu4e-mark-handle-when-leaving)
    (if mu4e-last-expr
      (mu4e-hdrs-search mu4e-last-expr)
      (call-interactively 'mu4e-search))))

(defun mu4e~hdrs-move (lines)
  "Move point LINES lines forward (if LINES is positive) or
backward (if LINES is negative). If this succeeds, return the new
docid. Otherwise, return nil."
  (unless (eq major-mode 'mu4e-hdrs-mode)
    (error "Must be in mu4e-hdrs-mode (%S)" major-mode))
  (let ((succeeded (= 0 (forward-line lines)))
	 (docid (mu4e~docid-at-point)))
    ;; trick to move point, even if this function is called when this window
    ;; is not visible
    (when docid
      (set-window-point (get-buffer-window mu4e~hdrs-buffer) (point))
      ;; attempt to highlight the new line, display the message
      (mu4e~hdrs-highlight docid)
      ;; if there already is a visible message view, show the message
      (when (and (buffer-live-p mu4e-view-buffer)
	      (window-live-p (get-buffer-window mu4e-view-buffer)))
	(mu4e-view-message)))
    ;; return the docid only if the move succeeded
    (when succeeded docid)))

(defun mu4e-next-header ()
  "Move point to the next message header. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~hdrs-move 1))

(defun mu4e-prev-header ()
  "Move point to the previous message header. If this succeeds,
return the new docid. Otherwise, return nil."
  (interactive)
  (mu4e~hdrs-move -1))


(defun mu4e-jump-to-maildir ()
  "Show the messages in maildir (user is prompted to ask what
maildir). With C-u prefix, show /all/ results, otherwise, limit to
up to `mu4e-search-results-limit'."
  (interactive)
  (let ((fld (mu4e-ask-maildir "Jump to maildir: ")))
    (when (and fld (mu4e-mark-handle-when-leaving))
      (mu4e-hdrs-search (concat "\"maildir:" fld "\"")
	current-prefix-arg))))


(defun mu4e-compose (compose-type)
  "Start composing a message of COMPOSE-TYPE, where COMPOSE-TYPE is
a symbol, one of `reply', `forward', `edit', `new'. All but `new'
take the message at point as input. Symbol `edit' is only allowed
for draft messages."
  (interactive)
  (let ((mu4e~hdrs-buffer (get-buffer-create mu4e~hdrs-buffer-name))
	 (compose-type
	   (or compose-type
	     (intern (ido-completing-read "Compose type: "
		       '("reply" "forward" "edit" "new"))))))
    (with-current-buffer mu4e~hdrs-buffer
      ;; 'new is special, since it takes no existing message as arg therefore,
      ;; we don't need to call thec backend, and call the handler *directly*
      (if (eq compose-type 'new)
	(mu4e~compose-handler 'new)
	;; otherwise, we need the doc-id
	(let ((docid (mu4e~docid-at-point)))
	  (unless docid (error "No message at point."))
	  ;; note, the first two chars of the line (the mark margin) does *not*
	  ;; have the 'draft property; thus, we check one char before the end of
	  ;; the current line instead
	  (unless (or (not (eq compose-type 'edit))
		    (get-text-property (- (line-end-position) 1) 'draft))
	    (error "Editing is only allowed for draft messages"))
	  ;; if there's a visible view window, select that before starting
	  ;; composing a new message, so that one will be replaced by the
	  ;; compose window. The 10-or-so line headers buffer is not a good way
	  ;; to write it...
	  (let ((viewwin (get-buffer-window mu4e-view-buffer)))
	    (when (window-live-p viewwin)
	      (select-window viewwin)))
 
	  ;; talk to the backend
	  (mu4e~proc-compose compose-type docid))))))

(defun mu4e-compose-reply ()
  "Reply to the current message."
  (interactive) (mu4e-compose 'reply))

(defun mu4e-compose-forward ()
  "Forward the current message."
  (interactive) (mu4e-compose 'forward))

(defun mu4e-compose-edit ()
  "Edit the draft message."
  (interactive) (mu4e-compose 'edit))

(defun mu4e-compose-new ()
  "Compose a new message."
  (interactive) (mu4e-compose 'new))

(defun mu4e-hdrs-action ()
  "Ask user what to do with message-at-point, then do it. The
actions are specified in `mu4e-headers-actions'."
  (interactive)
  (let ((msg (mu4e-message-at-point t))
	 (actionfunc (mu4e-choose-action "Action: " mu4e-headers-actions)))
    (funcall actionfunc msg)))

(defun mu4e-hdrs-mark-and-next (mark)
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
  (interactive)
  (mu4e-mark-set mark)
  (mu4e-next-header))

(defun mu4e-hdrs-mark-for-move-and-next ()
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
  (interactive)
  (mu4e-mark-for-move-set)
  (mu4e-next-header))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mu4e-hdrs)
