;; mu4e-hdrs.el -- part of mu4e, the mu mail user agent
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

;; In this file are function related to creating the list of one-line
;; descriptions of emails, aka 'headers' (not to be confused with headers like
;; 'To:' or 'Subject:')

;; Code:

(eval-when-compile (require 'cl))

(require 'mu4e-proc)

;;;; internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-last-expr nil
  "*internal* The most recent search expression.")

(defconst mu4e-hdrs-buffer-name "*mu4e-headers*"
  "*internal* Name of the buffer for message headers.")

(defvar mu4e-hdrs-buffer nil
  "*internal* Buffer for message headers")

(defconst mu4e-hdrs-fringe "  "
  "*internal* The space on the left of message headers to put marks.")

(defun mu4e-hdrs-search (expr &optional full-search)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results. If FULL-SEARCH is non-nil return all
results, otherwise, limit number of results to
`mu4e-search-results-limit'."
  (let ((buf (get-buffer-create mu4e-hdrs-buffer-name))
	  (inhibit-read-only t)
	 (esc (replace-regexp-in-string "\"" "\\\\\"" expr))) ;; escape "\"
    (with-current-buffer buf
      (erase-buffer)
      (mu4e-hdrs-mode)
      (setq
	global-mode-string (propertize expr 'face 'mu4e-title-face)
	mu4e-last-expr expr
	mu4e-hdrs-buffer buf
	mode-name "mu4e-headers"))
    (switch-to-buffer mu4e-hdrs-buffer)
    (mu4e-proc-find esc ;; '-1' means 'unlimited search'
      (if full-search -1 mu4e-search-results-limit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler functions
;;
;; next are a bunch of handler functions; those will be called from mu4e-proc in
;; response to output from the server process


(defun mu4e-hdrs-view-handler (msg)
  "Handler function for displaying a message."
  (mu4e-view msg mu4e-hdrs-buffer))

(defun mu4e-hdrs-error-handler (err)
  "Handler function for showing an error."
  (let ((errcode (plist-get err :error))
	 (errmsg (plist-get err :error-message)))
    (case errcode
      (4 (message "No matches for this search query."))
      (t (message (format "Error %d: %s" errcode errmsg))))))

(defun mu4e-hdrs-update-handler (msg is-move)
  "Update handler, will be called when a message has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p mu4e-hdrs-buffer)
    (with-current-buffer mu4e-hdrs-buffer
      (let* ((docid (plist-get msg :docid))
	      (marker (gethash docid mu4e-msg-map))
	      (point (when marker (marker-position marker))))
	(when point ;; is the message present in this list?
	  ;; if it's marked, unmark it now
	  (when (mu4e-hdrs-docid-is-marked docid) (mu4e-hdrs-mark 'unmark))
	  ;; first, remove the old one (otherwise, we'd have to headers with
	  ;; the same docid...
	  (mu4e-hdrs-remove-handler docid)

	  ;; if we we're actually viewing this message (in mu4e-view mode), we
	  ;; update the `mu4e-current-msg' there as well; that way, the flags can
	  ;; be updated, as well as the path (which is useful for viewing the
	  ;; raw message)
	  (let ((viewbuf (get-buffer mu4e-view-buffer-name)))
	    (when (and viewbuf (buffer-live-p viewbuf))
	      (with-current-buffer viewbuf
		(when (eq docid (plist-get mu4e-current-msg :docid))
		  (setq mu4e-current-msg msg)))))

	  ;; now, if this update was about *moving* a message, we don't show it
	  ;; anymore (of course, we cannot be sure if the message really no
	  ;; longer matches the query, but this seem a good heuristic.
	  ;; if it was only a flag-change, show the message with its updated flags.
	  (unless is-move
	    (mu4e-hdrs-header-handler msg point)))))))


(defun mu4e-hdrs-remove-handler (docid)
  "Remove handler, will be called when a message has been removed
from the database. This function will hide the remove message in
the current list of headers."
  (with-current-buffer mu4e-hdrs-buffer
    (let* ((marker (gethash docid mu4e-msg-map))
	    (pos (and marker (marker-position marker)))
	    (docid-at-pos (and pos (mu4e-hdrs-get-docid pos))))
      (unless marker (error "Message %d not found" docid))
      (unless (eq docid docid-at-pos)
	(error "At point %d, expected docid %d, but got %d"
	  pos docid docid-at-pos))
      (mu4e-hdrs-remove-header docid pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-hdrs-contact-str (contacts)
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
	;; FIXME: when updating an header line, we don't know the thread
	;; stuff

(defun mu4e-hdrs-header-handler (msg &optional point)
  "Create a one line description of MSG in this buffer, at POINT,
if provided, or at the end of the buffer otherwise."
  (let* ( (docid (plist-get msg :docid))
	  (thread-info
	    (or (plist-get msg :thread) (gethash docid mu4e-thread-info-map)))
	  (line
	   (mapconcat
	     (lambda (f-w)
	       (let* ((field (car f-w)) (width (cdr f-w))
		       (val (plist-get msg field))
		       (str
			 (case field
			   (:subject  (concat (mu4e-thread-prefix thread-info) val))
			   ((:maildir :path) val)
			   ((:to :from :cc :bcc) (mu4e-hdrs-contact-str val))
			   ;; if we (ie. `user-mail-address' is the 'From', show
			   ;; 'To', otherwise show From
			   (:from-or-to
			     (let* ((from-lst (plist-get msg :from))
				     (from (and from-lst (cdar from-lst))))
			       (if (and from (string-match
					       mu4e-user-mail-address-regexp from))
				 (concat "To "
				   (mu4e-hdrs-contact-str (plist-get msg :to)))
				 (mu4e-hdrs-contact-str from-lst))))
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

    ;; store the thread info, so we can use it when updating the message
    (when thread-info
      (puthash docid thread-info mu4e-thread-info-map))
    (mu4e-hdrs-add-header line (plist-get msg :docid)
      (if point point (point-max)))))


(defun mu4e-hdrs-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (with-current-buffer mu4e-hdrs-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (propertize
		  (case count
		    (0 "No matching messages found")
		    ;; note, don't show the number so we don't have to update it
		    ;; when we delete messsages...
		    (otherwise "End of search results"))
		    ;; (1 "Found 1 message")
		    ;; (otherwise (format "Found %d messages" count)))
		  'face 'mu4e-system-face 'intangible t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; hdrs-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-hdrs-mode-map nil)
(defvar mu4e-hdrs-mode-map nil
  "Keymap for *mu4e-headers* buffers.")
(unless mu4e-hdrs-mode-map
  (setq mu4e-hdrs-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "s" 'mu4e-search)

      (define-key map "b" 'mu4e-search-bookmark)

      (define-key map "q" 'mu4e-quit-buffer)

      (define-key map "r" 'mu4e-rerun-search)
      (define-key map "g" 'mu4e-rerun-search) ;; for compatibility

      ;; navigation
      (define-key map "n" 'mu4e-next-header)
      (define-key map "p" 'mu4e-prev-header)


      ;; marking/unmarking/executing
      (define-key map (kbd "<backspace>") 'mu4e-mark-for-trash)
      (define-key map "d" 'mu4e-mark-for-trash)

      (define-key map (kbd "<delete>") 'mu4e-mark-for-delete)
      (define-key map "D" 'mu4e-mark-for-delete)

      (define-key map "o" 'mu4e-mark-as-unread)
      (define-key map "r" 'mu4e-mark-as-read)

      (define-key map "j" 'mu4e-jump-to-maildir)
      (define-key map "m" 'mu4e-mark-for-move)

      (define-key map "u" 'mu4e-unmark)
      (define-key map "U" 'mu4e-unmark-all)
      (define-key map "x" 'mu4e-execute-marks)

      ;; message composition
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-edit-draft)

      (define-key map (kbd "RET") 'mu4e-view-message)
      (define-key map [mouse-2]   'mu4e-view-message)

      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Headers")))
	(define-key map [menu-bar headers] (cons "Headers" menumap))

	(define-key menumap [quit-buffer] '("Quit view" . mu4e-quit-buffer))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))

	(define-key menumap [execute-marks]  '("Execute marks" . mu4e-execute-marks))
	(define-key menumap [unmark-all]  '("Unmark all" . mu4e-unmark-all))
	(define-key menumap [unmark]      '("Unmark" . mu4e-unmark))

	(define-key menumap [mark-as-read]  '("Mark as read"   . mu4e-mark-as-read))
	(define-key menumap [mark-as-unread]   '("Mark as unread" .  mu4e-mark-as-unread))

	(define-key menumap [mark-delete]  '("Mark for deletion" . mu4e-mark-for-delete))
	(define-key menumap [mark-trash]   '("Mark for trash" .  mu4e-mark-for-trash))
	(define-key menumap [mark-move]  '("Mark for move" . mu4e-mark-for-move))
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

	;;(define-key menumap [draft]  '("Edit draft" . mu4e-compose-new))
      map)))

(fset 'mu4e-hdrs-mode-map mu4e-hdrs-mode-map)

  ;; we register our handler functions for the mu4e-proc (mu server) output
(setq mu4e-proc-error-func   'mu4e-hdrs-error-handler)
(setq mu4e-proc-update-func  'mu4e-hdrs-update-handler)
(setq mu4e-proc-header-func  'mu4e-hdrs-header-handler)
(setq mu4e-proc-found-func   'mu4e-hdrs-found-handler)
(setq mu4e-proc-view-func    'mu4e-hdrs-view-handler)
(setq mu4e-proc-remove-func  'mu4e-hdrs-remove-handler)
;; this last one is defined in mu4e-send.el
(setq mu4e-proc-compose-func 'mu4e-send-compose-handler)


(defun mu4e-hdrs-mode ()
  "Major mode for displaying mu4e search results.

\\{mu4e-hdrs-mode-map}."
  (interactive)

  (kill-all-local-variables)
  (use-local-map mu4e-hdrs-mode-map)

  (make-local-variable 'mu4e-last-expr)
  (make-local-variable 'mu4e-hdrs-proc)
  (make-local-variable 'mu4e-marks-map)
  (make-local-variable 'mu4e-msg-map)
  (make-local-variable 'mu4e-thread-info-map)
  (make-local-variable 'global-mode-string)

  (setq
    mu4e-marks-map (make-hash-table :size 16 :rehash-size 2)
    mu4e-msg-map (make-hash-table :size 1024 :rehash-size 2 :weakness nil)
    mu4e-thread-info-map (make-hash-table :size 512  :rehash-size 2)
    major-mode 'mu4e-hdrs-mode
    mode-name "mu4e: message headers"
    truncate-lines t
    buffer-undo-list t ;; don't record undo information
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary)

   (setq header-line-format
     (cons
       (make-string
	 (+ (length mu4e-hdrs-fringe) (floor (fringe-columns 'left t))) ?\s)
       (map 'list
	 (lambda (item)
	   (let ((field (cdr (assoc (car item) mu4e-header-names)))
		  (width (cdr item)))
	     (concat
	       (propertize
		 (if width
		   (truncate-string-to-width field width 0 ?\s t)
		   field)
		 'face 'mu4e-title-face) " ")))
	 mu4e-headers-fields))))

(put 'mu4e-hdrs-mode 'mode-class 'special)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-msg-map nil
  "*internal* A map (hashtable) which maps a database (Xapian)
docid (which uniquely identifies a message to a marker.  where
marker points to the buffer position for the message.

Using this map, we can update message headers which are currently
on the screen, when we receive (:update ) notices from the mu
server.")

(defun mu4e-hdrs-add-header (str docid point)
  "Add header STR with DOCID to the buffer at POINT."
  (unless docid (error "Invalid message"))
  (when (buffer-live-p mu4e-hdrs-buffer)
    (with-current-buffer mu4e-hdrs-buffer
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char point)
	  ;; Update `mu4e-msg-map' with MSG, and MARKER pointing to the buffer
	  ;; position for the message header."
	  (insert (propertize (concat mu4e-hdrs-fringe str "\n")  'docid docid))
	  ;; note: this maintaining the hash with the markers makes things slow
	  ;; when there are many (say > 1000) headers. this seems to be mostly
	  ;; in the use of markers. we use those to find messages when they need
	  ;; to be updated.
	  (puthash docid (copy-marker point t) mu4e-msg-map))))))


(defun mu4e-hdrs-remove-header (docid point)
  "Remove header with DOCID at POINT."
  (with-current-buffer mu4e-hdrs-buffer
    (goto-char point)
    ;; sanity check
    (unless (eq docid (mu4e-hdrs-get-docid))
      (error "%d: Expected %d, but got %d"
	(line-number-at-pos) docid (mu4e-hdrs-get-docid)))
    (let ((inhibit-read-only t))
      ;; (put-text-property (line-beginning-position line-beginning-positio 2)
      ;; 	'invisible t))
      (delete-region (line-beginning-position) (line-beginning-position 2)))
    (remhash docid mu4e-msg-map)))

(defun mu4e-hdrs-mark-header (docid mark)
  "(Visually) mark the header for DOCID with character MARK."
  (let ((marker (gethash docid mu4e-msg-map)))
    ;; (unless marker (error "Unregistered message"))
    (when marker
      (with-current-buffer mu4e-hdrs-buffer
	(save-excursion
	  (let ((inhibit-read-only t) (pos (marker-position marker)))
	    (goto-char pos)
	    (delete-char 2)
	    (insert (propertize mark 'face 'mu4e-hdrs-marks-face) " ")
	    (put-text-property pos
	      (line-beginning-position 2) 'docid docid)
	    ;; update the msg-map, ie., move it back to the start of the line
	    (puthash docid
	      (copy-marker (line-beginning-position) t)
	      mu4e-msg-map)))))))


(defun mu4e-hdrs-get-docid (&optional point)
  "Get the docid for the message at POINT, if provided, or (point), otherwise."
  (with-current-buffer mu4e-hdrs-buffer
    (get-text-property (if point point (point)) 'docid)))

(defun mu4e-dump-msg-map ()
  "*internal* dump the message map (for debugging)."
  (with-current-buffer mu4e-hdrs-buffer
    (message "msg-map (%d)" (hash-table-count mu4e-msg-map))
    (maphash
      (lambda (k v)
	(message "%s => %s" k v))
      mu4e-msg-map)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; threadinfo-map  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-thread-info-map nil
  "Map (hash) of docid->threadinfo; when filling the list of
  messages, we fill a map of thread info, such that when a header
  changes (e.g., it's read-flag gets set) through some (:update
  ...) message, we can restore the thread-info (this is needed
  since :update messages do not include thread info).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; marks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mu4e-marks-map nil
  "Map (hash) of docid->markinfo; when a message is marked, the
information is added here.

markinfo is a list consisting of the following:
\(marker mark target)
where
   MARKER is an emacs-textmarker pointing to the beginning of the header line
   MARK is the type of mark (move, trash, delete)
   TARGET (optional) is the target directory (for 'move')")

(defun mu4e-hdrs-mark-message (mark &optional target)
  "Mark (or unmark) message at point. MARK specifies the
  mark-type. For `move'-marks there is also the TARGET argument,
  which specifies to which maildir the message is to be moved.

The following marks are available, and the corresponding props:

   MARK       TARGET    description
   ----------------------------------------------------------
   `move'     y         move the message to some folder
   `trash'    n         move the message to `mu4e-trash-folder'
   `delete'   n         remove the message
   `read'     n         mark the message as read
   `unread'   n         mark the message as unread
   `unmark'   n         unmark this message"
  (let* ((docid (mu4e-hdrs-get-docid))
	  (markkar
	    (case mark     ;; the visual mark
	      ('move    "m")
	      ('trash   "d")
	      ('delete  "D")
	      ('unread  "U")
	      ('read    "R")
	      ('unmark  " ")
	      (t (error "Invalid mark %S" mark)))))
    (unless docid (error "No message on this line"))
    (save-excursion
      (when (mu4e-hdrs-mark-header docid markkar))
      ;; update the hash -- remove everything current, and if add the new stuff,
      ;; unless we're unmarking
      (remhash docid mu4e-marks-map)
      ;; remove possible overlays
      (remove-overlays (line-beginning-position) (line-end-position))

      ;; now, let's set a mark (unless we were unmarking)
      (unless (eql mark 'unmark)
	(puthash docid (list (point-marker) mark target) mu4e-marks-map)
	;; when we have a target (ie., when moving), show the target folder in
	;; an overlay
	(when target
	  (let* ((targetstr (propertize (concat "-> " target " ")
			      'face 'mu4e-system-face))
		  (start (+ 2 (line-beginning-position))) ;; +2 for the marker fringe
		  (overlay (make-overlay start (+ start (length targetstr)))))
	    (overlay-put overlay 'display targetstr)))))))


(defun mu4e-hdrs-mark (mark &optional target)
  "Mark the header at point, or, if
region is active, mark all headers in the region. Als see
`mu4e-hdrs-mark-message'."
  (with-current-buffer mu4e-hdrs-buffer
    (if (use-region-p)
      ;; mark all messages in the region.
      (save-excursion
	(let ((b (region-beginning)) (e (region-end)))
	  (goto-char b)
	  (while (<= (line-beginning-position) e)
	    (mu4e-hdrs-mark-message mark target)
	    (forward-line 1))))
      ;; just a single message
      (mu4e-hdrs-mark-message mark target))))



(defun mu4e-hdrs-marks-execute ()
  "Execute the actions for all marked messages in this
buffer. After the actions have been executed succesfully, the
affected messages are *hidden* from the current header list. Since
the headers are the result of a search, we cannot be certain that
the messages no longer matches the current one - to get that
certainty, we need to rerun the search, but we don't want to do
that automatically, as it may be too slow and/or break the users
flow. Therefore, we hide the message, which in practice seems to
work well."
  (if (= 0 (hash-table-count mu4e-marks-map))
    (message "Nothing is marked")
    (maphash
      (lambda (docid val)
	(let ((marker (nth 0 val)) (mark (nth 1 val)) (target (nth 2 val)))
	  (case mark
	    (move   (mu4e-proc-move-msg docid target))
	    (read   (mu4e-proc-flag docid "+S-u-N"))
	    (unread (mu4e-proc-flag docid "-S+u"))
	    (trash
	      (unless mu4e-trash-folder
		(error "`mu4e-trash-folder' not set"))
	      (mu4e-proc-move-msg docid mu4e-trash-folder "+T"))
	    (delete (mu4e-proc-remove-msg docid)))))
	  mu4e-marks-map)
    (mu4e-hdrs-unmark-all)))

(defun mu4e-hdrs-unmark-all ()
  "Unmark all marked messages."
  (unless (/= 0 (hash-table-count mu4e-marks-map))
    (error "Nothing is marked"))
  (maphash
    (lambda (docid val)
      (save-excursion
	(goto-char (marker-position (nth 0 val)))
	(mu4e-hdrs-mark 'unmark)))
    mu4e-marks-map))

(defun mu4e-hdrs-view ()
  "View message at point."
  (let ((docid (mu4e-hdrs-get-docid)))
    (unless docid (error "No message at point."))
    (mu4e-proc-view-msg docid)))


(defun mu4e-hdrs-compose (compose-type)
  "Compose either a reply/forward based on the message at point. or
start editing it. COMPOSE-TYPE is either `reply', `forward' or
`edit'."
  (if (eq compose-type 'new)
    (mu4e-send-compose-handler 'new)
    (let ((docid (mu4e-hdrs-get-docid))
	   ;; note, the first two chars of the line (the mark margin) does *not*
	   ;; have the 'draft property; thus, we check one char before the end of
	   ;; the current line instead
	   (is-draft (get-text-property (- (line-end-position) 1) 'draft)))
      (unless docid
	(error "No message at point."))
      (cond
	((member compose-type '(reply forward))
	  (mu4e-proc-compose compose-type docid))
	((eq compose-type 'edit)
	  (unless is-draft
	    (error "Cannot edit a non-draft message"))
	  (mu4e-proc-compose 'edit docid))
	(t (error "invalid compose type %S" compose-type))))))


(defun mu4e-hdrs-docid-is-marked (docid)
  "Is the given docid marked?"
  (when (gethash docid mu4e-marks-map) t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e-ignore-marks ()
  "If there are still marks in the header list, warn the user."
  (if mu4e-marks-map
    (let* ((num (hash-table-count mu4e-marks-map))
	    (unmark (or (= 0 num)
		      (y-or-n-p
			(format "Sure you want to unmark %d message(s)?" num)))))
      (message nil)
      unmark))
  t)

(defun mu4e-search (expr)
  "Start a new mu search. If prefix ARG is nil, limit the number of
results to `mu4e-search-results-limit', otherwise show all. In
other words, use the C-u prefix to get /all/ results, otherwise get
up to `mu4e-search-results-limit' much quicker."
  (interactive "s[mu] search for: ")
  (when (mu4e-ignore-marks)
    (mu4e-hdrs-search expr current-prefix-arg)))

(defun mu4e-search-bookmark ()
  "Search using some bookmarked query. With C-u prefix, show /all/ results, otherwise,
limit to up to `mu4e-search-results-limit'."
  (interactive)
  (let ((query (mu4e-ask-bookmark "Bookmark: ")))
    (when query
      (mu4e-hdrs-search query current-prefix-arg))))


(defun mu4e-quit-buffer ()
  "Quit the current buffer."
  (interactive)
  (when (mu4e-ignore-marks)
    (mu4e-kill-proc) ;; hmmm...
    (kill-buffer)
    (mu4e)))

(defun mu4e-rerun-search ()
  "Rerun the search for the last search expression; if none exists,
do a new search."
  (interactive)
   (when (mu4e-ignore-marks)
    (if mu4e-last-expr
      (mu4e-hdrs-search mu4e-last-expr)
      (mu4e-search))))

(defun mu4e-view-message ()
  "View the message at point."
  (interactive)
  (mu4e-hdrs-view))

(defun mu4e-next-header ()
  "Move point to the next message header. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (when (= 0 (forward-line 1))
      (or (mu4e-hdrs-get-docid) (mu4e-next-header)) ;; skip non-headers
      ;; trick to move point, even if this function is called when this window
      ;; is not visible
      (set-window-point (get-buffer-window mu4e-hdrs-buffer) (point)))))

(defun mu4e-prev-header ()
  "Move point to the previous message header. If this succeeds,
return the new docid. Otherwise, return nil."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (when (= 0 (forward-line -1))
      (or (mu4e-hdrs-get-docid) (mu4e-prev-header)) ;; skip non-headers
      ;; trick to move point, even if this function is called when this window
      ;; is not visible
      (set-window-point (get-buffer-window mu4e-hdrs-buffer) (point)))))


(defun mu4e-jump-to-maildir ()
  "Show the messages in maildir TARGET. If TARGET is not provided,
ask user for it. With C-u prefix, show /all/ results, otherwise,
limit to up to `mu4e-search-results-limit'."
  (interactive)
  (let ((fld (mu4e-ask-maildir "Jump to maildir: ")))
    (when fld
      (mu4e-hdrs-search (concat "\"maildir:" fld "\"")
	current-prefix-arg))))


(defun mu4e-mark-for-move (&optional target)
  "Mark message at point for moving to maildir TARGET. If target is
not provided, function asks for it."
  (interactive)
  (unless (mu4e-hdrs-get-docid)
      (error "No message at point."))
  (with-current-buffer mu4e-hdrs-buffer
    (let* ((target (or target (mu4e-ask-maildir "Move message to: ")))
	    (target (if (string= (substring target 0 1) "/")
		      target
		      (concat "/" target)))
	    (fulltarget (concat mu4e-maildir target)))
      (when (or (file-directory-p fulltarget)
	      (and (yes-or-no-p
		     (format "%s does not exist. Create now?" fulltarget))
		(mu4e-proc-mkdir fulltarget)))
	(mu4e-hdrs-mark 'move target)
	(mu4e-next-header)))))


(defun mu4e-mark (mark)
  "Mark message for MARK (trash, delete, read, unread, unmark)."
  (with-current-buffer mu4e-hdrs-buffer
    (mu4e-hdrs-mark mark)
    (mu4e-next-header)))

(defun mu4e-mark-for-trash ()
  "Mark message at point for moving to the trash
folder (`mu4e-trash-folder')."
  (interactive)
  (mu4e-mark 'trash))

(defun mu4e-mark-for-delete ()
  "Mark message at point for direct deletion."
  (interactive)
  (mu4e-mark 'delete))

(defun mu4e-mark-as-read ()
  "Mark message at point as unread."
  (interactive)
  (mu4e-mark 'read))

(defun mu4e-mark-as-unread ()
  "Mark message at point as read."
  (interactive)
  (mu4e-mark 'unread))

(defun mu4e-unmark ()
  "Unmark message at point."
  (interactive)
  (mu4e-mark 'unmark))

(defun mu4e-unmark-all ()
  "Unmark all messages."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (if (= 0 (hash-table-count mu4e-marks-map))
      (message "Nothing is marked")
      (when (mu4e-ignore-marks)
	(mu4e-hdrs-unmark-all)))))

(defun mu4e-execute-marks ()
  "Execute the actions for the marked messages."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (if (= 0 (hash-table-count mu4e-marks-map))
      (message "Nothing is marked")
      (when (y-or-n-p (format "Sure you want to execute marks on %d message(s)?"
			(hash-table-count mu4e-marks-map)))
	(mu4e-hdrs-marks-execute)
	(message nil)))))

(defun mu4e-compose-reply ()
  "Start composing a reply to the current message."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (mu4e-hdrs-compose 'reply)))

(defun mu4e-compose-forward ()
  "Start composing a forward to the current message."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (mu4e-hdrs-compose 'forward)))

(defun mu4e-compose-new ()
  "Compose a new, empty message."
  (interactive)
  (mu4e-hdrs-compose 'new))

(defun mu4e-edit-draft ()
  "Start editing the existing draft message at point."
  (interactive)
  (with-current-buffer mu4e-hdrs-buffer
    (mu4e-hdrs-compose 'edit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mu4e-hdrs)
