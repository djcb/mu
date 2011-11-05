;; mm-hdrs.el -- part of mm, the mu mail user agent
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

;; In this file are function related to creating the list of one-line
;; descriptions of emails, aka 'headers' (not to be confused with headers like
;; 'To:' or 'Subject:')

;; mm

;; Code:

(eval-when-compile (require 'cl))

(require 'mm-proc)

;;;; internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm/last-expr nil
  "*internal* The most recent search expression.")
(defvar mm/sortfield nil
  "*internal* Field to sort headers by")
(defvar mm/sort-descending nil
  "*internal Whether to sort in descending order")


(defconst mm/hdrs-buffer-name "*mm-headers*"
  "*internal* Name of the buffer for message headers.")

(defvar mm/hdrs-buffer nil
  "*internal* Buffer for message headers")

(defun mm/hdrs-search (expr)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results."
  (interactive "s[mu] search for: ")
  (let ((buf (get-buffer-create mm/hdrs-buffer-name))
	  (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (mm/hdrs-mode)
      (setq
	mm/mm/marks-map nil
	mm/msg-map (make-hash-table :size 1024 :rehash-size 2 :weakness nil)
	mode-name expr
	mm/last-expr expr
	mm/hdrs-buffer buf)))
  (switch-to-buffer mm/hdrs-buffer)
  (mm/proc-find expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler functions
;;
;; next are a bunch of handler functions; those will be called from mm-proc in
;; response to output from the server process


(defun mm/hdrs-view-handler (msg)
  "Handler function for displaying a message."
  (mm/view msg mm/hdrs-buffer))

(defun mm/hdrs-error-handler (err)
  "Handler function for showing an error."
  (let ((errcode (plist-get err :error))
	 (errmsg (plist-get err :error-message)))
    (case errcode
      (4 (message "No matches for this search query."))
      (t  (message (format "Error %d: %s" errcode errmsg))))))

(defun mm/hdrs-update-handler (msg is-move)
  "Update handler, will be called when a message has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p mm/hdrs-buffer)
    (with-current-buffer mm/hdrs-buffer
      (let* ((docid (plist-get msg :docid))
	      (marker (gethash docid mm/msg-map))
	      (point (when marker (marker-position marker))))
	(when point ;; is the message present in this list?
	  ;; if it's marked, unmark it now
	  (when (mm/hdrs-docid-is-marked docid) (mm/hdrs-mark 'unmark))
	  ;; first, remove the old one (otherwise, we'd have to headers with
	  ;; the same docid...
	  (mm/hdrs-remove-handler docid)
	  ;; now, if this update was about *moving* a message, we don't show it
	  ;; anymore (of course, we cannot be sure if the message really no
	  ;; longer matches the query, but this seem a good heuristic.
	  ;; if it was only a flag-change, show the message with its updated flags.
	  (unless is-move
	    (mm/hdrs-header-handler msg point)))))))


(defun mm/hdrs-remove-handler (docid)
  "Remove handler, will be called when a message has been removed
from the database. This function will hide the remove message in
the current list of headers."
  (with-current-buffer mm/hdrs-buffer
    (let* ((marker (gethash docid mm/msg-map))
	    (pos (and marker (marker-position marker)))
	    (docid-at-pos (and pos (mm/hdrs-get-docid pos))))
      (unless marker (error "Message %d not found" docid))
      (unless (eq docid docid-at-pos)
	(error "At point %d, expected docid %d, but got %d" pos docid docid-at-pos))
      (mm/hdrs-remove-header docid pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mm/hdrs-header-handler (msg &optional point)
  "Create a one line description of MSG in this buffer, at POINT,
if provided, or at the end of the buffer otherwise."
  (let* ((line (mapconcat
		(lambda (f-w)
		  (let* ((field (car f-w)) (width (cdr f-w))
			  (val (plist-get msg field))
			  (str
			    (case field
			      (:subject val)
			      ((:to :from :cc :bcc)
				(mapconcat
				  (lambda (ct)
				    (let ((name (car ct)) (email (cdr ct)))
				      (or name email "?"))) val ", "))
			      (:date (format-time-string "%x %X" val))
			      (:flags (mm/flags-to-string val))
			      (:size
				(cond
				  ((>= val 1000000) (format "%2.1fM" (/ val 1000000.0)))
				  ((and (>= val 1000) (< val 1000000))
				    (format "%2.1fK" (/ val 1000.0)))
				  ((< val 1000) (format "%d" val))))
			      (t
				(error "Unsupported header field (%S)" field)))))
		    (when str
		      (if (not width)
			str
			(truncate-string-to-width str width 0 ?\s t)))))
		  mm/header-fields " "))
	  (flags (plist-get msg :flags))
	  (line	(cond
		  ((member 'draft flags)
		    (propertize line 'face 'mm/draft-face 'draft t))
		  ((member 'trashed flags)
		    (propertize line 'face 'mm/trashed-face))
		  ((member 'unread flags)
		    (propertize line 'face 'mm/unread-face))
		  (t ;; else
		    (propertize line 'face 'mm/header-face)))))
    (mm/hdrs-add-header line (plist-get msg :docid)
      (if point point (point-max)))))


(defun mm/hdrs-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (with-current-buffer mm/hdrs-buffer
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
		  'face 'mm/system-face 'intangible t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; hdrs-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mm/hdrs-mode-map nil)
(defvar mm/hdrs-mode-map nil
  "Keymap for *mm-headers* buffers.")
(unless mm/hdrs-mode-map
  (setq mm/hdrs-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "s" 'mm/search)
      (define-key map "q" 'mm/quit-buffer)
;;      (define-key map "o" 'mm/change-sort)
      (define-key map "g" 'mm/rerun-search)

      ;; navigation
      (define-key map "n" 'mm/next-header)
      (define-key map "p" 'mm/prev-header)
      (define-key map "j" 'mm/jump-to-maildir)

      ;; marking/unmarking/executing
      (define-key map "m" 'mm/mark-for-move)

      (define-key map (kbd "<backspace>") 'mm/mark-for-trash)
      (define-key map "d" 'mm/mark-for-trash)

      (define-key map (kbd "<delete>") 'mm/mark-for-delete)
      (define-key map "D" 'mm/mark-for-delete)
      (define-key map "a" 'mm/mark-for-move-quick)

      (define-key map "u" 'mm/unmark)
      (define-key map "U" 'mm/unmark-all)
      (define-key map "x" 'mm/execute-marks)

      ;; message composition
      (define-key map "r" 'mm/compose-reply)
      (define-key map "f" 'mm/compose-forward)
      (define-key map "c" 'mm/compose-new)
      (define-key map "e" 'mm/edit-draft)

      (define-key map (kbd "RET") 'mm/view-message)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Headers")))
	(define-key map [menu-bar headers] (cons "Headers" menumap))

	(define-key menumap [quit-buffer] '("Quit" . mm/quit-buffer))
	(define-key menumap [sepa0] '("--"))

	(define-key menumap [execute-marks]  '("Execute marks" . mm/execute-marks))
	(define-key menumap [unmark-all]  '("Unmark all" . mm/unmark-all))
	(define-key menumap [unmark]      '("Unmark" . mm/unmark))
	(define-key menumap [mark-delete]  '("Mark for deletion" . mm/mark-for-delete))
	(define-key menumap [mark-trash]   '("Mark for trash" .  mm/mark-for-trash))
	(define-key menumap [mark-move]  '("Mark for move" . mm/mark-for-move))
	(define-key menumap [sepa1] '("--"))

	(define-key menumap [compose-new]  '("Compose new" . mm/compose-new))
	(define-key menumap [forward]  '("Forward" . mm/compose-forward))
	(define-key menumap [reply]  '("Reply" . mm/compose-reply))
	(define-key menumap [sepa2] '("--"))

	(define-key menumap [refresh]  '("Refresh" . mm/rerun-search))
	(define-key menumap [search]  '("Search" . mm/search))
	(define-key menumap [jump]  '("Jump to maildir" . mm/jump-to-maildir))
	(define-key menumap [sepa3] '("--"))

	(define-key menumap [view]  '("View" . mm/view-message))
	(define-key menumap [next]  '("Next" . mm/next-header))
	(define-key menumap [previous]  '("Previous" . mm/prev-header))
	(define-key menumap [sepa4] '("--")))

	;;(define-key menumap [draft]  '("Edit draft" . mm/compose-new))
      map)))

(fset 'mm/hdrs-mode-map mm/hdrs-mode-map)


(defun mm/hdrs-mode ()
  "Major mode for displaying mua search results."
  (interactive)

  (kill-all-local-variables)
  (use-local-map mm/hdrs-mode-map)

  (make-local-variable 'mm/last-expr)
  (make-local-variable 'mm/hdrs-proc)
  (make-local-variable 'mm/marks-map)
  (make-local-variable 'mm/msg-map)

  ;; we register our handler functions for the mm-proc (mu server) output
  (setq mm/proc-error-func   'mm/hdrs-error-handler)
  (setq mm/proc-update-func  'mm/hdrs-update-handler)
  (setq mm/proc-header-func  'mm/hdrs-header-handler)
  (setq mm/proc-found-func   'mm/hdrs-found-handler)
  (setq mm/proc-view-func    'mm/hdrs-view-handler)
  (setq mm/proc-remove-func  'mm/hdrs-remove-handler)
  ;; this last one is defined in mm-send.el
  (setq mm/proc-compose-func 'mm/send-compose-handler)

  (setq
    mm/marks-map (make-hash-table :size 16  :rehash-size 2)
    major-mode 'mm/hdrs-mode
    mode-name "*mm-headers*"
    truncate-lines t
    buffer-read-only t
    overwrite-mode 'overwrite-mode-binary)

   (setq header-line-format
     (cons "*  "
       (map 'list
	 (lambda (item) ;; FIXME
	   (let ((field (cdr (assoc (car item) mm/header-names)))
		  (width (cdr item)))
	     (concat
	       (propertize
		 (if width
		   (truncate-string-to-width field width 0 ?\s t)
		   field)
		 'face 'mm/title-face) " ")))
	 mm/header-fields))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm/msg-map nil
  "*internal* A map (hashtable) which maps a database (Xapian)
docid (which uniquely identifies a message to a marker.  where
marker points to the buffer position for the message.

Using this map, we can update message headers which are currently
on the screen, when we receive (:update ) notices from the mu
server.")

(defun mm/hdrs-add-header (str docid point)
  "Add header STR with DOCID to the buffer at POINT."
  (unless docid (error "Invalid message"))
  (when (buffer-live-p mm/hdrs-buffer)
    (with-current-buffer mm/hdrs-buffer
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char point)
	  ;; Update `mm/msg-map' with MSG, and MARKER pointing to the buffer
	  ;; position for the message header."
	  (insert (propertize (concat "  " str "\n")  'docid docid))
	  (puthash docid (copy-marker point t) mm/msg-map))))))

(defun mm/hdrs-remove-header (docid point)
  "Remove header with DOCID at POINT."
  (with-current-buffer mm/hdrs-buffer
    (goto-char point)
    ;; sanity check
    (unless (eq docid (mm/hdrs-get-docid))
      (error "%d: Expected %d, but got %d"
	(line-number-at-pos) docid (mm/hdrs-get-docid)))
    (let ((inhibit-read-only t))
      ;; (put-text-property (line-beginning-position line-beginning-positio 2)
      ;; 	'invisible t))
      (delete-region (line-beginning-position) (line-beginning-position 2)))
    (remhash docid mm/msg-map)))

(defun mm/hdrs-mark-header (docid mark)
  "(Visually) mark the header for DOCID with character MARK."
  (let ((marker (gethash docid mm/msg-map)))
    ;; (unless marker (error "Unregistered message"))
    (when marker
      (with-current-buffer mm/hdrs-buffer
	(save-excursion
	  (let ((inhibit-read-only t) (pos (marker-position marker)))
	    (goto-char pos)
	    (delete-char 2)
	    (insert (propertize mark 'face 'mm/hdrs-marks-face) " ")
	    (put-text-property pos
	      (line-beginning-position 2) 'docid docid)
	    ;; update the msg-map, ie., move it back to the start of the line
	    (puthash docid
	      (copy-marker (line-beginning-position) t)
	      mm/msg-map)))))))


(defun mm/hdrs-get-docid (&optional point)
  "Get the docid for the message at POINT, if provided, or (point), otherwise."
  (with-current-buffer mm/hdrs-buffer
    (get-text-property (if point point (point)) 'docid)))

(defun mm/dump-msg-map ()
  "*internal* dump the message map (for debugging)."
  (with-current-buffer mm/hdrs-buffer
    (message "msg-map (%d)" (hash-table-count mm/msg-map))
    (maphash
      (lambda (k v)
	(message "%s => %s" k v))
      mm/msg-map)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; marks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm/marks-map nil
  "Map (hash) of docid->markinfo; when a message is marked, the
information is added here.

markinfo is a list consisting of the following:
\(marker mark target)
where
   MARKER is an emacs-textmarker pointing to the beginning of the header line
   MARK is the type of mark (move, trash, delete)
   TARGET (optional) is the target directory (for 'move')")

(defun mm/hdrs-mark-message (mark &optional target)
  "Mark (or unmark) message at point. MARK specifies the
  mark-type. For `move'-marks there is also the TARGET argument,
  which specifies to which maildir the message is to be moved.

The following marks are available, and the corresponding props:

   MARK       TARGET    description
   ----------------------------------------------------------
   `move'     y         move the message to some folder
   `trash'    n         move the message to `mm/trash-folder'
   `delete'   n         remove the message
   `unmark'   n         unmark this message"
  (let* ((docid (mm/hdrs-get-docid))
	  (markkar
	    (case mark     ;; the visual mark
	      ('move    "m")
	      ('trash   "d")
	      ('delete  "D")
	      ('select  "*")
	      ('unmark  " ")
	      (t (error "Invalid mark %S" mark)))))
    (unless docid (error "No message on this line"))
    (save-excursion
      (when (mm/hdrs-mark-header docid markkar))
      ;; update the hash -- remove everything current, and if add the new stuff,
      ;; unless we're unmarking
      (remhash docid mm/marks-map)
      (unless (eql mark 'unmark)
	(puthash docid (list (point-marker) mark target) mm/marks-map)))))


(defun mm/hdrs-mark (mark &optional target)
  "Mark the header at point, or, if
region is active, mark all headers in the region. Als see
`mm/hdrs-mark-message'."
  (with-current-buffer mm/hdrs-buffer
    (if (use-region-p)
      ;; mark all messages in the region.
      (save-excursion
	(let ((b (region-beginning)) (e (region-end)))
	  (goto-char b)
	  (while (<= (line-beginning-position) e)
	    (mm/hdrs-mark-message mark target)
	    (forward-line 1))))
      ;; just a single message
      (mm/hdrs-mark-message mark target))))



(defun mm/hdrs-marks-execute ()
  "Execute the actions for all marked messages in this
buffer. After the actions have been executed succesfully, the
affected messages are *hidden* from the current header list. Since
the headers are the result of a search, we cannot be certain that
the messages no longer matches the current one - to get that
certainty, we need to rerun the search, but we don't want to do
that automatically, as it may be too slow and/or break the users
flow. Therefore, we hide the message, which in practice seems to
work well."
  (if (= 0 (hash-table-count mm/marks-map))
    (message "Nothing is marked")
    (maphash
      (lambda (docid val)
	(let ((marker (nth 0 val)) (mark (nth 1 val)) (target (nth 2 val)))
	  (case mark
	    (move
	      (mm/proc-move-msg docid target))
	    (trash
	      (unless mm/trash-folder
		(error "`mm/trash-folder' not set"))
	      (mm/proc-move-msg docid mm/trash-folder "+T"))
	    (delete
	      (mm/proc-remove-msg docid)))))
	  mm/marks-map)
    (mm/hdrs-unmark-all)))

(defun mm/hdrs-unmark-all ()
  "Unmark all marked messages."
  (unless (/= 0 (hash-table-count mm/marks-map))
    (error "Nothing is marked"))
  (maphash
    (lambda (docid val)
      (save-excursion
	(goto-char (marker-position (nth 0 val)))
	(mm/hdrs-mark 'unmark)))
    mm/marks-map))

(defun mm/hdrs-view ()
  "View message at point."
  (let ((docid (mm/hdrs-get-docid)))
    (unless docid (error "No message at point."))
    (mm/proc-view-msg docid)))

(defun mm/hdrs-compose (compose-type)
  "Compose either a reply/forward based on the message at point. or
start editing it. COMPOSE-TYPE is either `reply', `forward' or
`edit'."
  (if (eq compose-type 'new)
    (mm/send-compose-handler 'new)
    (let ((docid (mm/hdrs-get-docid))
	   ;; note, the first two chars of the line (the mark margin) does *not*
	   ;; have the 'draft property; thus, we check one char before the end of
	   ;; the current line instead
	   (is-draft (get-text-property (- (line-end-position) 1) 'draft)))
      (unless docid
	(error "No message at point."))
      (cond
	((member compose-type '(reply forward))
	  (mm/proc-compose compose-type docid))
	((eq compose-type 'edit)
	  (unless is-draft
	    (error "Cannot edit a non-draft message"))
	  (mm/proc-compose 'edit docid))
	(t (error "invalid compose type %S" compose-type))))))


(defun mm/hdrs-docid-is-marked (docid)
  "Is the given docid marked?"
  (when (gethash docid mm/marks-map) t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm/ignore-marks ()
  (let*
    ((num
       (hash-table-count mm/marks-map))
      (unmark (or (= 0 num)
		(y-or-n-p
		  (format "Sure you want to unmark %d message(s)?" num)))))
    (message nil)
    unmark))

(defun mm/search ()
  "Start a new mu search."
  (interactive)
  (when (mm/ignore-marks)
    (call-interactively 'mm/hdrs-search)))

(defun mm/quit-buffer ()
  "Quit the current buffer."
  (interactive)
  (when (mm/ignore-marks)
    (mm/kill-proc) ;; hmmm...
    (kill-buffer)
    (mm)))

;;;;  TODO implement
;; (defun mm/change-sort ()
;;   "Change the sorting field and/or direction."
;;   (interactive)
;;  )

(defun mm/rerun-search ()
  "Rerun the search for the last search expression; if none exists,
do a new search."
  (interactive)
   (when (mm/ignore-marks)
    (if mm/last-expr
      (mm/hdrs-search mm/last-expr)
      (mm/search))))

(defun mm/view-message ()
  "View the message at point."
  (interactive)
  (mm/hdrs-view))

(defun mm/next-header ()
  "Move point to the next message header. If this succeeds, return
the new docid. Otherwise, return nil."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (when (= 0 (forward-line 1))
      (or (mm/hdrs-get-docid) (mm/next-header))))) ;; skip non-headers

(defun mm/prev-header ()
  "Move point to the previous message header. If this succeeds,
return the new docid. Otherwise, return nil."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (when (= 0 (forward-line -1))
      (or (mm/hdrs-get-docid) (mm/prev-header))))) ;; skip non-headers


(defun mm/jump-to-maildir ()
  "Show the messages in one of the standard folders."
  (interactive)
  (let ((fld (mm/ask-maildir "Jump to maildir: ")))
    (mm/hdrs-search (concat "maildir:" fld))))


(defun mm/mark-for-move (&optional target)
  "Mark message at point for moving to maildir TARGET. If target is
not provided, function asks for it."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (let* ((target (or target (mm/ask-maildir "Target maildir for move: ")))
	    (fulltarget (concat mm/maildir target)))
      (when (or (file-directory-p fulltarget)
	      (and (yes-or-no-p
		     (format "%s does not exist. Create now?" fulltarget))
		(mm/proc-mkdir fulltarget)))
      (mm/hdrs-mark 'move target)
	(mm/next-header)))))


(defun mm/mark-for-move-quick ()
  "Mark message at point (or all messages in region) for moving to
a folder; see `mm/move-quick-targets'."
  (interactive)
  (unless mm/move-quick-targets
    (error "`mm/move-quick-targets' has not been defined"))
  (let* ((fnames
	   (mapconcat
	     (lambda (item)
	       (concat
		 "["
		 (propertize (make-string 1 (cdr item)) 'face 'mm/view-link-face)
		 "]"
		 (car item)))
	     mm/move-quick-targets ", "))
	  (kar (read-char (concat "Move to: " fnames)))
	  (targetitem
	    (find-if (lambda (item) (= kar (cdr item))) mm/move-quick-targets))
	  (target (and targetitem (car targetitem))))
    ;; if the target is not found, we simply exit
    (when target
      (mm/mark-for-move target))))


(defun mm/mark-for-trash ()
  "Mark message at point for moving to the trash
folder (`mm/trash-folder')."
  (interactive)
    (unless mm/trash-folder
      (error "`mm/trash-folder' is not set"))
    (with-current-buffer mm/hdrs-buffer
      (mm/hdrs-mark 'trash)
      (mm/next-header)))

(defun mm/mark-for-delete ()
  "Mark message at point for direct deletion."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (mm/hdrs-mark 'delete)
    (mm/next-header)))

(defun mm/unmark ()
  "Unmark message at point."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (mm/hdrs-mark 'unmark)
    (mm/next-header)))

(defun mm/unmark-all ()
  "Unmark all messages."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (if (= 0 (hash-table-count mm/marks-map))
      (message "Nothing is marked")
      (when (mm/ignore-marks)
	(mm/hdrs-unmark-all)))))

(defun mm/execute-marks ()
  "Execute the actions for the marked messages."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (if (= 0 (hash-table-count mm/marks-map))
      (message "Nothing is marked")
      (when (y-or-n-p (format "Sure you want to execute marks on %d message(s)?"
			(hash-table-count mm/marks-map)))
	(mm/hdrs-marks-execute)
	(message nil)))))

(defun mm/compose-reply ()
  "Start composing a reply to the current message."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (mm/hdrs-compose 'reply)))

(defun mm/compose-forward ()
  "Start composing a forward to the current message."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (mm/hdrs-compose 'forward)))

(defun mm/compose-new ()
  "Compose a new, empty message."
  (interactive)
  (mm/hdrs-compose 'new))

(defun mm/edit-draft ()
  "Start editing the existing draft message at point."
  (interactive)
  (with-current-buffer mm/hdrs-buffer
    (mm/hdrs-compose 'edit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mm-hdrs)
