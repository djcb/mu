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

;;; Code:

(eval-when-compile (require 'cl))

(require 'mm-common)
(require 'mm-proc)

(defvar mm/header-fields
  '( (:date          .  25)
     (:from-or-to    .  22)
     (:subject       .  40))
  "A list of header fields and their character widths.")


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
      (setq mm/msg-map nil mm/mm/marks-map nil)
      (mm/msg-map-init)
      (setq
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
	      (marker (mm/msg-map-get-marker docid)))
	(unless docid (error "Invalid update %S" update))
	(unless marker (error "Message %d not found" docid))
	      (save-excursion
		(goto-char (marker-position marker))
	;; sanity check
		(unless (eq docid (get-text-property (point) 'docid))
	  (error "Unexpected docid"))
		;; if it's marked, unmark it now
		(when (mm/hdrs-docid-is-marked docid)
	  (mm/hdrs-mark 'unmark))
		(let ((inhibit-read-only t) (bol (line-beginning-position))
		       (eol (line-beginning-position 2)))
		  ;; hide the old line (removing it causes some problems)
		  (put-text-property bol eol 'invisible t)
		  ;; now, if this update was about *moving* a message, we don't show it
		  ;; anymore (of course, we cannot be sure if the message really no
		  ;; longer matches the query, but this seem a good heuristic.
		  ;; if it was only a flag-change, show the message with its updated flags.
		  (unless is-move
		    (mm/hdrs-header-handler msg bol))))))))

(defun mm/hdrs-remove-handler (docid)
  "Remove handler, will be called when a message has been removed
from the database. This function will hide the remove message in
the current list of headers."
  (with-current-buffer mm/hdrs-buffer
    (let ((marker (mm/msg-map-get-marker docid)))
      (unless marker (error "Message %d not found" docid))
      (save-excursion
	(goto-char (marker-position marker))
	;; sanity check
	(unless (eq docid (get-text-property (point) 'docid))
	  (error "Unexpected docid"))
	;; if it's marked, unmark it now
	(when (mm/hdrs-docid-is-marked docid)
	  (mm/hdrs-mark 'unmark))
	(let ((inhibit-read-only t) (bol (line-beginning-position))
	       (eol (line-beginning-position 2)))
	  ;; hide the message
	  (set-text-properties bol eol '(invisible t)))))))

(defun mm/hdrs-header-handler (msg &optional point)
  "Function to add a line for a message. This will be called by
`mm/proc-find'. Function expects to be in the output buffer
already. Normally, msg is appended to the end of the buffer, but if
POINT is given, message is insert at POINT."
  (let* ((docid (plist-get msg :docid))
	  (line (propertize (concat "  " (mm/hdrs-line msg) "\n")
		  'docid docid)))
    ;; add message to the docid=>path map, see `mm/msg-map'.
    (with-current-buffer mm/hdrs-buffer
      (save-excursion
	;; append to end, or insert at POINT if that was provided
	(goto-char (if point point (point-max)))
	(mm/msg-map-add msg (point-marker))
	(let ((inhibit-read-only t))
	  (insert line))))))

(defun mm/hdrs-line (msg)
  "Get the one-line description of MSG (as per `mm/hdrs-raw-line'), and
apply text-properties based on the message flags."
  (let ((line (mm/hdrs-raw-line msg)) (flags (plist-get msg :flags)))
    (cond
      ((member 'trashed flags) (propertize line 'face 'mm/trashed-face))
      ((member 'unread flags)  (propertize line 'face 'mm/unread-face))
      (t                       (propertize line 'face 'mm/header-face)))))

(defun mm/hdrs-raw-line (msg)
  "Create a one line description of MSG in this buffer at
point. Line does not include a newline or any text-properties."
  (mapconcat
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
		  (:date  (format-time-string "%x %X" val))
		  (:flags (mm/flags-to-string val))
		  (:size
		    (cond
		      ((>= val 1000000) (format "%2.1fM" (/ val 1000000.0)))
		      ((and (>= val 1000) (< val 1000000))
			(format "%2.1fK" (/ val 1000.0)))
		      ((< val 1000) (format "%d" val))))
		  (t (error "Unsupported header field (%S)" field)))))
	(when str (truncate-string-to-width str width 0 ?\s t))))
    mm/header-fields " "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;; hdrs-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm/hdrs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "s" 'mm/search)
    (define-key map "q" 'mm/quit-buffer)
    (define-key map "o" 'mm/change-sort)
    (define-key map "g" 'mm/rerun-search)

    ;; navigation
    (define-key map "n" 'mm/next-header)
    (define-key map "p" 'mm/prev-header)
    (define-key map "j" 'mm/jump-to-maildir)

    ;; marking/unmarking/executing
    (define-key map "m" 'mm/mark-for-move)
    (define-key map "d" 'mm/mark-for-trash)
    (define-key map "D" 'mm/mark-for-delete)
    (define-key map "u" 'mm/unmark)
    (define-key map "U" 'mm/unmark-all)
    (define-key map "x" 'mm/execute-marks)

    ;; message composition
    (define-key map "r" 'mm/compose-reply)
    (define-key map "f" 'mm/compose-forward)
    (define-key map "c" 'mm/compose-new)

    (define-key map (kbd "RET") 'mm/view-message)
    map)
  "Keymap for *mm-headers* buffers.")
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
    overwrite-mode 'overwrite-mode-binary))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; the message map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm/msg-map nil
  "*internal* A map (hashtable) which maps a database (Xapian)
docid (which uniquely identifies a message to a marker.  where
marker points to the buffer position for the message.

Using this map, we can update message headers which are currently
on the screen, when we receive (:update ) notices from the mu
server.")

(defun mm/msg-map-add (msg marker)
  "Update `mm/msg-map' with MSG, and MARKER pointing to the buffer
  position for the message header."
  (let ((docid (plist-get msg :docid)))
    (unless docid (error "Invalid message"))
    (puthash docid marker mm/msg-map)))

(defun mm/msg-map-get-marker (docid)
  "Get the marker for the message identified by DOCID."
  (gethash docid mm/msg-map))

(defun mm/msg-map-init()
  "(Re)initialize the msg map for use -- re-create the hash table,
and reset the last-uid to 0."
  (setq mm/msg-map
    (make-hash-table :size 256 :rehash-size 2 :weakness nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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

(defun mm/hdrs-mark (mark &optional target)
  "Mark (or unmark) header line at point. MARK specifies the
  mark-type. For `move'-marks there is also the TARGET argument,
  which specifies to which maildir the message is to be moved.

The following marks are available, and the corresponding props:

   MARK       TARGET    description
   ----------------------------------------------------------
   `move'     y         move the message to some folder
   `trash'    n         move the message to `mm/trash-folder'
   `delete'   n         remove the message
   `unmark'   n         unmark this message"
  (let* ((docid (get-text-property (point) 'docid))
	  (markkar
	    (case mark     ;; the visual mark
	      ('move    "m")
	      ('trash   "d")
	      ('delete  "D")
	      ('unmark  " ")
	      (t (error "Invalid mark %S" mark)))))
    (unless docid (error "No message on this line"))
    (save-excursion
      (move-beginning-of-line 1)

      ;; is there anything to mark/unmark?
      (when (and (looking-at "  ") (eql mark 'unmark))
	(error "Not marked"))
      (when (not (or (looking-at "  ") (eql mark 'unmark)))
	(error "Already marked"))

      ;; update the hash
      (if (eql mark 'unmark)
	(remhash docid mm/marks-map)
	(puthash docid (list (point-marker) mark target) mm/marks-map))

      ;; now, update the visual mark..;
      (let ((inhibit-read-only t))
	(delete-char 2)
	(insert (propertize (concat markkar " ") 'docid docid))))))

(defun mm/hdrs-marks-execute ()
  "Execute the actions for all marked messages in this
buffer.

After the actions have been executed succesfully, the affected
messages are *hidden* from the current header list. Since the
headers are the result of a search, we cannot be certain that the
messages no longer matches the current one - to get that certainty,
we need to rerun the search, but we don't want to do that
automatically, as it may be too slow and/or break the users
flow. Therefore, we hide the message, which in practice seems to
work well."
  (if (= 0 (hash-table-count mm/marks-map))
    (message "Nothing is marked")
    (maphash
      (lambda (docid val)
	(let*
	  ((marker (nth 0 val)) (mark (nth 1 val)) (target (nth 2 val))
	    (ok (case mark
		  (move
		    (mm/proc-move-msg docid target))
		  (trash
		    (unless mm/trash-folder "`mm/trash-folder' not set")
		    (mm/proc-move-msg docid mm/trash-folder "+T"))
		  (delete
		    (mm/proc-remove-msg docid)))))))
      mm/marks-map)) )

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
  (let ((docid (get-text-property (point) 'docid)))
    (unless docid (error "No message at point."))
    (mm/proc-view-msg docid)))

(defun mm/hdrs-compose (reply-or-forward)
  "Compose either a reply or a forward based on the message at
point."
  (let ((docid (get-text-property (point) 'docid)))
    (unless docid (error "No message at point."))
    (mm/proc-compose-msg docid reply-or-forward)))


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

;; TODO implement
(defun mm/change-sort ()
  "Change the sorting field and/or direction."
  (interactive)
  )

;; TODO warn if marks exist
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
  (if (= 0 (forward-line 1))
    (let ((docid (get-text-property (point) 'docid)))
      (if docid
	docid
	(mm/next-header))) ;; skip non-headers
    (progn (message "No next message available") nil)))


(defun mm/prev-header ()
  "Move point to the previous message header. If this succeeds,
return the new docid. Otherwise, return nil."
  (interactive)
  (if (= 0 (forward-line -1))
    (let ((docid (get-text-property (point) 'docid)))
      (if docid
	docid
	(mm/prev-header))) ;; skip non-headers
    (progn (message "No previous message available") nil)))


(defun mm/jump-to-maildir ()
  "Show the messages in one of the standard folders."
  (interactive)
  (let ((fld (mm/ask-maildir "Jump to maildir: ")))
    (mm/hdrs-search (concat "maildir:" fld))))


(defun mm/mark-for-move ()
  "Mark message at point for moving to a maildir."
  (interactive)
  (let* ((target (mm/ask-maildir "Target maildir for move: "))
	  (fulltarget (concat mm/maildir target)))
    (when (or (file-directory-p fulltarget)
	    (and (yes-or-no-p
		   (format "%s does not exist. Create now?" fulltarget))
	      (mm/proc-mkdir fulltarget)))
      (mm/hdrs-mark 'move target)
      (mm/next-header))))

(defun mm/mark-for-trash ()
  "Mark message at point for moving to the trash
folder (`mm/trash-folder')."
  (interactive)
  (unless mm/trash-folder (error "`mm/trash-folder' is not set"))
  (mm/hdrs-mark 'trash)
  (mm/next-header))

(defun mm/mark-for-delete ()
  "Mark message at point for direct deletion."
  (interactive)
  (mm/hdrs-mark 'delete)
  (mm/next-header))

(defun mm/unmark ()
  "Unmark message at point."
  (interactive)
  (mm/hdrs-mark 'unmark)
  (mm/next-header))

(defun mm/unmark-all ()
  "Unmark all messages."
  (interactive)
  (if (= 0 (hash-table-count mm/marks-map))
    (message "Nothing is marked")
    (when (mm/ignore-marks)
      (mm/hdrs-unmark-all))))

(defun mm/execute-marks ()
  "Execute the actions for the marked messages."
  (interactive)
  (if (= 0 (hash-table-count mm/marks-map))
    (message "Nothing is marked")
    (when (y-or-n-p (format "Sure you want to execute marks on %d message(s)?"
		      (hash-table-count mm/marks-map)))
      (mm/hdrs-marks-execute)
      (message nil))))

(defun mm/compose-reply ()
  "Start composing a reply to the current message."
  (interactive)
  (mm/hdrs-compose 'reply))


(defun mm/compose-forward ()
  "Start composing a forward to the current message."
  (interactive)
  (mm/hdrs-compose 'forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mm-hdrs)

