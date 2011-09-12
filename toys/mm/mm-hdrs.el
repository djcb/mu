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

;; mu

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


(defconst mm/hdrs-buffer-name "*headers*"
  "*internal* Name of the buffer for message headers.")

(defvar mm/hdrs-buffer nil
  "*internal* Buffer for message headers")

(defun mm/hdrs-search (expr)
  "Search in the mu database for EXPR, and switch to the output
buffer for the results."
  (interactive "s[mu] search for: ")
  ;; make sure we get a brand new buffer
  (setq mm/hdrs-buffer (mm/new-buffer mm/hdrs-buffer-name))
  (switch-to-buffer mm/hdrs-buffer)
  (mm/hdrs-mode)
  (setq mm/last-expr expr)
  (mm/msg-map-init)
  (let ((inhibit-read-only t)) (erase-buffer)) ;; FIXME -- why is this needed?!

  ;; all set -- now execute the search
  (mm/proc-find expr))

(defun mm/hdrs-message-handler (msg)
  (message "Received message %d (%s)"
    (plist-get msg :docid)
    (plist-get msg :subject)))

(defun mm/hdrs-error-handler (err)
  (message "Error %d: %s"
    (plist-get err :error)
    (plist-get err :error-message)))

(defun mm/hdrs-update-handler (update)
  "Update handler, will be called when we get '(:update ... )' from
the mu server process. This function will update the current list
of headers."
  (message "We received a database update: %S" update)
  (let* ((type (plist-get update :update))  (docid (plist-get update :docid))
	  (marker (mm/msg-map-get-marker docid)))
    (unless docid (error "Invalid update %S" update))
    (unless marker (error "Message %d not found" docid))
    (with-current-buffer mm/hdrs-buffer
      (save-excursion
	(goto-char (marker-position marker))
	;; sanity check
	(unless (eq docid (get-text-property (point) 'docid))
	  (error "Unexpected docid"))
	(mm/hdrs-mark 'unmark)
	(let ((inhibit-read-only t) (bol (line-beginning-position))
	       (eol (line-beginning-position 2)))
	  (case type
	    (remove  (put-text-property bol eol 'invisible t))
	    (move    (put-text-property bol eol 'face 'mm/moved-face))
	    (t       (error "Invalid update %S" update))))))))


(defun mm/hdrs-header-handler (msg)
  "Function to insert a line for a message. This will be called by
`mm/proc-find'. Function expects to be in the output buffer
already."
  (let* ((docid (mm/msg-field msg :docid))
	  (line (propertize (concat "  " (mm/hdrs-line msg) "\n")
		  'docid docid)))
    ;; add message to the docid=>path map, see `mm/msg-map'.
    (with-current-buffer mm/hdrs-buffer
      (save-excursion
	(goto-char (point-max))
	(mm/msg-map-add msg (point-marker))
	(let ((inhibit-read-only t))
	  (insert line))))))

(defun mm/hdrs-line (msg)
  "Get the one-line description of MSG (as per `mm/hdrs-raw-line'), and
apply text-properties based on the message flags."
  (let ((line (mm/hdrs-raw-line msg))
	 (flags (plist-get msg :flags)))
    (cond
      ((member 'unread flags) (propertize line 'face 'mm/unread-face))
      (t                      (propertize line 'face 'mm/header-face)))))

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
    ;; (define-key map "r" 'mua/hdrs-reply)
    ;; (define-key map "f" 'mua/hdrs-forward)
    ;; (define-key map "c" 'mua/hdrs-compose)

    (define-key map (kbd "RET") 'mm/view-message)
    map)
  "Keymap for *mm-headers* buffers.")
(fset 'mm/hdrs-mode-map mm/hdrs-mode-map)

(defun mm/hdrs-mode ()
  "Major mode for displaying mua search results."
  (interactive)

  (kill-all-local-variables)
  (use-local-map mm/hdrs-mode-map)

  (make-local-variable 'mm/buf)
  (make-local-variable 'mm/last-expr)
  (make-local-variable 'mm/hdrs-proc)
  (make-local-variable 'mm/marks-map)
  (make-local-variable 'mm/msg-map)

  ;; we register our handler functions for the mm-proc (mu server) output
  (setq mm/proc-error-func   'mm/hdrs-error-handler)
  (setq mm/proc-update-func  'mm/hdrs-update-handler)
  (setq mm/proc-header-func  'mm/hdrs-header-handler)
  (setq mm/proc-message-func 'mm/hdrs-message-handler)

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
  (unless (/= 0 (hash-table-count mm/marks-map))
    (error "Nothing is marked"))
  (maphash
    (lambda (docid val)
      (let* ((marker (nth 0 val)) (mark (nth 1 val)) (target (nth 2 val))
	     (ok (case mark
		   (move
		     (mm/proc-move-msg docid target))
		   (trash
		     (unless mm/maildir "`mm/maildir' not set")
		     (unless mm/trash-folder "`mm/trash-folder' not set")
		     (mm/proc-move-msg docid (concat mm/maildir "/" mm/trash-folder) "+T"))
		   (delete
		     (mm/proc-remove-msg docid)))))
	;; (when ok
	;;   (save-excursion
	;;     (goto-char (marker-position marker))
	;;     (mm/hdrs-mark 'unmark)
	;;     ;; hide the line
	;;     (let ((inhibit-read-only t))
	;;       (put-text-property (line-beginning-position) (line-beginning-position 2)
	;; 	'invisible t))))))
	))
    mm/marks-map))


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
  "View message at point"
  (let ((docid (get-text-property (point) 'docid)))
    (unless docid (error "No message at point."))
    (mm/proc-view-msg docid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO warn if marks exist
(defun mm/search ()
  "Start a new mu search."
  (interactive)
  (call-interactively 'mm/hdrs-search))

;; TODO warn if marks exist
;; TODO: return to previous buffer
(defun mm/quit-buffer ()
  "Quit the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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
  (if mm/last-expr
    (mm/hdrs-search mm/last-expr)
    (mm/search)))

(defun mm/view-message ()
  "View the message at point."
  (interactive)
  (mm/hdrs-view))

(defun mm/next-header ()
  "Move point to the next header."
  (interactive)
  (when (or (/= 0 (forward-line 1)) (not (get-text-property (point) 'docid)))
    (error "No header after this one")))

(defun mm/prev-header ()
  "Move point to the previous header."
  (interactive)
  (when (or (/= 0 (forward-line -1)) (not (get-text-property (point) 'docid)))
    (error "No header before this one")))

(defun mm/jump-to-maildir ()
  "Show the messages in one of the standard folders."
  (interactive)
  (let ((fld (mm/ask-maildir "Jump to maildir: ")))
    (mm/hdrs-search (concat "maildir:" fld))))

(defun mm/mark-for-move ()
  "Mark message at point for moving to a maildir."
  (interactive)
  (let ((target (mm/ask-maildir "Target maildir for move: ")))
    (when (or (file-directory-p target)
	    (and (yes-or-no-p
		   (format "%s does not exist. Create now?" target))
	      (mm/proc-mkdir target)))
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
  (unless (/= 0 (hash-table-count mm/marks-map))
    (error "Nothing is marked"))
  (when (y-or-n-p (format "Sure you want to unmark %d message(s)?"
		    (hash-table-count mm/marks-map)))
    (mm/hdrs-unmark-all)))

(defun mm/execute-marks ()
  "Execute the actions for the marked messages."
  (interactive)
  (unless (/= 0 (hash-table-count mm/marks-map))
      (error "Nothing is marked"))
  (when (y-or-n-p (format "Sure you want to execute marks on %d message(s)?"
		    (hash-table-count mm/marks-map)))
    (mm/hdrs-marks-execute)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'mm-hdrs)

