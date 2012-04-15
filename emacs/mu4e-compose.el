;; mu4e-compose.el -- part of mu4e, the mu mail user agent for emacs
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

;; In this file, various functions to compose/send messages, piggybacking on
;; gnus' message mode

;;; Code:
 ;; we use some stuff from gnus..
(require 'cl)
(require 'message)
(require 'mail-parse)
(require 'smtpmail)

(require 'mu4e-utils)
(require 'mu4e-vars)
(require 'mu4e-proc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e--cite-original (msg)
  "Cite the body text of MSG, with a \"On %s, %s wrote:\"
  line (with the %s's replaced with the date of MSG and the name
  or e-mail address of its sender (or 'someone' if nothing
  else)), followed of the quoted body of MSG, constructed by by
  prepending `mu4e-citation-prefix' to each line. If there is
  no body in MSG, return nil."
  (let* ((from (plist-get msg :from))
	  (body (mu4e-body-text msg)))
    (when body
      (concat
	(format "On %s, %s wrote:"
	  (format-time-string "%c" (plist-get msg :date))
	  (if (and from (car from)) ;; a list ((<name> . <email>))
	    (or (caar from) (cdar from) "someone")
	    "someone"))
	"\n\n"
	(replace-regexp-in-string "^"
	  mu4e-citation-prefix body)))))

(defun mu4e--header (hdr val)
  "Return a header line of the form HDR: VAL\n. If VAL is nil,
return nil."
  (when val (format "%s: %s\n" hdr val)))


(defun mu4e--refererences-construct (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of the
existing References (which may be empty) and the message-id. If the
message-id is empty, returns the old References. If both are empty,
return nil."
  (let ((refs (plist-get msg :references))
	 (old-msgid (plist-get msg :message-id)))
    (when old-msgid
      (setq refs (append refs (list old-msgid)))
      (mapconcat
	(lambda (msgid) (format "<%s>" msgid))
	refs ","))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine the recipient fields for new messages

(defun mu4e--recipients-list-to-string (lst)
  "Convert a lst LST of address cells into a string with a list of
e-mail addresses. If LST is nil, returns nil."
  (when lst
    (mapconcat
      (lambda (addrcell)
	(let ((name (car addrcell))
	       (email (cdr addrcell)))
	  (if name
	    (format "\"%s\" <%s>" name email)
	    (format "%s" email))))
      lst ", ")))

(defun mu4e--address-cell-equal (cell1 cell2)
  "Return t if cell1 and cell2 have the same e-mail
  address (case-insensitively), nil otherwise. cell1 and cell2 are
  cons cells (NAME . EMAIL)."
  (string=
    (downcase (or (cdr cell1) ""))
    (downcase (or (cdr cell2) ""))))



(defun mu4e--create-to-lst (origmsg)
  "Create a list of address for the To: in a new message, based on
the original message ORIGMSG. If the Reply-To address is set, use
that, otherwise use the From address. Note, whatever was in the To:
field before, goes to the Cc:-list (if we're doing a reply-to-all)."
  (let* ((reply-to (plist-get origmsg :reply-to))
 	  (to-lst
	    (or reply-to
	      (delete-duplicates
		(plist-get origmsg :from)
		:test #'mu4e--address-cell-equal))))
    to-lst))

(defun mu4e--create-cc-lst (origmsg reply-all)
  "Create a list of address for the Cc: in a new message, based on
the original message ORIGMSG, and whether it's a reply-all."
  (when reply-all
    (let* ((cc-lst ;; get the cc-field from the original, remove dups
	      (delete-duplicates
		(append
		  (plist-get origmsg :to)
		  (plist-get origmsg :cc))
		:test #'mu4e--address-cell-equal))
	    ;; now we have the basic list, but we must remove
	    ;; addresses also in the to list
	    (cc-lst
	      (delete-if
		(lambda (cc-cell)
		  (find-if
		    (lambda (to-cell)
		      (mu4e--address-cell-equal cc-cell to-cell))
		    (mu4e--create-to-lst origmsg)))
		cc-lst))
	    ;; finally, we need to remove ourselves from the cc-list
	    (cc-lst
	      (if (null user-mail-address)
		cc-lst
		(delete-if
		  (lambda (cc-cell)
		    (mu4e--address-cell-equal cc-cell
		      (cons nil user-mail-address)))
		  cc-lst))))
      cc-lst)))

 (defun mu4e--recipients-construct (field origmsg &optional reply-all)
   "Create value (a string) for the recipient field FIELD (a
symbol, :to or :cc), based on the original message ORIGMSG,
and (optionally) REPLY-ALL which indicates this is a reply-to-all
message. Return nil if there are no recipients for the particular field."
   (mu4e--recipients-list-to-string
     (case field
       (:to
	 (mu4e--create-to-lst origmsg))
       (:cc
	 (mu4e--create-cc-lst origmsg reply-all))
       (otherwise
	 (error "Unsupported field")))))


(defun mu4e--from-construct ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mu4e--insert-mail-header-separator ()
  "Insert `mail-header-separator' in the first empty line of the
message. message-mode needs this line to know where the headers end
and the body starts. Note, in `mu4e-edit-mode, we use
`before-save-hook' and `after-save-hook' to ensure that this
separator is never written to file. Also see
`mu4e-remove-mail-header-separator'."
  (save-excursion
    (goto-char (point-min))
      ;; search for the first empty line
    (if (search-forward-regexp (concat "^$"))
      (replace-match
	(propertize mail-header-separator 'read-only t 'intangible t))
      ;; no empty line? then append one
      (progn
	(goto-char (point-max))
	(insert (concat "\n" mail-header-separator "\n"))))))

(defun mu4e--remove-mail-header-separator ()
  "Remove `mail-header-separator; we do this before saving a
file (and restore it afterwardds), to ensure that the separator
never hits the disk. Also see `mu4e--insert-mail-header-separator."
  (save-excursion
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (when (search-forward-regexp (concat "^" mail-header-separator))
      (replace-match ""))))

(defun mu4e--user-wants-reply-all (origmsg)
  "Ask user whether she wants to reply to *all* recipients if there
are more than 1 (based on ORIGMSG)."
  (let* ((recipnum
	   (+ (length (mu4e--create-to-lst origmsg))
	      (length (mu4e--create-cc-lst origmsg t))))
	  (response
	    (if (= recipnum 1)
	       ?a ;; with one recipient, we can reply to 'all'....
	       (mu4e-read-option
		 "Reply to "
		 `( (,(format "all %d recipients" recipnum))
		    ("sender only"))))))
    (= response ?a)))


(defun mu4e--reply-construct (origmsg)
  "Create a draft message as a reply to original message ORIGMSG."
  (let* ((recipnum
	   (+ (length (mu4e--create-to-lst origmsg))
	      (length (mu4e--create-cc-lst origmsg t))))
	  (reply-all (mu4e--user-wants-reply-all origmsg))
	  (old-msgid (plist-get origmsg :message-id))
	  (subject (or (plist-get origmsg :subject) "")))
    (concat

      (mu4e--header "From" (or (mu4e--from-construct) ""))
      (mu4e--header "Reply-To" mu4e-reply-to-address)
      (mu4e--header "To" (mu4e--recipients-construct :to origmsg))
      (mu4e--header "Cc" (mu4e--recipients-construct :cc origmsg
			       reply-all))
      (mu4e--header "User-agent"  (mu4e-user-agent))
      (mu4e--header "References"  (mu4e--refererences-construct origmsg))

      (when old-msgid
	(mu4e--header "In-reply-to" (format "<%s>" old-msgid)))

      (mu4e--header "Subject"
	(concat
	  ;; if there's no Re: yet, prepend it
	  (if (string-match (concat "^" mu4e-reply-prefix) subject)
	    "" mu4e-reply-prefix)
	  subject))
      "\n\n"
      (mu4e--cite-original origmsg))))


(defun mu4e--forward-construct (origmsg)
  "Create a draft forward message for original message ORIGMSG."

  (let ((subject
	  (or (plist-get origmsg :subject) "")))
    (concat
      (mu4e--header "From" (or (mu4e--from-construct) ""))
      (mu4e--header "Reply-To" mu4e-reply-to-address)
      (mu4e--header "To" "")
      (mu4e--header "User-agent"  (mu4e-user-agent))
      (mu4e--header "References"  (mu4e--refererences-construct origmsg))
      (mu4e--header "Subject"
	(concat
	  ;; if there's no Re: yet, prepend it
	  (if (string-match (concat "^" mu4e-forward-prefix) subject)
	    "" mu4e-forward-prefix)
	  subject))
      "\n\n"
      (mu4e--cite-original origmsg))))


(defun mu4e--newmsg-construct ()
  "Create a new message."
  (concat
    (mu4e--header "From" (or (mu4e--from-construct) ""))
    (mu4e--header "Reply-To" mu4e-reply-to-address)
    (mu4e--header "To" "")
    (mu4e--header "User-agent"  (mu4e-user-agent))
    (mu4e--header "Subject" "")
     "\n"))


(defun mu4e--open-new-draft-file (compose-type &optional msg)
  "Open a draft file for a new message, creating it if it does not
already exist, and optionally fill it with STR. Function also adds
the new message to the database. When the draft message is added to
the database, `mu4e-path-docid-map' will be updated, so that we can
use the new docid. Returns the full path to the new message."
  (let* ((hostname
	   (downcase
	     (save-match-data
	       (substring system-name
		 (string-match "^[^.]+" system-name) (match-end 0)))))
	  (draft
	    (concat mu4e-maildir mu4e-drafts-folder "/cur/"
	      ;; 'D': rarely used, but hey, it's available
	      ;; 'S': because we're looking at the draft as we speak
	      (format "%s-%x%x.%s:2,DS"
		(format-time-string "%Y%m%d" (current-time))
		(emacs-pid) (random t) hostname)))
	  (str (case compose-type
		 (reply   (mu4e--reply-construct msg))
		 (forward (mu4e--forward-construct msg))
		 (new     (mu4e--newmsg-construct))
		 (t (error "unsupported compose-type %S" compose-type)))))
    (when str
      (with-current-buffer (find-file-noselect draft)
	(insert str)))
    draft)) ;; return the draft buffer file


(define-derived-mode mu4e-edit-mode message-mode "mu4e:edit"
  "Major mode for the mu4e message composition, derived from `message-mode'.
\\{message-mode-map}."
  (let ((message-hidden-headers
	  `("^References:" "^Face:" "^X-Face:" "^X-Draft-From:"
	     "^User-agent:")))
    (use-local-map mu4e-edit-mode-map)
    (message-hide-headers)
    (make-local-variable 'before-save-hook)
    (make-local-variable 'after-save-hook)
    (make-local-variable 'message-default-charset)
    ;; if the default charset is not set, use UTF-8
    (unless message-default-charset
      (setq message-default-charset 'utf-8))
    ;; hack-hack-hack... just before saving, we remove the
    ;; mail-header-separator; just after saving we restore it; thus, the
    ;; separator should never appear on disk
    (add-hook 'before-save-hook 'mu4e--remove-mail-header-separator)
    (add-hook 'after-save-hook
      (lambda ()
	(mu4e--set-friendly-buffer-name)
	(mu4e--insert-mail-header-separator)
	(set-buffer-modified-p nil)))
    ;; update the db when the file is saved...]
    (add-hook 'after-save-hook
      (lambda()
	(mu4e-proc-add (buffer-file-name) mu4e-drafts-folder))))
    ;; notify the backend that a message has been sent. The backend will respond
    ;; with (:sent ...) sexp, which is handled in `mu4e-compose-handler'.
    (add-hook 'message-sent-hook
      (lambda ()
	(set-buffer-modified-p t)
	(basic-save-buffer)
	(mu4e-proc-sent (buffer-file-name) mu4e-drafts-folder)))
  ;; register the function; this function will be called when the '(:sent...)'
  ;; message is received (see mu4e-proc.el) with parameters docid and path
  (setq mu4e-sent-func 'mu4e-sent-handler)
    ;; set the default directory to the user's home dir; this is probably more
    ;; useful e.g. when finding an attachment file the directory the current
    ;; mail files lives in...
  (setq default-directory (expand-file-name "~/")))


(defconst mu4e--buffer-max-name-length 30
  "Maximum length of the mu4e-send-buffer-name.")

(defun mu4e--set-friendly-buffer-name (&optional compose-type)
  "Set some user-friendly buffer name based on the compose type."
  (let* ((subj (message-field-value "subject"))
	  (subj (unless (and subj (string-match "^[:blank:]*$" subj)) subj))
	  (str (or subj
		 (case compose-type
		   (reply       "*reply*")
		   (forward     "*forward*")
		   (otherwise   "*draft*")))))
    (rename-buffer (generate-new-buffer-name
		     (truncate-string-to-width str
		       mu4e--buffer-max-name-length
		       nil nil t)))))


(defun mu4e-compose-handler (compose-type &optional original-msg includes)
    "Create a new draft message, or open an existing one.

COMPOSE-TYPE determines the kind of message to compose and is a
symbol, either `reply', `forward', `edit', `new'. `edit' is for
editing existing messages.

When COMPOSE-TYPE is `reply' or `forward', MSG should be a message
plist.  If COMPOSE-TYPE is `new', MSG should be nil.

Optionally (when forwarding, replying) ORIGINAL-MSG is the original
message we will forward / reply to.

Optionally (when forwarding) INCLUDES contains a list of
   (:file-name <filename> :mime-type <mime-type> :disposition <disposition>)
for the attachements to include; file-name refers to
a file which our backend has conveniently saved for us (as a
tempfile).

The name of the draft folder is constructed from the concatenation
 of `mu4e-maildir' and `mu4e-drafts-folder' (therefore, these must be
 set).

The message file name is a unique name determined by
`mu4e-send-draft-file-name'.

The initial STR would be created from either `mu4e--reply-construct',
ar`mu4e--forward-construct' or `mu4e--newmsg-construct'. The editing buffer is
using Gnus' `message-mode'."
    (unless mu4e-maildir       (error "mu4e-maildir not set"))
    (unless mu4e-drafts-folder (error "mu4e-drafts-folder not set"))
    (let ((draft
	    (if (member compose-type '(reply forward new))
	      (mu4e--open-new-draft-file compose-type original-msg)
	      (if (eq compose-type 'edit)
		(plist-get original-msg :path)
		(error "unsupported compose-type %S" compose-type)))))
      (find-file draft)
      (mu4e-edit-mode)
      ;; insert mail-header-separator, which is needed by message mode to separate
      ;; headers and body. will be removed before saving to disk
      (mu4e--insert-mail-header-separator)
      ;; include files -- e.g. when forwarding a message with attachments,
      ;; we take those from the original.
      (save-excursion
	(goto-char (point-max)) ;; put attachments at the end
	(dolist (att includes)
	  (mml-attach-file
	    (plist-get att :file-name) (plist-get att :mime-type))))

      ;; include the message header if it's set; but not when editing an existing
      ;; message
      (unless (eq compose-type 'edit)
	(when message-signature
	  (message-insert-signature)))

      (if (member compose-type '(new forward))
	(message-goto-to)
	(message-goto-body))
      (mu4e--set-friendly-buffer-name compose-type)
      ;; buffer is not user-modified yet
      (set-buffer-modified-p nil)))

(defun mu4e-insert-captured-message ()
  "Insert the last captured message file."
  (interactive)
  (unless mu4e-captured-message
    (error "No message has been captured"))
  (let ((path (plist-get mu4e-captured-message :path)))
    (unless (file-exists-p path)
      (error "Captured message file not found"))
    (mml-attach-file
      path
      "message/rfc822"
      (or (plist-get mu4e-captured-message :subject) "No subject")
      "attachment")))

(defun mu4e-sent-handler (docid path)
  "Handler function, called with DOCID and PATH for the just-sent
message."
  (with-current-buffer(find-file-noselect path)
    ;; for Forward ('Passed') and Replied messages, try to set the appropriate
    ;; flag at the message forwarded or replied-to
    (mu4e--set-parent-flag docid path)
    ;; handle the draft -- should it be moved to the sent-folder, or elsewhere?
    (mu4e--save-copy-maybe docid path)
    ;; now, get rid of the buffer
    (kill-buffer)))

(defun mu4e--save-copy-maybe (docid path)
  "Handler function, called with DOCID and PATH for the just-sent
message, which will move it to the sent-folder or elsewhere,
depending on the value of `mu4e-sent-messages-behavior'.

Function assumes that it's executed in the context of the message
buffer."
  ;; first, what to do with the draft message in PATH?
  (if (eq mu4e-sent-messages-behavior 'delete)
    (mu4e-proc-remove docid) ;; remove it
    ;; otherwise,
    (progn ;; prepare the message for saving
      (basic-save-buffer)
      ;; now either move it to trash or to sent
      (if (eq mu4e-sent-messages-behavior 'trash)
	(mu4e-proc-move docid mu4e-trash-folder "+T-D+S")
	(mu4e-proc-move docid mu4e-sent-folder  "-T-D+S")))))

(defun mu4e--set-parent-flag (docid path)
  "Set the 'replied' \"R\" flag on messages we replied to, and the
'passed' \"F\" flag on message we have forwarded.

If a message has a 'in-reply-to' header, it is considered a reply
to the message with the corresponding message id. If it does not
have an 'in-reply-to' header, but does have a 'references' header,
it is considered to be a forward message for the message
corresponding with the /last/ message-id in the references header.

Now, if the message has been determined to be either a forwarded
message or a reply, we instruct the server to update that message
with resp. the 'P' (passed) flag for a forwarded message, or the
'R' flag for a replied message.

Function assumes that it's executed in the context of the message
buffer.
"
  (let ((in-reply-to (message-fetch-field "in-reply-to"))
	 (forwarded-from)
  	 (references (message-fetch-field "references")))
    (unless in-reply-to
      (when references
	(with-temp-buffer ;; inspired by `message-shorten-references'.
	  (insert references)
	  (goto-char (point-min))
	  (let ((refs))
	    (while (re-search-forward "<[^ <]+@[^ <]+>" nil t)
	      (push (match-string 0) refs))
	    ;; the last will be the first
	    (setq forwarded-from (first refs))))))
    ;; remove the <>
    (when (and in-reply-to (string-match "<\\(.*\\)>" in-reply-to))
      (mu4e-proc-move (match-string 1 in-reply-to) nil "+R"))
    (when (and forwarded-from (string-match "<\\(.*\\)>" forwarded-from))
      (mu4e-proc-move (match-string 1 forwarded-from) nil "+P"))))

(provide 'mu4e-compose)
