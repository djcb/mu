;;; mu4e-view.el -- part of mu4e, the mu mail user agent
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

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:
(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-proc)
(require 'mu4e-compose)
(require 'mu4e-actions)

;; we prefer the improved fill-region
(require 'filladapt nil 'noerror)
(require 'comint)
(require 'browse-url)
(require 'button)
(require 'epa)
(require 'epg)

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

;; the message view
(defgroup mu4e-view nil
  "Settings for the message view."
  :group 'mu4e)

(defcustom mu4e-view-fields
  '(:from :to  :cc :subject :flags :date :maildir :attachments :signature)
  "Header fields to display in the message view buffer. For the
complete list of available headers, see `mu4e-header-info'."
  :type (list 'symbol)
  :group 'mu4e-view)

(defcustom mu4e-view-show-addresses nil
  "Whether to initially show full e-mail addresses for contacts in
address fields, rather than only their names. Note that you can
toggle between long/short display by klicking / M-RET on the
contact."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-wrap-lines nil
  "Whether to automatically wrap lines in the body of messages when
viewing them. Note that wrapping does not work well with all
messages, but you can always toggle between wrapped/unwrapped
display with `mu4e-view-toggle-wrap-lines (default keybinding: <w>)."
  :group 'mu4e-view)

(defcustom mu4e-view-date-format "%c"
  "Date format to use in the message view, in the format of
  `format-time-string'."
  :type 'string
  :group 'mu4e-view)

 (defcustom mu4e-view-hide-cited nil
  "Whether to automatically hide cited parts of messages (as
determined by the presence of '> ' at the beginning of the
line). Note that you can always toggle between hidden/unhidden
display with `mu4e-view-toggle-hide-cited (default keybinding:
<w>)."
  :group 'mu4e-view)

(defcustom mu4e-view-image-max-width 800
  "The maximum width for images to display; this is only effective
  if you're using an emacs with Imagemagick support, and
  `mu4e-show-image' is non-nil."
  :group 'mu4e-view)

(defvar mu4e-view-actions
  '( ("capture message" . mu4e-action-capture-message)
     ("view as pdf"     . mu4e-action-view-as-pdf))
  "List of actions to perform on messages in view mode. The actions
are of the form:
   (NAME FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first letter of NAME is used as a shortcut character.")

(defvar mu4e-view-attachment-actions
  '( ("wopen-with" . mu4e-view-open-attachment-with)
     ("ein-emacs"  . mu4e-view-open-attachment-emacs)
     ("|pipe"      . mu4e-view-pipe-attachment))
  "List of actions to perform on message attachments. The actions
are of the form:
   (NAME  FUNC)
where:
    * NAME is the name of the action (e.g. \"Count lines\")
     * FUNC is a function which receives two arguments: the message
      plist and the attachment number.

The first letter of NAME is used as a shortcut character.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; some buffer-local variables / constants
(defvar mu4e~view-headers-buffer nil
  "The headers buffer connected to this view.")

(defvar mu4e~view-lines-wrapped nil "Whether lines are wrapped.")
(defvar mu4e~view-cited-hidden nil "Whether cited lines are hidden.")

(defvar mu4e~view-link-map nil
  "A map of some number->url so we can jump to url by number.")

(defconst mu4e~view-url-regexp
  "\\(\\(https?\\://\\|mailto:\\)[-+a-zA-Z0-9.?_$%/+&#@!*~,:;=/()]+\\)"
  "Regexp that matches http:/https:/mailto: URLs; match-string 1
  will contain the matched URL, if any.")

(defvar mu4e~view-attach-map nil
  "A mapping of user-visible attachment number to the actual part index.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-view-message-with-msgid (msgid)
  "View message with MSGID. This is meant for external programs
wanting to show specific messages - for example, `mu4e-org'."
  ;; note: hackish; if mu4e-decryption-policy is non-nil (ie., t or 'ask), we
  ;; decrpt the message. Since here we don't know if message is encrypted or
  ;; not, when the policy is 'ask'. we simply assume the user said yes...  the
  ;; alternative would be to ask for each message, encrypted or not.  maybe we
  ;; need an extra policy...
  (mu4e~proc-view msgid mu4e-show-images mu4e-decryption-policy))

(defun mu4e-view-message-text (msg)
  "Return the message to display (as a string), based on the MSG
plist."
  (concat
    (mapconcat
      (lambda (field)
	(let ((fieldval (plist-get msg field)))
	  (case field
	    (:subject  (mu4e~view-construct-header field fieldval))
	    (:path     (mu4e~view-construct-header field fieldval))
	    (:maildir  (mu4e~view-construct-header field fieldval))
	    (:flags    (mu4e~view-construct-flags-header fieldval))
	    ;; contact fields
	    (:to       (mu4e~view-construct-contacts-header msg field))
	    (:from     (mu4e~view-construct-contacts-header msg field))
	    (:cc       (mu4e~view-construct-contacts-header msg field))
	    (:bcc      (mu4e~view-construct-contacts-header msg field))

	    ;; if we (`user-mail-address' are the From, show To, otherwise,
	    ;; show From
	    (:from-or-to
	      (let* ((from (plist-get msg :from))
		      (from (and from (cdar from))))
		(if (and from (string-match mu4e-user-mail-address-regexp from))
		  (mu4e~view-construct-contacts-header msg :to)
		  (mu4e~view-construct-contacts-header msg :from))))
	    ;; date
	    (:date
	      (let ((datestr
		      (when fieldval (format-time-string mu4e-view-date-format
				       fieldval))))
		(if datestr (mu4e~view-construct-header field datestr) "")))
	    ;; size
	    (:size
	      (mu4e~view-construct-header field (mu4e-display-size fieldval)))
	    ;; attachments
	    (:attachments (mu4e~view-construct-attachments-header msg))
	    ;; pgp-signatures
	    (:signature   (mu4e~view-construct-signature-header msg))
	    (t (mu4e-error "Unsupported field: %S" field)))))
      mu4e-view-fields "")
    "\n"
    (mu4e-body-text msg)
    ;; ;; decrypt maybe; depends on whether there are any such parts
    ;; ;; and the value of `mu4e-view-decrypt-parts'
    ;; (mu4e~decrypt-parts-maybe msg)
    ))


(defun mu4e-view (msg headersbuf &optional refresh)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

REFRESH is for re-showing an already existing message.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let ((buf (get-buffer-create mu4e~view-buffer-name)))
    (with-current-buffer buf
      (mu4e-view-mode)
      (let ((inhibit-read-only t))
	(setq ;; buffer local
	  mu4e~view-msg msg
	  mu4e~view-headers-buffer headersbuf
	  mu4e~view-buffer buf)

	(erase-buffer)
	(insert (mu4e-view-message-text msg))

	(switch-to-buffer buf)
	(goto-char (point-min))

	(mu4e~view-fontify-cited)
	(mu4e~view-fontify-footer)
	(mu4e~view-make-urls-clickable)
	(mu4e~view-show-images-maybe msg)

	(unless refresh
	  ;; if we're showing the message for the first time, use the values of
	  ;; user-settable variables `mu4e~view-wrap-lines' and
	  ;; `mu4e~view-hide-cited' to determine whether we should wrap/hide
	  (when mu4e-view-wrap-lines (mu4e~view-wrap-lines))
	  (when mu4e-view-hide-cited  (mu4e~view-hide-cited))
	  ;; no use in trying to set flags again
	  (mu4e~view-mark-as-read-maybe))))))


(defun mu4e~view-construct-header (field val &optional dont-propertize-val)
  "Return header field FIELD (as in `mu4e-header-info') with value
VAL if VAL is non-nil. If DONT-PROPERTIZE-VAL is non-nil, do not
add text-properties to VAL."
  (let* ((info (cdr (assoc field mu4e-header-info)))
	  (key (plist-get info :name))
	  (help (plist-get info :help)))
    (if (and val (> (length val) 0))
    (with-temp-buffer
      (insert (propertize key
		'face 'mu4e-view-header-key-face
		'help-echo help) ": "
	(if dont-propertize-val
	  val
	  (propertize val 'face 'mu4e-view-header-value-face)) "\n")
      ;; temporarily set the fill column <margin> positions to the right, so
      ;; we can indent following lines with positions
      (let*((margin 1) (fill-column (- fill-column margin)))
	(fill-region (point-min) (point-max))
	(goto-char (point-min))
	(while (and (zerop (forward-line 1)) (not (looking-at "^$")))
	  (indent-to-column margin)))
      (buffer-string))
    "")))

(defun mu4e~view-toggle-contact (&optional point)
  "Toggle between the long and short versions of long/short string
at POINT, or if nil, at (point)."
  (interactive)
  (unless (get-text-property (or point (point)) 'long)
    (error "point is not toggleable"))
  (let* ((point (or point (point)))
	  ;; find the first pos part of the button
	  (start (previous-property-change point))
	  (start (if start (next-property-change start) (point-min)))
	  ;; find the first pos not part of the button (possibly nil)
	  (end (next-property-change point))
	  (end (or end (1+ (point-max)))) ;; one beyond
	  (longtext (get-text-property point 'long))
	  (shorttext (get-text-property point 'short))
	  (inhibit-read-only t))
    (if (string= (get-text-property point 'display) longtext)
      (add-text-properties start end `(display ,shorttext))
      (add-text-properties start end `(display ,longtext)))))

(defun mu4e~view-compose-contact (&optional point)
  "Compose a message for the address at point."
  (interactive)
  (unless (get-text-property (or point (point)) 'email)
    (error "No address at point"))
  (mu4e~compose-mail (get-text-property (or point (point)) 'email)))

(defun mu4e~view-construct-contacts-header (msg field)
  "Add a header for a contact field (ie., :to, :from, :cc, :bcc)."
  (mu4e~view-construct-header field
    (mapconcat
      (lambda(c)
	(let* ((name (car c))
		(email (cdr c))
		(short (or name email)) ;; name may be nil
		(long (if name (format "%s <%s>" name email) email))
		(map (make-sparse-keymap)))
	  (define-key map [mouse-1] 'mu4e~view-toggle-contact)
	  (define-key map [?\M-\r]  'mu4e~view-toggle-contact)
	  (define-key map [mouse-2] 'mu4e~view-compose-contact)
	  (define-key map "C"  'mu4e~view-compose-contact)
	  (propertize
	    (if mu4e-view-show-addresses long short)
	    'long long
	    'short short
	    'email email
	    'display (if mu4e-view-show-addresses long short)
	    'keymap map
	    'face 'mu4e-view-contact-face
	    'mouse-face 'highlight
	    'help-echo
	    (format (concat
		      "<" email ">\n"
		      "[mouse-1] or [M-RET] to toggle long/short display\n"
		      "[mouse-2] or C to compose a mail for this recipient")))))
      (plist-get msg field) ", ") t))


(defun mu4e~view-construct-flags-header (flags)
  "Construct a Flags: header."
  (mu4e~view-construct-header
    :flags
    (mapconcat
      (lambda (flag)
	(propertize (symbol-name flag) 'face 'mu4e-view-special-header-value-face))
      flags
      (propertize ", " 'face 'mu4e-view-header-value-face)) t))

(defun mu4e~view-construct-signature-header (msg)
  "Construct a Signature: header, if there are any signed parts."
  (let* ((parts (plist-get msg :parts))
	  (verdicts
	    (remove-if 'null
	      (mapcar (lambda (part) (plist-get part :signature)) parts)))
	  (val (when verdicts
		 (mapconcat
		   (lambda (v)
		     (propertize (symbol-name v)
		       'face (if (eq v 'verified) 'mu4e-ok-face 'mu4e-warning-face)))
		   verdicts ", ")))
	  (btn (when val
		 (with-temp-buffer
		   (insert-text-button "Details"
		     'action (lambda (b)
			       (mu4e-view-verify-msg-popup (button-get b 'msg))))
		   (buffer-string))))
	  (val (when val (concat val " (" btn ")"))))
    (mu4e~view-construct-header :signature val t)))

(defun mu4e~view-open-save-attach-func (msg attachnum is-open)
  "Return a function that offers to save attachment NUM. If IS-OPEN
is nil, and otherwise open it."
  (lexical-let ((msg msg) (attachnum attachnum) (is-open is-open))
    (lambda ()
      (interactive)
      (if is-open
	(mu4e-view-open-attachment msg attachnum)
	(mu4e-view-save-attachment-single msg attachnum)))))

(defun mu4e~view-construct-attachments-header (msg)
  "Display attachment information; the field looks like something like:
   	:parts ((:index 1 :name \"1.part\" :mime-type \"text/plain\"
                 :type (leaf) :attachment nil :size 228)
                (:index 2 :name \"analysis.doc\" :mime-type \"application/msword\"
                 :type (leaf attachment) :attachment nil :size 605196))"
  (setq mu4e~view-attach-map ;; buffer local
    (make-hash-table :size 64 :weakness nil))
  (let* ((id 0)
	  (attachments
	    ;; we only list parts that look like attachments, ie. that have a
	    ;; non-nil :attachment property; we record a mapping between user-visible
	    ;; numbers and the part indices
	    (remove-if-not
	      (lambda (part)
		(let ((mtype (plist-get part :mime-type))
			(isattach  (member 'attachment (plist-get part :type))))
		  (or ;; remove if it's not an attach *or* if it's an image/audio/application type
		      ;; (but not a signature)
		    isattach
		    (string-match "^\\(image\\|audio\\)" mtype)
		    (and (string-match "^application" mtype)
		      (not (string-match "signature" mtype))))))
	      (plist-get msg :parts)))
	  (attstr
	    (mapconcat
	      (lambda (part)
		(let ((index (plist-get part :index))
		       (name (plist-get part :name))
		       (size (plist-get part :size))
		       (map (make-sparse-keymap)))
		  (incf id)
		  (puthash id index mu4e~view-attach-map)
		  (define-key map [mouse-2]
		    (mu4e~view-open-save-attach-func msg id nil))
		  (define-key map [?\M-\r]
		    (mu4e~view-open-save-attach-func msg id nil))
		  (define-key map [S-mouse-2]
		    (mu4e~view-open-save-attach-func msg id t))
		  (define-key map (kbd "<S-return>")
		    (mu4e~view-open-save-attach-func msg id t))
		  (concat
		    (propertize (format "[%d]" id)
		      'face 'mu4e-view-attach-number-face)
		    (propertize name 'face 'mu4e-view-link-face
		      'keymap map 'mouse-face 'highlight)
		    (when (and size (> size 0))
		      (concat (format "(%s)"
				(propertize (mu4e-display-size size)
				  'face 'mu4e-view-header-key-face)))))))
	      attachments ", ")))
    (when attachments
      (mu4e~view-construct-header :attachments attstr t))))

;; (defun mu4e~decrypt-parts-maybe (msg)
;;   "Decrypt maybe; depends on whether there are any such parts
;; and the value of `mu4e-view-decrypt-parts'."
;;   (let ((str ""))
;;     (mu4e-view-for-each-part msg
;;       (lambda (msg part)
;; 	(when (member 'encrypted (plist-get part :type))
;; 	  (let ((file (plist-get part :temp)))
;; 	    (when (and file (file-exists-p file))
;; 	      ;; if our mu-server was build with crypto support, we only use EPA
;; 	      ;; to push the password into gpg's memory
;; 	      (let* ((decr
;; 		       (condition-case nil
;; 			 (epg-decrypt-file (epg-make-context epa-protocol) file nil)
;; 			 (err (mu4e-error "Decryption failed: %S" err))))
;; 		      (decr
;; 			(if (and decr (plist-get mu4e~server-props :crypto))

;; 			  )))) ;; TODO: reload message
;; 			 ;; otherwise, we try to handle it here.
;; 			 )
;; 		decr))))))))

(defun mu4e-view-for-each-part (msg func)
    "Apply FUNC to each part in MSG. FUNC should be a function taking
  two arguments:
 1. the message MSG, and
 2. a plist describing the attachment. The plist looks like:
    	 (:index 1 :name \"test123.doc\"
          :mime-type \"application/msword\" :attachment t :size 1234)."
    (dolist (part (mu4e-msg-field msg :parts))
      (funcall func msg part)))


(defvar mu4e-view-mode-map nil
  "Keymap for \"*mu4e-view*\" buffers.")
(unless mu4e-view-mode-map
  (setq mu4e-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'mu4e~view-quit-buffer)

      ;; note, 'z' is by-default bound to 'bury-buffer'
      ;; but that's not very useful in this case
      (define-key map "z" 'mu4e~view-quit-buffer)

      (define-key map "s" 'mu4e-headers-search)
      (define-key map "S" 'mu4e-view-search-edit)
      (define-key map "/" 'mu4e-view-search-narrow)

      (define-key map (kbd "<M-left>")  'mu4e-headers-query-prev)
      (define-key map (kbd "<M-right>") 'mu4e-headers-query-next)

      (define-key map "b" 'mu4e-headers-search-bookmark)
      (define-key map "B" 'mu4e-headers-search-bookmark-edit)

      (define-key map "%" 'mu4e-view-mark-pattern)
      (define-key map "t" 'mu4e-view-mark-subthread)
      (define-key map "T" 'mu4e-view-mark-thread)

      (define-key map "v" 'mu4e-view-verify-msg-popup)

      (define-key map "j" 'mu4e~headers-jump-to-maildir)

      (define-key map "g" 'mu4e-view-go-to-url)

      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-compose-edit)

      (define-key map "." 'mu4e-view-raw-message)
      (define-key map "|" 'mu4e-view-pipe)
      (define-key map "a" 'mu4e-view-action)

      ;; change the number of headers
      (define-key map (kbd "C-+") 'mu4e-headers-split-view-resize)
      (define-key map (kbd "C--")
	(lambda () (interactive) (mu4e-headers-split-view-resize -1)))
      (define-key map (kbd "<C-kp-add>") 'mu4e-headers-split-view-resize)
      (define-key map (kbd "<C-kp-subtract>")
	(lambda () (interactive) (mu4e-headers-split-view-resize -1)))

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'scroll-up)
      (define-key map (kbd "<home>")
	#'(lambda () (interactive) (goto-char (point-min))))
      (define-key map (kbd "<end>")
	#'(lambda () (interactive) (goto-char (point-max))))
      (define-key map (kbd "RET")
	#'(lambda () (interactive) (scroll-up 1)))
      (define-key map (kbd "<backspace>")
	#'(lambda () (interactive) (scroll-up -1)))

      ;; navigation between messages
      (define-key map "p" 'mu4e-view-headers-prev)
      (define-key map "n" 'mu4e-view-headers-next)
      ;; the same
      (define-key map (kbd "<M-down>") 'mu4e-view-headers-next)
      (define-key map (kbd "<M-up>") 'mu4e-view-headers-prev)

      ;; switching to view mode (if it's visible)
      (define-key map "y" 'mu4e-select-other-view)

      ;; attachments
      (define-key map "e" 'mu4e-view-save-attachment)
      (define-key map "o" 'mu4e-view-open-attachment)
      (define-key map "A" 'mu4e-view-attachment-action)

      ;; marking/unmarking
      (define-key map (kbd "<backspace>") 'mu4e-mark-for-trash)
      (define-key map "d" 'mu4e-view-mark-for-trash)

      (define-key map (kbd "<delete>") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "<deletechar>") 'mu4e-mark-for-delete)
      (define-key map (kbd "D") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "m") 'mu4e-view-mark-for-move)
      (define-key map (kbd "&") 'mu4e-view-mark-custom)

      (define-key map (kbd "+") 'mu4e-view-mark-flag)
      (define-key map (kbd "-") 'mu4e-view-mark-unflag)

      (define-key map (kbd "*")             'mu4e-view-mark-deferred)
      (define-key map (kbd "<kp-multiply>") 'mu4e-view-mark-deferred)
      (define-key map (kbd "#") 'mu4e-mark-resolve-deferred-marks)

      ;; misc
      (define-key map "w" 'mu4e-view-toggle-wrap-lines)
      (define-key map "h" 'mu4e-view-toggle-hide-cited)

      (define-key map "r" 'mu4e-view-refresh)

      ;; next 3 only warn user when attempt in the message view
      (define-key map "u" 'mu4e-view-unmark)
      (define-key map "U" 'mu4e-view-unmark-all)
      (define-key map "x" 'mu4e-view-marked-execute)

      (define-key map "$" 'mu4e-show-log)
      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "View")))
	(define-key map [menu-bar headers] (cons "View" menumap))

	(define-key menumap [quit-buffer]
	  '("Quit view" . mu4e~view-quit-buffer))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . mu4e-view-toggle-wrap-lines))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . mu4e-view-toggle-hide-cited))
	(define-key menumap [raw-view]
	  '("View raw message" . mu4e-view-raw-message))
	(define-key menumap [pipe]
	  '("Pipe through shell" . mu4e-view-pipe))
	;; (define-key menumap [inspect]
	;;   '("Inspect with guile" . mu4e-inspect-message))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . mu4e-view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . mu4e-view-save-attachment))
	(define-key menumap [goto-url]
	  '("Visit URL" . mu4e-view-go-to-url))

	(define-key menumap [sepa1] '("--"))
	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mu4e-view-mark-for-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mu4e-view-mark-for-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mu4e-view-mark-for-move))

	(define-key menumap [sepa2] '("--"))
	(define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
	(define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
	(define-key menumap [sepa3] '("--"))


	(define-key menumap [query-next]
	  '("Next query" . mu4e-headers-query-next))
	(define-key menumap [query-prev]
	  '("Previous query" . mu4e-headers-query-prev))
	(define-key menumap [narrow-search]
	  '("Narrow search" . mu4e-headers-search-narrow))
	(define-key menumap [bookmark]
	  '("Search bookmark" . mu4e-headers-search-bookmark))
	(define-key menumap [jump]
	  '("Jump to maildir" . mu4e~headers-jump-to-maildir))
	(define-key menumap [refresh]
	  '("Refresh" . mu4e-headers-rerun-search))
	(define-key menumap [search]
	  '("Search" . mu4e-headers-search))


	(define-key menumap [sepa4] '("--"))
	(define-key menumap [next]  '("Next" . mu4e-view-headers-next))
	(define-key menumap [previous]  '("Previous" . mu4e-view-headers-prev)))
      map)))

(fset 'mu4e-view-mode-map mu4e-view-mode-map)

(defvar mu4e-view-mode-abbrev-table nil)
(define-derived-mode mu4e-view-mode special-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e.
\\{mu4e-view-mode-map}."
  (use-local-map mu4e-view-mode-map)

  (make-local-variable 'mu4e~view-headers-buffer)
  (make-local-variable 'mu4e~view-msg)
  (make-local-variable 'mu4e~view-link-map)
  (make-local-variable 'mu4e~view-attach-map)

  (make-local-variable 'mu4e~view-lines-wrapped)
  (make-local-variable 'mu4e~view-cited-hidden)

  (setq buffer-undo-list t) ;; don't record undo info

  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t))

  ;; filladapt is much better than the built-in filling
  ;; esp. with '>' cited parts
  (when (fboundp 'filladapt-mode)
    (filladapt-mode))
  (setq truncate-lines t))


;; we mark messages are as read when we leave the message; i.e., when skipping
;; to the next/previous one, or leaving the view buffer altogether.

(defun mu4e~view-mark-as-read-maybe ()
  "Clear the current message's New/Unread status and set it to
Seen; if the message is not New/Unread, do nothing."
  (when mu4e~view-msg
    (let ((flags (plist-get mu4e~view-msg :flags))
	   (docid (plist-get mu4e~view-msg :docid)))
      ;; is it a new message?
      (when (or (member 'unread flags) (member 'new flags))
	(mu4e~proc-move docid nil "+S-u-N")))))

(defun mu4e~view-fontify-cited ()
  "Colorize message content based on the citation level."
  (save-excursion
    (let ((more-lines t))
      (goto-char (point-min))
      (when (search-forward-regexp "^\n") ;; search the first empty line
	(while more-lines
	  ;; Get the citation level at point -- i.e., the number of '>'
	  ;; prefixes, starting with 0 for 'no citation'
	  (beginning-of-line 1)
	  ;; consider only lines that heuristically look like a citation line...
	  (when (looking-at "[[:blank:]]*[^[:blank:]]*[[:blank:]]*>")
	    (let* ((level (how-many ">" (line-beginning-position 1)
			    (line-end-position 1)))
		    (face
		      (unless (zerop level)
			(intern-soft (format "mu4e-cited-%d-face" level)))))
	      (when face
		(add-text-properties (line-beginning-position 1)
		  (line-end-position 1) `(face ,face)))))
	  (setq more-lines
	    (and (= 0 (forward-line 1))
	      ;; we need to add this weird check below; it seems in some cases
	      ;; `forward-line' continues to return 0, even when at the end, which
	      ;; would lead to an infinite loop
	      (not (= (point-max) (line-end-position))))))))))

(defun mu4e~view-fontify-footer ()
  "Give the message footers a distinctive color."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (let ((p (search-forward "\n-- \n" nil t)))
	(when p
	  (add-text-properties p (point-max) '(face mu4e-footer-face)))))))

(defun mu4e~view-browse-url-func (url)
  "Return a function that executes `browse-url' with URL. What
browser is called is depending on `browse-url-browser-function' and
`browse-url-mailto-function'."
  (save-match-data
    (if (string-match "^mailto:" url)
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (mu4e~compose-browse-url-mail url)))
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (browse-url url))))))

(defun mu4e~view-show-images-maybe (msg)
  "Show attached images, if `mu4e-show-images' is non-nil."
  (when (and (display-images-p) mu4e-show-images)
    (mu4e-view-for-each-part msg
      (lambda (msg part)
	(when (string-match "^image/" (plist-get part :mime-type))
	  (let ((imgfile (plist-get part :temp)))
	    (when (and imgfile (file-exists-p imgfile))
	      (save-excursion
		(goto-char (point-max))
		(mu4e-display-image imgfile mu4e-view-image-max-width)))))))))

;; this is fairly simplistic...
(defun mu4e~view-make-urls-clickable ()
  "Turn things that look like URLs into clickable things, and
number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e~view-link-map ;; buffer local
	(make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e~view-url-regexp nil t)
	(let ((url (match-string 0))
	       (map (make-sparse-keymap)))
	  (define-key map [mouse-2] (mu4e~view-browse-url-func url))
	  (define-key map [?\M-\r] (mu4e~view-browse-url-func url))
	  (puthash (incf num) url mu4e~view-link-map)
	  (add-text-properties 0 (length url)
	    `(face mu4e-view-link-face
	       mouse-face highlight
	       keymap ,map) url)
	  (replace-match
	    (concat url
	      (propertize (format "[%d]" num)
		'face 'mu4e-view-url-number-face))))))))


(defun mu4e~view-wrap-lines ()
  "Wrap lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (search-forward "\n\n") ;; search for the message body
	(fill-region (point) (point-max)))
      (setq mu4e~view-lines-wrapped t))))

(defun mu4e~view-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (flush-lines "^[:blank:]*>")
      (setq mu4e~view-cited-hidden t))))


(defmacro mu4e~view-in-headers-context (&rest body)
  "Evaluate BODY in the current headers buffer, with moved to the
current message."
  `(progn
     (unless '(buffer-live-p mu4e~view-headers-buffer)
       (mu4e-error "no headers-buffer connected"))
     (let* ((docid (mu4e-field-at-point :docid)))
       (with-current-buffer mu4e~view-headers-buffer
	 (if (and docid (mu4e~headers-goto-docid docid))
	   ,@body
	   (mu4e-error "cannot find message in headers buffer."))))))

(defun mu4e-view-headers-next(&optional n)
  "Move point to the next message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (mu4e~view-in-headers-context
    (mu4e~headers-move (or n 1))))

(defun mu4e-view-headers-prev(&optional n)
  "Move point to the previous message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (mu4e~view-in-headers-context
    (mu4e~headers-move (- (or n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Interactive functions


(defun mu4e-view-toggle-wrap-lines ()
  "Toggle line wrap in the message body."
  (interactive)
  (if mu4e~view-lines-wrapped
    (mu4e-view-refresh)
    (mu4e~view-wrap-lines)))

(defun mu4e-view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mu4e~view-cited-hidden
    (mu4e-view-refresh)
    (mu4e~view-hide-cited)))

(defun mu4e-view-refresh ()
  "Redisplay the current message, without wrapped lines or hidden
citations."
  (interactive)
  (mu4e-view mu4e~view-msg mu4e~view-headers-buffer t)
  (setq
    mu4e~view-lines-wrapped nil
    mu4e~view-cited-hidden nil))

(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG (or message-at-point,
if nil), then do it. The actions are specified in
`mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point t)))
	  (actionfunc (mu4e-read-option "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))

(defun mu4e-view-mark-pattern ()
    "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-headers-mark-pattern)))

(defun mu4e-view-mark-thread ()
  "Ask user for a kind of mark (move, delete etc.), and apply it to
all messages in the thread at point in the headers view."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-headers-mark-thread)))

(defun mu4e-view-mark-subthread ()
  "Ask user for a kind of mark (move, delete etc.), and apply it to
all messages in the thread at point in the headers view."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-headers-mark-subthread)))

(defun mu4e-view-search-narrow ()
  "Run `mu4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-headers-search-narrow nil)))

(defun mu4e-view-search-edit ()
  "Run `mu4e-headers-search-edit' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-headers-search-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attachment handling
(defun mu4e~view-get-attach-num (prompt msg &optional multi)
  "Ask the user with PROMPT for an attachment number for MSG, and
  ensure it is valid. The number is [1..n] for attachments
  [0..(n-1)] in the message. If MULTI is nil, return the number for
  the attachment; otherwise (MULTI is non-nil), accept ranges of
  attachment numbers, as per `mu4e-split-ranges-to-numbers', and
  return the corresponding string."
  (let* ((count (hash-table-count mu4e~view-attach-map)) (def))
    (when (zerop count) (mu4e-error "No attachments for this message"))
    (if (not multi)
      (if (= count 1)
	(read-number (mu4e-format "%s: " prompt) 1)
	(read-number (mu4e-format "%s (1-%d): " prompt count)))
      (progn
	(setq def (if (= count 1) "1" (format "1-%d" count)))
	(read-string (mu4e-format "%s (default %s): " prompt def) nil nil def)))))

(defun mu4e~view-get-attach (msg attnum)
  "Return the attachment plist in MSG corresponding to attachment
number ATTNUM."
  (let* ((partid (gethash attnum mu4e~view-attach-map))
	 (attach
	   (find-if
	     (lambda (part)
	       (eq (plist-get part :index) partid))
	     (plist-get msg :parts))))
    (or attach (mu4e-error "Not a valid attachment"))))


(defun mu4e-view-save-attachment-single (&optional msg attnum)
  "Save attachment number ATTNUM (or ask if nil) from MSG (or
message-at-point if nil) to disk."
  (interactive)
  (unless mu4e-attachment-dir
    (mu4e-error "`mu4e-attachment-dir' is not set"))
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to save" msg)))
	  (att (mu4e~view-get-attach msg attnum))
	  (path (concat mu4e-attachment-dir "/" (plist-get att :name)))
	  (index (plist-get att :index))
	  (retry t))
    (while retry
      (setq path (expand-file-name (read-string
				     (mu4e-format "Save as ") path)))
      (setq retry
	(and (file-exists-p path)
	  (not (y-or-n-p (mu4e-format "Overwrite '%s'?" path))))))
    (mu4e~proc-extract
      'save (plist-get msg :docid) index path)))

(defun mu4e-view-save-attachment-multi (&optional msg)
  "Offer to save multiple email attachments from the current message.
Default is to save all messages, [1..n], where n is the number of
attachments.  You can type multiple values separated by space, e.g.
  1 3-6 8
will save attachments 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which so means all
attachments, but as this is the default, you may not need it."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	 (attachstr (mu4e~view-get-attach-num
		      "Attachment number range (or 'a' for 'all')" msg t))
	  (count (hash-table-count mu4e~view-attach-map))
	  (attachnums (mu4e-split-ranges-to-numbers attachstr count)))
    (dolist (num attachnums)
      (mu4e-view-save-attachment-single msg num))))

(defun mu4e-view-save-attachment (&optional multi)
  "Offer to save attachment(s). If MULTI (prefix-argument) is nil,
save a single one, otherwise, offer to save a range of
attachments."
  (interactive "P")
  (if multi
    (mu4e-view-save-attachment-multi)
    (mu4e-view-save-attachment-single)))

(defun mu4e-view-open-attachment (&optional msg attnum)
  "Open attachment number ATTNUM (or ask if nil) from MSG (or
message-at-point if nil)."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to open" msg)))
	  (att (or (mu4e~view-get-attach msg attnum)))
	  (index (plist-get att :index)))
    (mu4e~proc-extract 'open (plist-get msg :docid) index)))


(defun mu4e~view-temp-action (docid index what &optional param)
  "Open attachment INDEX for message with DOCID, and invoke
ACTION."
  (interactive)
  (mu4e~proc-extract 'temp docid index nil what param))

(defvar mu4e~view-open-with-hist nil
  "History list for the open-with argument.")

(defun mu4e-view-open-attachment-with (msg attachnum &optional cmd)
  "Open MSG's attachment ATTACHNUM with CMD; if CMD is nil, ask
user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (cmd (or cmd
		 (read-string
		   (mu4e-format "Shell command to open it with: ")
		   nil 'mu4e~view-open-with-hist)))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action (plist-get msg :docid) index "open-with" cmd)))

(defvar mu4e~view-pipe-hist nil
  "History list for the pipe argument.")

(defun mu4e-view-pipe-attachment (msg attachnum &optional pipecmd)
  "Feed MSG's attachment ATTACHNUM through pipe PIPECMD; if
PIPECMD is nil, ask user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (pipecmd (or pipecmd
		     (read-string
		       (mu4e-format "Pipe: ")
		       nil
		       'mu4e~view-pipe-hist)))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action (plist-get msg :docid) index "pipe" pipecmd)))


(defun mu4e-view-open-attachment-emacs (msg attachnum)
  "Open MSG's attachment ATTACHNUM in the current emacs instance."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action (plist-get msg :docid) index "emacs")))


(defun mu4e-view-attachment-action (&optional msg)
  "Ask user what to do with attachments in MSG (or nil to use
message-at-point, then do it. The actions are specified in
`mu4e-view-attachment-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point t)))
	  (actionfunc (mu4e-read-option
			"Action on attachment: "
			mu4e-view-attachment-actions))
	  (attnum (mu4e~view-get-attach-num "Which attachment" msg)))
    (when (and actionfunc attnum)
      (funcall actionfunc msg attnum))))


;; handler-function to handle the response we get from the server when we
;; want to do something with one of the attachments.
(defun mu4e~view-temp-handler (path what param)
  "Handler function for doing things with temp files (ie.,
attachments) in response to a (mu4e~proc-extract 'temp ... )."
  (cond
    ((string= what "open-with")
      ;; 'param' will be the program to open-with
      (start-process "*mu4e-open-with-proc*" "*mu4e-open-with*" param path))
    ((string= what "pipe")
      ;; 'param' will be the pipe command, path the infile for this
      (mu4e-process-file-through-pipe path param))
    ((string= what "emacs")
      (find-file path)
      ;; make the buffer read-only since it usually does not make
	;; sense to edit the temp buffer; use C-x C-q if you insist...
      (setq buffer-read-only t))
      (t (mu4e-error "Unsupported action %S" what))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; marking
(defun mu4e~view-mark-set (mark)
  "Set mark on the current messages."
  (let ((docid (mu4e-msg-field mu4e~view-msg :docid)))
    (mu4e~view-in-headers-context
      (if (eq mark 'move)
	(mu4e-mark-for-move-set)
	(mu4e-mark-at-point mark)))))

(defun mu4e-view-mark-custom ()
  "Run some custom mark function."
  (mu4e~view-in-headers-context
    (mu4e-headers-mark-custom)))

(defun mu4e~split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))

(defun mu4e-view-unmark-all ()
  "If we're in split-view, unmark all messages. Otherwise, warn
user that unmarking only works in the header list."
  (interactive)
  (if (mu4e~split-view-p)
    (mu4e~view-in-headers-context (mu4e-mark-unmark-all))
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point. Otherwise, warn
user that unmarking only works in the header list."
  (interactive)
  (if (mu4e~split-view-p)
    (mu4e~view-mark-set 'unmark)
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-mark-for-move ()
  "Mark the current message for moving."
  (interactive)
  (mu4e~view-mark-set 'move)
  (mu4e-view-headers-next))

(defun mu4e-view-mark-for-trash ()
  "Mark the current message for moving to the trash folder."
  (interactive)
  (mu4e~view-mark-set 'trash)
  (mu4e-view-headers-next))

(defun mu4e-view-mark-for-delete ()
  "Mark the current message for deletion."
  (interactive)
  (mu4e~view-mark-set 'delete)
  (mu4e-view-headers-next))

(defun mu4e-view-mark-flag ()
  "Mark the current message for flagging."
  (interactive)
  (mu4e~view-mark-set 'flag)
  (mu4e-view-headers-next))

(defun mu4e-view-mark-unflag ()
  "Mark the current message for unflagging."
  (interactive)
  (mu4e~view-mark-set 'unflag)
  (mu4e-view-headers-next))

(defun mu4e-view-mark-deferred ()
  "Mark the current message for unflagging."
  (interactive)
  (mu4e~view-mark-set 'deferred)
  (mu4e-view-headers-next))

 (defun mu4e-view-marked-execute ()
  "Execute the marks."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-mark-execute-all)))

(defun mu4e-view-go-to-url (num)
  "Go to a numbered url."
  (interactive "n[mu4e] Visit url with number: ")
  (let ((url (gethash num mu4e~view-link-map)))
    (unless url (mu4e-warn "Invalid number for URL"))
    (funcall (mu4e~view-browse-url-func url))))

(defconst mu4e~view-raw-buffer-name "*mu4e-raw-view*"
  "*internal* Name for the raw message view buffer")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-field-at-point :path))
	 (buf (get-buffer-create mu4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (mu4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert-file-contents path)
	(view-mode)
	(goto-char (point-min))))
    (switch-to-buffer buf)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD, and display
the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-field-at-point :path)))
    (mu4e-process-file-through-pipe path cmd)))

(defconst mu4e~verify-buffer-name " *mu4e-verify*")

(defun mu4e-view-verify-msg-popup (&optional msg)
  "Pop-up a little signature verification window for (optional) MSG
or message-at-point."
  (interactive)
  (let* ((path (if msg (plist-get msg :path)  (mu4e-field-at-point :path)))
	  (cmd (format "%s verify --verbose %s"
		 mu4e-mu-binary
		 (shell-quote-argument path)))
	  (output (shell-command-to-string cmd))
	    ;; create a new one
	  (buf (get-buffer-create mu4e~verify-buffer-name))
	  (win (or (get-buffer-window buf)
		 (split-window-vertically (- (window-height) 6)))))
    (with-selected-window win
      (let ((inhibit-read-only t))
	;; (set-window-dedicated-p win t)
	(switch-to-buffer buf)
	(erase-buffer)
	(insert output)
	(goto-char (point-min))
	(local-set-key "q" 'kill-buffer-and-window)))
    (select-window win)))


(defun mu4e~view-quit-buffer ()
  "Quit the mu4e-view buffer. This is a rather complex function, to
ensure we don't disturb other windows."
  (interactive)
  (unless (eq major-mode 'mu4e-view-mode)
    (mu4e-error "Must be in mu4e-view-mode (%S)" major-mode))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
	 (headers-win))
    (walk-windows
      (lambda (win)
	;; check whether the headers buffer window is visible
	(when (eq mu4e~view-headers-buffer (window-buffer win))
	  (setq headers-win win))
	;; and kill any _other_ (non-selected) window that shows the current
	;; buffer
	(when
	  (and
	    (eq curbuf (window-buffer win)) ;; does win show curbuf?
	    (not (eq curwin win))	    ;; but it's not the curwin?
	    (not (one-window-p))) ;; and not the last one on the frame?
	  (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone.
    ;; if the headers view is also visible, kill ourselves + window; otherwise
    ;; switch to the headers view
    (if (window-live-p headers-win)
      ;; headers are visible
      (progn
	(kill-buffer-and-window) ;; kill the view win
	(select-window headers-win)) ;; and switch to the headers win...
      ;; headers are not visible...
      (progn
	(kill-buffer)
	(when (buffer-live-p mu4e~view-headers-buffer)
	  (switch-to-buffer mu4e~view-headers-buffer))))))

(provide 'mu4e-view)
;; end of mu4e-view
