;;; mu4e-mark.el --- Marking messages -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file are function related to marking messages; they assume we are
;; currently in the headers buffer.

;;; Code:

(require 'mu4e-server)
(require 'mu4e-message)
(require 'mu4e-folders)

;; keep byte-compiler happy
(declare-function mu4e~headers-mark "mu4e-headers")
(declare-function mu4e~headers-goto-docid "mu4e-headers")
(declare-function mu4e-headers-next "mu4e-headers")

;;; Variables & constants

(defcustom mu4e-headers-leave-behavior 'ask
  "What to do when user leaves the current headers view.

\"Leaving\" here means quitting the headers views, refreshing it
or even quitting mu4e or Emacs.

Value is one of the following symbols:
- `ask'     ask user whether to ignore the marks
- `apply'   automatically apply the marks before doing anything else
- `ignore'  automatically ignore the marks without asking"
  :type '(choice (const :tag "ask user whether to ignore marks" ask)
                 (const :tag "apply marks without asking"       apply)
                 (const :tag "ignore marks without asking"      ignore))
  :group 'mu4e-headers)

(defcustom mu4e-mark-execute-pre-hook nil
  "Hook run just *before* a mark is applied to a message.
The hook function is called with two arguments, the mark being
executed and the message itself."
  :type 'hook
  :group 'mu4e-headers)

(defvar mu4e-headers-show-target t
  "Whether to show targets (such as \"-> delete\", \"-> /archive\")
when marking message. Normally, this is useful information for
the user, however, when you often mark large numbers (thousands)
of message, showing the target makes this quite a bit
slower (showing the target uses Emacs overlays, which can be slow
when overused).")

(defvar mu4e-trash-without-flag nil
  "Non-nil means avoid adding the Maildir T flag when trashing.

When \"trashing\" a message, it is moved to the \"trash\"-folder.
Furthermore, as per the Maildir-spec, the \"T\" flag is added to
its filename. This marks it for *manual* removal later.

Some message retrieval and IMAP synchronization tools, however,
interpret this flag instead as a trigger for *automatic* removal,
may not be what the user expects. If, so set the flag to non-nil.
This makes the \"trashing\" merely a move the trash-folder.")

;;; Insert stuff

(defvar mu4e--mark-map nil
  "Contains a mapping of docid->markinfo.
When a message is marked, the information is added here. markinfo
is a cons cell consisting of the following: (mark . target) where
MARK is the type of mark (move, trash, delete) TARGET (optional)
is the target directory (for \"move\")")

;; the mark-map is specific for the current header buffer
;; currently, there can't be more than one, but we never know what will
;; happen in the future

;; the fringe is the space on the left of headers, where we put marks below some
;; handy definitions; only `mu4e-mark-fringe-len' should be change (if ever),
;; the others follow from that.
(defconst mu4e--mark-fringe-len 2
  "Width of the fringe for marks on the left.")
(defconst mu4e--mark-fringe (make-string mu4e--mark-fringe-len ?\s)
  "The space on the left of message headers to put marks.")
(defconst mu4e--mark-fringe-format (format "%%-%ds" mu4e--mark-fringe-len)
  "Format string to set a mark and leave remaining space.")

(defun mu4e--mark-initialize ()
  "Initialize the marks-subsystem."
  (set (make-local-variable 'mu4e--mark-map) (make-hash-table))
  ;; ask user when kill buffer / emacs with live marks.
  ;; (subject to mu4e-headers-leave-behavior)
  (add-hook 'kill-buffer-query-functions
            #'mu4e-mark-handle-when-leaving nil t)
  (add-hook 'kill-emacs-query-functions
            #'mu4e-mark-handle-when-leaving nil t))

(defun mu4e--mark-clear ()
  "Clear the marks-subsystem."
  (clrhash mu4e--mark-map))

(defun mu4e--mark-find-headers-buffer ()
  "Find the headers buffer, if any."
  (seq-find (lambda (_)
              (mu4e-current-buffer-type-p 'headers))
            (buffer-list)))

(defmacro mu4e--mark-in-context (&rest body)
  "Evaluate BODY in the context of the headers buffer.
The current buffer must be either a headers or view buffer."
  `(cond
    ((mu4e-current-buffer-type-p 'headers) ,@body)
    ((mu4e-current-buffer-type-p 'view)
     (when (buffer-live-p (mu4e-get-headers-buffer))
       (let* ((msg (mu4e-message-at-point))
              (docid (mu4e-message-field msg :docid)))
         (with-current-buffer (mu4e-get-headers-buffer)
           (when (mu4e~headers-goto-docid docid)
             ,@body
             )))))))

(defconst mu4e-marks
  '((refile
     :char ("r" . "▶")
     :prompt "refile"
     :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
     :action (lambda (docid msg target)
               (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
    (delete
     :char ("D" . "x")
     :prompt "Delete"
     :show-target (lambda (target) "delete")
     :action (lambda (docid msg target) (mu4e--server-remove docid)))
    (flag
     :char ("+" . "✚")
     :prompt "+flag"
     :show-target (lambda (target) "flag")
     :action (lambda (docid msg target)
               (mu4e--server-move docid nil "+F-u-N")))
    (move
     :char ("m" . "▷")
     :prompt "move"
     :ask-target  mu4e--mark-get-move-target
     :action (lambda (docid msg target)
               (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
    (read
     :char    ("!" . "◼")
     :prompt "!read"
     :show-target (lambda (target) "read")
     :action (lambda (docid msg target) (mu4e--server-move docid nil "+S-u-N")))
    (trash
     :char ("d" . "▼")
     :prompt "dtrash"
     :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
     :action (lambda (docid msg target)
               (mu4e--server-move docid
                                  (mu4e--mark-check-target target)
                                  (if mu4e-trash-without-flag "-N" "+T-N"))))
    (unflag
     :char    ("-" . "➖")
     :prompt "-unflag"
     :show-target (lambda (target) "unflag")
     :action (lambda (docid msg target) (mu4e--server-move docid nil "-F-N")))
    (untrash
     :char   ("=" . "▲")
     :prompt "=untrash"
     :show-target (lambda (target) "untrash")
     :action (lambda (docid msg target) (mu4e--server-move docid nil "-T")))
    (unread
     :char    ("?" . "◻")
     :prompt "?unread"
     :show-target (lambda (target) "unread")
     :action (lambda (docid msg target) (mu4e--server-move docid nil "-S+u-N")))
    (unmark
     :char  " "
     :prompt "unmark"
     :action (mu4e-error "No action for unmarking"))
    (action
     :char ( "a" . "◯")
     :prompt "action"
     :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
     :action  (lambda (docid msg actionfunc)
                (save-excursion
                  (when (mu4e~headers-goto-docid docid)
                    (mu4e-headers-action actionfunc)))))
    (something
     :char  ("*" . "✱")
     :prompt "*something"
     :action (mu4e-error "No action for deferred mark")))

  "The list of all the possible marks.
This is an alist mapping mark symbols to their properties.  The
properties are:
  :char (string) or (basic . fancy) The character to display in
    the headers view. Either a single-character string, or a
    dotted-pair cons cell where the second item will be used if
    `mu4e-use-fancy-chars' is t, otherwise we'll use
    the first one. It can also be a plain string for backwards
    compatibility since we didn't always support
    `mu4e-use-fancy-chars' here.
  :prompt (string) The prompt to use when asking for marks (used for
     example when marking a whole thread)
  :ask-target (function returning a string) Get the target.  This
     function run once per bulk-operation, and thus is suitable
     for user-interaction.  If nil, the target is nil.
  :dyn-target (function from (TARGET MSG) to string).  Compute
     the dynamic target.  This is run once per message, which is
     passed as MSG.  The default is to just return the target.
  :show-target (function from TARGET to string) How to display
     the target.
  :action (function taking (DOCID MSG TARGET)).  The action to
     apply on the message.")

(defun mu4e-mark-at-point (mark target)
  "Mark (or unmark) message at point.
MARK specifies the mark-type. For `move'-marks and `trash'-marks
the TARGET argument is non-nil and specifies to which maildir the
message is to be moved/trashed. The function works in both
headers buffers and message buffers.

The following marks are available, and the corresponding props:

   MARK       TARGET    description
   ----------------------------------------------------------
   `refile'    y        mark this message for archiving
   `something' n        mark this message for *something* (decided later)
   `delete'    n        remove the message
   `flag'      n        mark this message for flagging
   `move'      y        move the message to some folder
   `read'      n        mark the message as read
   `trash'     n        trash the message to some folder
   `unflag'    n        mark this message for unflagging
   `untrash'   n        remove the `trashed' flag from a message
   `unmark'    n        unmark this message
   `unread'    n        mark the message as unread
   `action'    y        mark the message for some action."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (docid (mu4e-message-field msg :docid))
         ;; get a cell with the mark char and the "move" already has a target
         ;; (the target folder) the other ones get a pseudo "target", as info
         ;; for the user.
         (markdesc (cdr (or (assq mark mu4e-marks)
                            (mu4e-error "Invalid mark %S" mark))))
         (get-markkar
          (lambda (char)
            (if (listp char)
                (if mu4e-use-fancy-chars (cdr char) (car char))
              char)))
         (markkar (funcall get-markkar (plist-get markdesc :char)))
         (target (mu4e--mark-get-dyn-target mark target))
         (show-fct (plist-get markdesc :show-target))
         (shown-target (if show-fct
                           (funcall show-fct target)
                         (if target (format "%S" target)))))
    (unless docid (mu4e-warn "No message on this line"))
    (unless (eq major-mode 'mu4e-headers-mode)
      (mu4e-error "Not in headers-mode"))
    (save-excursion
      (when (mu4e~headers-mark docid markkar)
        ;; update the hash -- remove everything current, and if add the new
        ;; stuff, unless we're unmarking
        (remhash docid mu4e--mark-map)
        ;; remove possible mark overlays
        (remove-overlays (line-beginning-position) (line-end-position)
                         'mu4e-mark t)
        ;; now, let's set a mark (unless we were unmarking)
        (unless (eql mark 'unmark)
          (puthash docid (cons mark target) mu4e--mark-map)
          ;; when we have a target (ie., when moving), show the target folder in
          ;; an overlay
          (when (and shown-target mu4e-headers-show-target)
            (let* ((targetstr (propertize (concat "-> " shown-target " ")
                                          'face 'mu4e-system-face))
                   ;; mu4e~headers-goto-docid docid t \will take us just after
                   ;; the docid cookie and then we skip the mu4e--mark-fringe
                   (start (+ (length mu4e--mark-fringe)
                             (mu4e~headers-goto-docid docid t)))
                   (overlay (make-overlay start (min (line-end-position)
                                                     (+ start (length targetstr))))))
              (overlay-put overlay 'display targetstr)
              (overlay-put overlay 'mu4e-mark t)
              (overlay-put overlay 'evaporate t)
              docid)))))))

(defun mu4e--mark-get-move-target ()
  "Ask for a move target, and propose to create it if it does not exist."
  (let* ((target (mu4e-ask-maildir "Move message to: "))
         (target (if (string= (substring target 0 1) "/")
                     target
                   (concat "/" target)))
         (fulltarget (mu4e-join-paths (mu4e-root-maildir) target)))
    (when (mu4e-create-maildir-maybe fulltarget)
      target)))

(defun mu4e--mark-ask-target (mark)
  "Ask the target for MARK, if the user should be asked the target."
  (let ((getter (plist-get (cdr (assq mark mu4e-marks)) :ask-target)))
    (and getter (funcall getter))))

(defun mu4e--mark-get-dyn-target (mark target)
  "Get the dynamic TARGET for MARK.
The result may depend on the message at point."
  (let ((getter (plist-get (cdr (assq mark mu4e-marks)) :dyn-target)))
    (if getter
        (funcall getter target (mu4e-message-at-point))
      target)))

(defun mu4e-mark-set (mark &optional target)
  "Mark the header at point with MARK or all in the region.
Optionally, provide TARGET (for moves)."
  (unless target
    (setq target (mu4e--mark-ask-target mark)))
  (if (not (use-region-p))
      ;; single message
      (mu4e-mark-at-point mark target)
    ;; mark all messages in the region.
    (save-excursion
      (let ((cant-go-further) (eor (region-end)))
        (goto-char (region-beginning))
        (while (and (< (point) eor) (not cant-go-further))
          (mu4e-mark-at-point mark target)
          (setq cant-go-further (not (mu4e-headers-next))))))))

(defun mu4e-mark-restore (docid)
  "Restore the visual mark for the message with DOCID."
  (let ((markcell (gethash docid mu4e--mark-map)))
    (when markcell
      (save-excursion
        (when (mu4e~headers-goto-docid docid)
          (mu4e-mark-at-point (car markcell) (cdr markcell)))))))

(defun mu4e--mark-get-markpair (prompt &optional allow-something)
  "Ask user with PROMPT for a mark and return (MARK . TARGET).
If ALLOW-SOMETHING is non-nil, allow the `something' pseudo mark
as well."
  (let* ((marks (mapcar (lambda (markdescr)
                          (cons (plist-get (cdr markdescr) :prompt)
                                (car markdescr)))
                        mu4e-marks))
         (marks
          (if allow-something
              marks (seq-remove (lambda (m) (eq 'something (cdr m))) marks)))
         (mark (mu4e-read-option prompt marks))
         (target (mu4e--mark-ask-target mark)))
    (cons mark target)))

(defun mu4e-mark-resolve-deferred-marks ()
  "Check if there are any deferred ('something') mark-instances.
If there are such marks, replace them with a _real_ mark (ask the
user which one)."
  (interactive)
  (mu4e--mark-in-context
   (let ((markpair))
     (maphash
      (lambda (docid val)
        (let ((mark (car val)))
          (when (eql mark 'something)
            (unless markpair
              (setq markpair
                    (mu4e--mark-get-markpair "Set deferred mark(s) to: " nil)))
            (save-excursion
              (when (mu4e~headers-goto-docid docid)
                (mu4e-mark-set (car markpair) (cdr markpair)))))))
      mu4e--mark-map))))

(defun mu4e--mark-check-target (target)
  "Check if TARGET exists; if not, offer to create it."
  (let ((fulltarget (mu4e-join-paths (mu4e-root-maildir) target)))
    (if (not (mu4e-create-maildir-maybe fulltarget))
        (mu4e-error "Target dir %s does not exist " fulltarget)
      target)))

(defun mu4e-mark-execute-all (&optional no-confirmation)
  "Execute the actions for all marked messages in this buffer.
After the actions have been executed successfully, the affected
messages are *hidden* from the current header list. Since the
headers are the result of a search, we cannot be certain that the
messages no longer match the current one - to get that
certainty, we need to rerun the search, but we don't want to do
that automatically, as it may be too slow and/or break the user's
flow. Therefore, we hide the message, which in practice seems to
work well.

If NO-CONFIRMATION is non-nil, don't ask user for confirmation."
  (interactive "P")
  (mu4e--mark-in-context
   (let* ((marknum (mu4e-mark-marks-num))
          (prompt (format "Are you sure you want to execute %d mark%s?"
                          marknum (if (> marknum 1) "s" ""))))
     (if (zerop marknum)
         (mu4e-warn "Nothing is marked")
       (mu4e-mark-resolve-deferred-marks)
       (when (or no-confirmation (y-or-n-p prompt))
         (maphash
          (lambda (docid val)
            (let* ((mark (car val)) (target (cdr val))
                   (markdescr (assq mark mu4e-marks))
                   (msg (save-excursion
                          (mu4e~headers-goto-docid docid)
                          (mu4e-message-at-point))))
              ;; note: whenever you do something with the message,
              ;; it looses its N (new) flag
              (if markdescr
                  (progn
                    (run-hook-with-args
                     'mu4e-mark-execute-pre-hook mark msg)
                    (funcall (plist-get (cdr markdescr) :action)
                             docid msg target))
                (mu4e-error "Unrecognized mark %S" mark))))
          mu4e--mark-map))
       (mu4e-mark-unmark-all 'no-confirm)
       (message nil)))))

(defun mu4e-mark-unmark-all (&optional no-confirmation)
  "Unmark all marked messages."
  (interactive)
  (mu4e--mark-in-context
   (when (zerop (mu4e-mark-marks-num))
     (mu4e-warn "Nothing is marked"))
   (let* ((marknum (hash-table-count mu4e--mark-map))
          (prompt (format "Are you sure you want to unmark %d message%s?"
                          marknum (if (> marknum 1) "s" ""))))
     (when (or no-confirmation (y-or-n-p prompt))
       (maphash
        (lambda (docid _val)
          (save-excursion
            (when (mu4e~headers-goto-docid docid)
              (mu4e-mark-set 'unmark))))
        mu4e--mark-map)
       ;; in any case, clear the marks map
       (mu4e--mark-clear)))))

(defun mu4e-mark-docid-marked-p (docid)
  "Is the given DOCID marked?"
  (when (gethash docid mu4e--mark-map) t))

(defun mu4e-mark-marks-num ()
  "Return the number of mark-instances in the current buffer."
  (mu4e--mark-in-context
   (if mu4e--mark-map (hash-table-count mu4e--mark-map) 0)))

(defun mu4e-mark-handle-when-leaving ()
  "Handle any mark-instances in the current buffer when leaving.
This is done according to the value of
`mu4e-headers-leave-behavior'. This function is to be called
before any further action (like searching, quitting the buffer)
is taken; returning t means \"take the following action\", return
nil means \"don't do anything\"."
  (mu4e--mark-in-context
   (let ((marknum (mu4e-mark-marks-num))
         (what mu4e-headers-leave-behavior))
     (unless (zerop marknum) ;; nothing to do?
       (when (eq what 'ask)
         (setq what (mu4e-read-option
                     (format  "There are %d existing mark(s); should we: "
                              marknum)
                     '( ("apply marks"   . apply)
                        ("ignore marks?" . ignore)))))
       ;; we determined what to do... now do it
       (when (eq what 'apply)
         (mu4e-mark-execute-all t)))))
  t) ;; return t for compat with `kill-buffer-query-functions

;;; _
(provide 'mu4e-mark)
;;; mu4e-mark.el ends here
