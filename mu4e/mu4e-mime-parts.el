;;; mu4e-mime-parts.el --- Dealing with MIME-parts & URLs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Dirk-Jan C. Binnema

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

;; Implements functions and variables for dealing with MIME-parts and URLs.

;;; TODO:
;; [~] mime part candidate sorting -> is his even possible generally?
;; [ ] URL support

;;; Code:
(require 'mu4e-vars)
(require 'mu4e-folders)
(require 'gnus-art)
(require 'crm)

(defcustom mu4e-view-open-program
  (pcase system-type
    ('darwin "open")
    ('cygwin "cygstart")
    (_ "xdg-open"))
  "Tool to open the correct program for a given file or MIME-type.
May also be a function of a single argument, the file to be
opened.

In the function-valued case a likely candidate is
`mailcap-view-file' although note that there was an Emacs bug up
to Emacs 29 which prevented opening a file if `mailcap-mime-data'
specified a function as viewer."
  :type '(choice string function)
  :group 'mu4e-view)

(defcustom mu4e-uniquify-save-file-name-function 'mu4e--uniquify-file-name
  "Function to create a unique, not-yet-existing file name.

Takes one parameter, a file-name path, and returns a file-name
path that does not yet exist. This can be the same, or some
variation.

See `mu4e--uniquify-file-name' for an example."
  :type 'function
  :group 'mu4e-view)

;; remember the mime-handles, so we can clean them up when
;; we quit this buffer.
(defvar-local mu4e~gnus-article-mime-handles nil)
(put 'mu4e~gnus-article-mime-handles 'permanent-local t)

(defun mu4e--view-kill-mime-handles ()
  "Kill cached MIME-handles, if any."
  (when mu4e~gnus-article-mime-handles
    (mm-destroy-parts mu4e~gnus-article-mime-handles)
    (setq mu4e~gnus-article-mime-handles nil)))

;;; MIME-parts
(defvar-local mu4e--view-mime-parts nil
  "Cached MIME parts for this message.")

(defun mu4e-view-mime-parts()
  "Get the list of MIME parts for this message.
The list is a list of plists, one for each MIME-part.

The plists have the properties:

    :part-index  : Gnus index number
    :mime-type   : MIME-type (string) or nil
    :encoding    : Content encoding (string) or nil
    :disposition : Content disposition (attachment\" or inline\") or nil
    :filename    : The file name if it has one, or an invented one
                   otherwise

There are some internal fields as well, e.g. ; subject to change:

    :target-dir      : Target directory for saving
    :attachment-like : When it has a filename, we can save it
    :handle          : Gnus handle."
  (or mu4e--view-mime-parts
      (setq
       mu4e--view-mime-parts
       (let ((parts) (indices))
         (save-excursion
           (goto-char (point-min))
           (while (not (eobp))
             (when-let* ((part (get-text-property (point) 'gnus-data))
                        (index (get-text-property (point) 'gnus-part)))
               (when (and part (numberp index) (not (member index indices)))
                 (let* ((disp (mm-handle-disposition part))
                        (fname (mm-handle-filename part))
                        (mime-type (mm-handle-media-type part))
                        (info
                         `(:part-index  ,index
                           :mime-type   ,mime-type
                           :encoding    ,(mm-handle-encoding part)
                           :disposition ,(car-safe disp)

                           ;; if there's no file-name, invent one
                           ;; XXX perhaps guess extension based on mime-type
                           :filename   ,(or fname
                                            (format "mime-part-%02d" index))

                           ;; below are internal

                           :target-dir ,(mu4e-determine-attachment-dir
                                         fname mime-type)
                           ;; 'attachment-like' just means it has its own
                           ;; filename an we thus we can save it through
                           ;; `mu4e-view-save-attachments', even if it has an
                           ;; 'inline' disposition.
                           :attachment-like ,(if fname t nil)
                           :handle          ,part)))
                   (push index indices)
                   (push info parts))))
             (goto-char (or (next-single-property-change (point) 'gnus-part)
                            (point-max)))))
         ;; sort by the GNU's part-index, so the order is the same as
         ;; in the message on screen
         (seq-sort (lambda (p1 p2) (< (plist-get p1 :part-index)
                                      (plist-get p2 :part-index))) parts)))))

;; https://emacs.stackexchange.com/questions/74547/completing-read-search-also-in-annotationsxc

(defun mu4e--uniquify-file-name (fname)
  "Return a not-yet-existing filename based on FNAME.

If FNAME does not yet exist, return it unchanged.
Otherwise, return a file with a unique number appended to the base-name."
  (let ((num 1) (orig-name fname))
    (while (file-exists-p fname)
      (setq fname (format "%s(%d)%s%s"
                          (file-name-sans-extension orig-name)
                          num
                          (if (file-name-extension orig-name) "." "")
                          (file-name-extension orig-name)))
      (cl-incf num)))
  fname)

(defvar mu4e--completions-table nil)

(defun mu4e-view-complete-all ()
  "Pick all current candidates."
  (interactive)
  (if (bound-and-true-p helm-mode)
      (mu4e-warn "Not supported with helm")
    (when mu4e--completions-table
        (insert (string-join
                 (seq-map #'car mu4e--completions-table) ", ")))))

(defvar mu4e-view-completion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'mu4e-view-complete-all)
    ;; XXX perhaps a binding for clearing all?
    map)
  "Keybindings for mu4e-view completion.")

(define-minor-mode mu4e-view-completion-minor-mode
  "Minor-mode for completing mu4e mime parts."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap mu4e-view-completion-minor-mode-map)

(defun mu4e--part-annotation (candidate part type longest-filename)
  "Calculate the annotation candidates as per annotation.
I.e., `:annotation-function' (see `completion-extra-properties')

CANDIDATE is the value to annotate.

PART is the matching MIME-part for the annotation, (as per
`mu4e-view-mime-part').

TYPE is the of what to annotate, a symbol, either ATTACHMENT or
MIME-PART.

LONGEST-FILENAME is the length of the longest filename; this
information' is used for alignment."
  (let* ((filename (propertize (or  (plist-get part :filename) "")
                               'face 'mu4e-header-key-face))
         (mimetype (propertize (or (plist-get part :mime-type) "")
                               'face 'mu4e-header-value-face))
         (target (propertize (or  (plist-get part :target-dir) "")
                             'face 'mu4e-system-face)))

    ;; Sadly, we need too align by hand; this makes some assumptions
    ;; such a mono-type font and enough space in the minibuffer; and
    ;; mixing values and representation; ideally Emacs would allow
    ;; just take some columns and align them (since it knows the display
    ;; details).

    (pcase type
      ('attachment
       ;; in case we're annotating an attachment, the filename is
       ;; the candidate (completion), so we don't need it in the
       ;; the annotation. We just need to but some space at beginning
       ;; for alignment
       (concat
        (make-string  (-  (+ longest-filename 2)
                          (length (format "%s" candidate))) ?\s)
        (format "%20s" mimetype)
        "       "
        (format "%s" (concat "-> " target))))
      ('mime-part
       ;; when we're annotating a mime-part, the candidate is just a number,
       ;; and the filename is part of the annotation.
       (concat
        "  "
        filename
        (make-string  (-  (+ longest-filename 2)
                          (length filename)) ?\s)
        (format "%20s" mimetype)
        "       "
        (format "%s" (concat "-> " target))))
      (_ (mu4e-error "Unsupported annotation type %s" type)))))

(defvar helm-comp-read-use-marked)
(defun mu4e--completing-read-real (prompt candidates multi)
  "Call the appropriate completion-read function.
- PROMPT is a string informing the user what to complete
- CANDIDATES is an alist of candidates of the form
    (id . part)
- MULTI if t, allow for completing _multiple_ candidates."
  (cond
   ((bound-and-true-p helm-mode)
    ;; tweaks for "helm"; it's not nice to have to special-case for
    ;; completion frameworks, but this has been supported for while.
    ;; basically, with helm, helm-comp-read-use-marked + completing-read
    ;; is preferred over completing-read-multiple
    (let ((helm-comp-read-use-marked t))
      (completing-read prompt candidates)))
   (multi
    (completing-read-multiple prompt candidates))
   (t
    (completing-read prompt candidates))))

(defun mu4e--completing-read (prompt candidates type &optional multi)
  "Read the part-id of some MIME-type in this message.

Presents the user with completions for the MIME-parts in
the current message.

- PROMPT is a string informing the user what to complete
- CANDIDATES is an alist of candidates of the form
    (id . part)
- TYPE is the annotation type to uses as per `mu4e--part-annotation'.
Optionally,
- MULTI if t, allow for completing _multiple_ candidates."
  (cl-assert candidates)
  (let* ((longest-filename (seq-max
                            (seq-map (lambda (c)
                                       (length (plist-get (cdr c) :filename)))
                                     candidates)))
         (annotation-func (lambda (candidate)
                            (mu4e--part-annotation candidate
                                                   (cdr-safe
                                                    (assoc candidate candidates))
                                                   type longest-filename)))
         (completion-extra-properties
          `(;; :affixation-function requires emacs 28
            :annotation-function ,annotation-func
            :exit-function (lambda (_a _b) (setq mu4e--completions-table nil)))))
    (setq mu4e--completions-table candidates)
    (minibuffer-with-setup-hook
        (lambda ()
          (mu4e-view-completion-minor-mode))
      (mu4e--completing-read-real prompt candidates multi))))

(defun mu4e-view-save-attachments (&optional ask-dir)
  "Save files from the current view buffer.

This applies to all MIME-parts that are \"attachment-like\" (have
a filename), regardless of their disposition.

With ASK-DIR is non-nil, user can specify the target-directory; otherwise
one is determined using `mu4e-attachment-dir'.

This command assumes unique filenames for the attachments, since
that is how the underlying completion mechanism works. If there
are duplicates, only one is recognized.

Furthermore, file-names that match `crm-separator' (by default, a
comma and some optional whitespace) are not supported (see
`completing-read-multiple' for further details). Hence, when we
detect that, the function bails out and advises to use
`mu4e-view-mime-part-action' instead, which does support such
files."
  (interactive "P")
  (let* ((parts (mu4e-view-mime-parts))
         (candidates  (seq-map
                       (lambda (fpart)
                         (let ((fname (plist-get fpart :filename)))
                           (when (and crm-separator (string-match-p crm-separator fname))
                             (mu4e-warn (concat  "File(s) match `crm-separator'; "
                                                 "use mu4e-view-mime-part-action instead")))
                           ;; (filename . annotation)
                           (cons fname fpart)))
                       (seq-filter
                        (lambda (part) (plist-get part :attachment-like))
                        parts)))
         (candidates (or candidates
                         (mu4e-warn "No attachments for this message")))
         (files (mu4e--completing-read "Save file(s): " candidates
                                       'attachment 'multi))
         (custom-dir (when ask-dir (read-directory-name
                                    "Save to directory: "))))
    ;; we have determined what files to save, and where.
    (seq-do (lambda (fname)
              (let* ((part (cdr (assoc fname candidates)))
                     (path (funcall mu4e-uniquify-save-file-name-function
                            (mu4e-join-paths
                             (or custom-dir (plist-get part :target-dir))
                             (plist-get part :filename))))
                     (handle (plist-get part :handle)))
                (when handle ;; completion may fail, and then there's no handle.
                  (mm-save-part-to-file handle path))))
            files)))

(defvar mu4e-view-mime-part-actions
  '(
    ;;
    ;; some basic ones
    ;;

    ;; save MIME-part to a file
    (:name "save"  :handler gnus-article-save-part :receives index)
    ;; pipe MIME-part to some arbitrary shell command
    (:name "|pipe" :handler gnus-article-pipe-part :receives index)
    ;; open with the default handler, if any
    (:name "open" :handler mu4e--view-open-file :receives temp)
    ;; open with some custom file.
    (:name "wopen-with" :handler (lambda (file)(mu4e--view-open-file file t))
           :receives temp)

    ;;
    ;; some more examples
    ;;

    ;; import GPG key
    (:name "gpg" :handler epa-import-keys :receives temp)
    ;; open in this emacs instance; tries to use the attachment name,
    ;; so emacs can use specific modes etc.
    (:name "emacs" :handler find-file-read-only :receives temp)
    ;; open in this emacs instance, "raw"
    (:name "raw" :handler (lambda (str)
                            (let ((tmpbuf
                                   (get-buffer-create " *mu4e-raw-mime*")))
                              (with-current-buffer tmpbuf
                                (insert str)
                                (view-mode)
                                (goto-char (point-min)))
                              (display-buffer tmpbuf))) :receives pipe))

  "Specifies actions for MIME-parts.

Each of the actions is a plist with keys
`(:name <name>         ;; name of the action; shortcut is first letter of name

  :handler             ;; one of:
                       ;; - a function receiving the index/temp/pipe
                       ;; - a string, which is taken as a shell command

  :receives            ;;  a symbol specifying what the handler receives
                       ;; - index: the index number of the mime part (default)
                       ;; - temp: the full path to the mime part in a
                       ;;         temporary file, which is deleted immediately
                       ;;         after the handler returns
                       ;; - pipe:  the attachment is piped to some shell command
                       ;;          or as a string parameter to a function
).")

(defun mu4e--view-mime-part-to-temp-file (handle)
  "Write MIME-part HANDLE to a temporary file and return the file name.
The filename is deduced from the MIME-part's filename, or
otherwise random; the result is placed in a temporary directory
with a unique name. Returns the full path for the file created.
The directory and file are self-destructed."
  (let* ((tmpdir (make-temp-file "mu4e-temp-" t))
         (fname (mm-handle-filename handle))
         (fname (and fname
                     (gnus-map-function mm-file-name-rewrite-functions
                                        (file-name-nondirectory fname))))
         (fname (if fname
                    (concat tmpdir "/" (replace-regexp-in-string "/" "-" fname))
                  (let ((temporary-file-directory tmpdir))
                    (make-temp-file "mimepart")))))
    (mm-save-part-to-file handle fname)
    (run-at-time "30 sec" nil
                 (lambda () (ignore-errors (delete-directory tmpdir t))))
    fname))

(defun mu4e--view-open-file (file &optional force-ask)
  "Open FILE with default handler, if any.
Otherwise, or if FORCE-ASK is set, ask user for the program to
open with."
  (if (and (not force-ask)
           (functionp mu4e-view-open-program))
      (funcall mu4e-view-open-program file)
    (let ((opener
           (or (and (not force-ask) mu4e-view-open-program
                    (executable-find mu4e-view-open-program))
               (read-shell-command "Open MIME-part with: "))))
      (call-process opener nil 0 nil file))))

(defun mu4e-view-mime-part-action (&optional n)
  "Apply some action to MIME-part N in the current message.
If N is not specified, ask for it. For instance, '3 A o' opens
the third MIME-part."
  ;; (interactive
  ;;  (list (read-number "Number of MIME-part: ")))
  (interactive "P")
  (let* ((parts (mu4e-view-mime-parts))
         (candidates (seq-map
                      (lambda (part)
                        (cons (number-to-string
                               (plist-get part :part-index)) part))
                      parts))
         (candidates (or candidates
                         (mu4e-warn "No MIME-parts for this message")))
         (ids (seq-map #'string-to-number
                       (if n (list (number-to-string n))
                           (mu4e--completing-read "MIME-part(s) to operate on: "
                                                  candidates
                                                  'mime-part 'multi))))
         (options
          (mapcar (lambda (action) `(,(plist-get action :name) . ,action))
                  mu4e-view-mime-part-actions))
         (action
          (or (and options (mu4e-read-option "Action: " options))
              (mu4e-error "No such action")))
         (handler
          (or (plist-get action :handler)
              (mu4e-error "No :handler item found for action %S" action)))
         (receives
          (or (plist-get action :receives)
              (mu4e-error "No :receives item found for action %S" action))))

    ;; Apply the action to all selected MIME-parts
    (seq-do (lambda (id)
              (cl-assert (numberp id))
              (let* ((part (or (cdr-safe (assoc (number-to-string id) candidates))
                               (mu4e-error "No part found for id %s" id)))
                     (handle (plist-get part :handle)))
                (save-excursion
                  (cond
                   ((functionp handler)
                    (cond
                     ((eq receives 'index) (funcall handler id))
                     ((eq receives 'pipe)
                      (funcall handler (mm-with-unibyte-buffer
                                         (mm-insert-part handle)
                                         (buffer-string))))
                     ((eq receives 'temp)
                      (funcall handler
                               (mu4e--view-mime-part-to-temp-file handle)))
                     (t (mu4e-error "Invalid :receive for %S" action))))
                   ((stringp handler)
                    (cond
                     ((eq receives 'index)
                      (shell-command
                       (concat handler " " (shell-quote-argument id))))
                     ((eq receives 'pipe)
                      (progn
                        (mm-pipe-part handle handler)))
                     ((eq receives 'temp)
                      (shell-command
                       (shell-command
                        (concat
                         handler " "
                         (shell-quote-argument
                          (mu4e--view-mime-part-to-temp-file handle))))))
                     (t (mu4e-error "Invalid action %S" action))))))))
            ids)))

(defun mu4e-process-file-through-pipe (path pipecmd)
  "Process file at PATH through a pipe with PIPECMD."
  (let ((buf (get-buffer-create "*mu4e-output")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process-shell-command pipecmd path t t)
        (view-mode)))
    (display-buffer buf)))


(provide 'mu4e-mime-parts)
;;; mu4e-mime-parts.el ends here
