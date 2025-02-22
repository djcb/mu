;;; mu4e-update.el --- Update the mu4e message store -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Dirk-Jan C. Binnema

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

;; Updating the mu4e message store: calling a mail retrieval program and
;; re-running the index.

;;; Code:

(require 'mu4e-helpers)
(require 'mu4e-server)

;;; Customization

(defcustom mu4e-get-mail-command "true"
  "Shell command for retrieving new mail.
Common values are \"offlineimap\", \"fetchmail\" or \"mbsync\", but
arbitrary shell-commands can be used.

When set to the literal string \"true\" (the default), the
command simply finishes successfully (running the \"true\"
command) without retrieving any mail. This can be useful when
mail is already retrieved in another way, such as a local MDA."
  :type 'string
  :group 'mu4e
  :safe 'stringp)

(defcustom mu4e-index-update-error-warning t
  "Whether to display warnings during the retrieval process.
This depends on the `mu4e-get-mail-command' exit code."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-update-error-continue t
  "Whether to continue with indexing after an error during retrieval."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-update-in-background t
  "Whether to retrieve mail in the background."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-cleanup t
  "Whether to run a cleanup phase after indexing.

That is, validate that each message in the message store has a
corresponding message file in the filesystem.

Having this option as t ensures that no non-existing messages are
shown but can slow with large message stores on slow file-systems."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-index-lazy-check nil
  "Whether to only use a \"lazy\" check during reindexing.
This influences how we decide whether a message
needs (re)indexing or not.

When this is set to non-nil, mu only uses the directory
timestamps to decide whether it needs to check the messages
beneath it. This makes indexing much faster, but might miss some
changes. For this, you might want to occasionally call
`mu4e-update-index-nonlazy'; `mu4e-update-pre-hook' can be used
to automate this."
  :type 'boolean
  :group 'mu4e
  :safe 'booleanp)

(defcustom mu4e-update-interval nil
  "Number of seconds between mail retrieval/indexing.
If nil, don't update automatically. Note, changes in
`mu4e-update-interval' only take effect after restarting mu4e.

Important, the automatic update *only* works when `mu4e' is
running."
  :type '(choice (const :tag "No automatic update" nil)
                 (integer :tag "Seconds"))
  :group 'mu4e
  :safe 'integerp)

(defvar mu4e-update-pre-hook nil
  "Hook run just *before* the mail-retrieval / database updating process starts.
You can use this hook for example to `mu4e-get-mail-command' with
some specific setting.")

(defcustom mu4e-hide-index-messages nil
  "Whether to hide the \"Indexing...\" and contacts messages."
  :type 'boolean
  :group 'mu4e)

(defvar mu4e-index-updated-hook nil
  "Hook run when the indexing process has completed.
The variable `mu4e-index-update-status' can be used to get
information about what changed.")

(defvar mu4e-message-changed-hook nil
  "Hook run when there is a message changed in the data store.
For new messages, it depends on `mu4e-index-updated-hook'. This
can be used as a simple way to invoke some action when a message
changed")

(defvar mu4e-index-update-status nil
  "Last-seen completed update status, based on server status messages.

If non-nil, this is a plist of the form:
\(
:checked     <number of messages processed> (checked whether up-to-date)
:updated     <number of messages updated/added
:cleaned-up  <number of stale messages removed from store
:stamp       <emacs (current-time) timestamp for the status)")

(defconst mu4e-last-update-buffer "*mu4e-last-update*"
  "Name of buffer with cloned from the last update buffer.
Useful for diagnosing update problems.")

;;; Internal variables / const
(defconst mu4e--update-name " *mu4e-update*"
  "Name of the process and buffer to update mail.")
(defvar mu4e--progress-reporter nil
  "Internal, the progress reporter object.")
(defvar mu4e--update-timer nil
  "The mu4e update timer.")
(defconst mu4e--update-buffer-height 8
  "Height of the mu4e message retrieval/update buffer.")
(defvar mu4e--get-mail-ask-password "mu4e get-mail: Enter password: "
  "Query string for `mu4e-get-mail-command' password.")
(defvar mu4e--get-mail-password-regexp "^Remote: Enter password: $"
  "Regexp for a `mu4e-get-mail-command' password query.")

(defun mu4e--get-mail-process-filter (proc msg)
  "Filter the MSG output of the `mu4e-get-mail-command' PROC.

Currently the filter only checks if the command asks for a
password by matching the output against
`mu4e~get-mail-password-regexp'. The messages are inserted into
the process buffer.

Also scrolls to the final line, and update the progress
throbber."
  (when mu4e--progress-reporter
    (progress-reporter-update mu4e--progress-reporter))

  (when (string-match mu4e--get-mail-password-regexp msg)
    (if (process-get proc 'x-interactive)
        (process-send-string proc
                             (concat (read-passwd mu4e--get-mail-ask-password)
                                     "\n"))
      ;; TODO kill process?
      (mu4e-error "Unrecognized password request")))
  (when (process-buffer proc)
    (let ((inhibit-read-only t)
          (procwin (get-buffer-window (process-buffer proc))))
      ;; Insert at end of buffer. Leave point alone.
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (if (string-match ".*\r\\(.*\\)" msg)
            (progn
              ;; kill even with \r
              (end-of-line)
              (let ((end (point)))
                (beginning-of-line)
                (delete-region (point) end))
              (insert (match-string 1 msg)))
          (insert msg)))
      ;; Auto-scroll unless user is interacting with the window.
      (when (and (window-live-p procwin)
                 (not (eq (selected-window) procwin)))
        (with-selected-window procwin
          (goto-char (point-max)))))))

(defun mu4e-index-message (frm &rest args)
  "Display FRM with ARGS like `mu4e-message' for index messages.
However, if `mu4e-hide-index-messages' is non-nil, do not display anything."
  (unless mu4e-hide-index-messages
    (apply 'mu4e-message frm args)))

(defun mu4e-update-index ()
  "Update the mu4e index."
  (interactive)
  (mu4e--server-index mu4e-index-cleanup mu4e-index-lazy-check))

(defun mu4e-update-index-nonlazy ()
  "Update the mu4e index non-lazily.
This is just a convenience wrapper for indexing the non-lazy way
if you otherwise want to use `mu4e-index-lazy-check'."
  (interactive)
  (let ((mu4e-index-cleanup t) (mu4e-index-lazy-check nil))
    (mu4e-update-index)))

(defvar mu4e--update-buffer nil
  "The buffer of the update process when updating.")

(define-derived-mode mu4e--update-mail-mode special-mode "mu4e:update"
  "Major mode used for retrieving new e-mail messages in `mu4e'.")

(define-key mu4e--update-mail-mode-map (kbd "q") 'mu4e-kill-update-mail)

(defun mu4e--temp-window (buf height)
  "Create a temporary window with HEIGHT at the bottom BUF.

This function uses `display-buffer' with a default preset.

To override this behavior, customize `display-buffer-alist'."
  (display-buffer buf `(display-buffer-at-bottom
                        (preserve-size . (nil . t))
                        (height . ,height)
                        (inhibit-same-window . t)
                        (window-height . fit-window-to-buffer)))
  (set-window-buffer (get-buffer-window buf) buf))

(defun mu4e--update-sentinel-func (proc _msg)
  "Sentinel function for the update process PROC."
  (when mu4e--progress-reporter
    (progress-reporter-done mu4e--progress-reporter)
    (setq mu4e--progress-reporter nil))
  (unless mu4e-hide-index-messages
    (message nil))
  (if (or (not (eq (process-status proc) 'exit))
          (/= (process-exit-status proc) 0))
      (progn
        (when mu4e-index-update-error-warning
          (mu4e-message "Update process returned with non-zero exit code")
          (sit-for 5))
        (when mu4e-index-update-error-continue
          (mu4e-update-index)))
    (mu4e-update-index))
  (when (buffer-live-p mu4e--update-buffer)
    (delete-windows-on mu4e--update-buffer)
    ;; clone the update buffer for diagnosis
    (when (get-buffer mu4e-last-update-buffer)
      (kill-buffer mu4e-last-update-buffer))
    (with-current-buffer mu4e--update-buffer
      (special-mode)
      (clone-buffer mu4e-last-update-buffer))
    ;; and kill the buffer itself; the cloning is needed
    ;; so the temp window handling works as expected.
    (kill-buffer mu4e--update-buffer)))

;; complicated function, as it:
;;   - needs to check for errors
;;   - (optionally) pop-up a window
;;   - (optionally) check password requests
(defun mu4e--update-mail-and-index-real (run-in-background)
  "Get a new mail by running `mu4e-get-mail-command'.
If
RUN-IN-BACKGROUND is non-nil (or called with prefix-argument),
run in the background; otherwise, pop up a window."
  (let* ((process-connection-type t)
         (proc (start-process-shell-command
                mu4e--update-name mu4e--update-name
                mu4e-get-mail-command))
         (buf (process-buffer proc))
         (win (or run-in-background
                  (mu4e--temp-window buf mu4e--update-buffer-height))))
    (set-process-query-on-exit-flag proc nil)
    (setq mu4e--update-buffer buf)
    (when (window-live-p win)
      (with-selected-window win
        (erase-buffer)
        (insert "\n") ;; FIXME -- needed so output starts
        (mu4e--update-mail-mode)))
    (setq mu4e--progress-reporter
          (unless mu4e-hide-index-messages
            (make-progress-reporter
             (mu4e-format "Retrieving mail..."))))
    (set-process-sentinel proc 'mu4e--update-sentinel-func)
    ;; if we're running in the foreground, handle password requests
    (unless run-in-background
      (process-put proc 'x-interactive (not run-in-background))
      (set-process-filter proc 'mu4e--get-mail-process-filter))))

(defun mu4e-update-mail-and-index (run-in-background)
  "Retrieve new mail by running `mu4e-get-mail-command'.
If RUN-IN-BACKGROUND is non-nil (or called with prefix-argument),
run in the background; otherwise, pop up a window."
  (interactive "P")
  (unless mu4e-get-mail-command
    (mu4e-error "`mu4e-get-mail-command' is not defined"))
  (if (and (buffer-live-p mu4e--update-buffer)
           (process-live-p (get-buffer-process mu4e--update-buffer)))
      (mu4e-message "Update process is already running")
    (progn
      (run-hooks 'mu4e-update-pre-hook)
      (mu4e--update-mail-and-index-real run-in-background))))

(defun mu4e-kill-update-mail ()
  "Stop the update process by killing it."
  (interactive)
  (let* ((proc (and (buffer-live-p mu4e--update-buffer)
                    (get-buffer-process mu4e--update-buffer))))
    (when (process-live-p proc)
      (kill-process proc t))))

(define-minor-mode mu4e-update-minor-mode
  "Mode for triggering mu4e updates."
  :global nil
  :init-value nil ;; disabled by default
  :group 'mu4e
  :lighter ""
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "C-S-u")   #'mu4e-update-mail-and-index)
    ;; for terminal users
    (define-key map  (kbd "C-c C-u") #'mu4e-update-mail-and-index)
    map))

(provide 'mu4e-update)
;;; mu4e-update.el ends here
