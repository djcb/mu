;;; mua-mu.el -- part of mua, the mu mail user agent
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

;; mua-mu contains common functions that interact with the mu program

;;; Code:
(eval-when-compile (require 'cl))


(defun mua/mu-run (&rest args)
  "Run 'mu' synchronously with ARGS as command-line argument;,
where <exit-code> is the exit code of the program, or 1 if the
process was killed. <str> contains whatever the command wrote on
standard output/error, or nil if there was none or in case of
error. Basically, `mua/mu-run' is like `shell-command-to-string',
but with better possibilities for error handling. The --muhome=
parameter is added automatically if `mua/mu-home' is non-nil."
  (let* ((rv)
	  (args (append args (when mua/mu-home
			       (list (concat "--muhome=" mua/mu-home)))))
	  (cmdstr (concat mua/mu-binary " " (mapconcat 'identity args " ")))
	  (str (with-output-to-string
		 (with-current-buffer standard-output ;; but we also get stderr...
		   (setq rv (apply 'call-process mua/mu-binary nil t nil	  
			      args))))))
    (when (and (numberp rv) (/= 0 rv))
      (mua/log "mua error: %s" (mua/mu-error rv)))
    (mua/log "%s => %S" cmdstr rv)
    `(,(if (numberp rv) rv 1) . ,str)))
    	  
(defun mua/mu-binary-version ()
  "Get the version string of the mu binary, or nil if we failed
to get it"
  (let ((rv (mua/mu-run "--version")))
    (if (and (= (car rv) 0) (string-match "version \\(.*\\)$" (cdr rv)))
      (match-string 1 (cdr rv))
      (mua/warn "Failed to get version string"))))


(defun mua/mu-view-sexp (path)
  "Return a string with an s-expression representing the message
at PATH; the format is described in `mua/msg-from-string', and
that function converts the string into a Lisp object (plist)"
  (if (not (file-readable-p path))
    (mua/warn "Cannot view unreadable file %s" path)
    (let* ((rv (mua/mu-run "view" "--format=sexp" path))
	    (code (car rv)) (str (cdr rv)))
      (if (= code 0)
	str
	(mua/warn "mu view failed (%d): %s"
	  code (if str str "error"))))))


(defvar mua/db-update-proc  nil "*internal* process for db updates")
(defvar mua/db-update-name "*mua-db-update*"
  "*internal* name of the db-update process")
(defvar mua/db-add-paths    nil "list of paths to add to database")
(defvar mua/db-remove-paths nil "list of paths to remove from database")

(defun mua/db-update-proc-sentinel (proc msg)
  "Check the process upon completion"
  (let ((procbuf (process-buffer proc))
	 (status (process-status proc))
	 (exit-status (process-exit-status proc)))
    (when (and (buffer-live-p procbuf) (memq status '(exit signal)))
      (case status
	('signal (mua/warn "Process killed"))
	('exit
	  (case exit-status
	    (mua/warn "Result: %s" (mua/mu-log exit-status))))))
    (mua/mu-db-update-execute)))

(defun mua/mu-db-update-execute ()
  "Update the database; remove paths in `mua/db-remove-paths',
and add paths in `mua/db-add-paths'. Updating is ansynchronous."

  ;; when it's already running, do nothing
  (unless (and mua/db-update-proc (eq (process-status mua/db-update-proc) 'run))
    (when mua/db-remove-paths
      (let ((remove-paths (copy-list mua/db-remove-paths)))
	(mua/log (concat mua/mu-binary " remove "
		   (mapconcat 'identity remove-paths " ")))
	(setq mua/db-remove-paths nil) ;; clear the old list
	(setq mua/db-update-proc
	  (apply 'start-process mua/db-update-name mua/db-update-name mua/mu-binary
	    "remove" remove-paths))
      (set-process-sentinel mua/db-update-proc 'mua/db-update-proc-sentinel))))
         
  ;; when it's already running, do nothing
  (unless (and mua/db-update-proc
	  (eq (process-status mua/db-update-proc) 'run))
    (when mua/db-add-paths
      (let ((add-paths (copy-list mua/db-add-paths)))
	(mua/log (concat mua/mu-binary " add " (mapconcat 'identity add-paths " ")))
	(setq mua/db-add-paths nil) ;; clear the old list
	(setq mua/db-update-proc
	  (apply 'start-process mua/db-update-name mua/db-update-name mua/mu-binary
	    "add" add-paths))
	(set-process-sentinel mua/db-update-proc 'mua/db-update-proc-sentinel)))))


(defun mua/mu-add-async (path-or-paths)
  "Asynchronously add msg at PATH-OR-PATHS to
database. PATH-OR-PATHS is either a single path or a list of
them."
  (setq mua/db-add-paths
    (append mua/db-add-paths
      (if (listp path-or-paths) path-or-paths `(,path-or-paths)))) 
  (mua/mu-db-update-execute))

(defun mua/mu-remove-async (path-or-paths)
  "Asynchronously remove msg at PATH-OR-PATHS from
database. PATH-OR-PATHS is either a single path or a list of
them."
  (setq mua/db-remove-paths
    (append mua/db-remove-paths
      (if (listp path-or-paths) path-or-paths `(,path-or-paths))))
  (mua/mu-db-update-execute))



;; generated with:
;; cat mu-util.h | sed 's/\([A-Z_]\+\).*=\(.*\),/(defconst \L\1 \2)/' < "$<" \
;;     	| sed 's/_/-/g' > mu-errors.el
(defconst mu-error  1)
(defconst mu-error-in-parameters  2)
(defconst mu-error-internal  3)	
(defconst mu-error-no-matches  4)
(defconst mu-error-xapian  11)
(defconst mu-error-xapian-query	 13)
(defconst mu-error-xapian-dir-not-accessible  14)
(defconst mu-error-xapian-not-up-to-date  15)
(defconst mu-error-xapian-missing-data	16)
(defconst mu-error-xapian-corruption  17)
(defconst mu-error-xapian-cannot-get-writelock	18)
(defconst mu-error-gmime  30)
(defconst mu-error-contacts  50)
(defconst mu-error-contacts-cannot-retrieve  51)
(defconst mu-error-file	 70)
(defconst mu-error-file-invalid-name  71)
(defconst mu-error-file-cannot-link  72)
(defconst mu-error-file-cannot-open  73)
(defconst mu-error-file-cannot-read  74)
(defconst mu-error-file-cannot-create  75)
(defconst mu-error-file-cannot-mkdir  76)
(defconst mu-error-file-stat-failed  77)
(defconst mu-error-file-readdir-failed	78)
(defconst mu-error-file-invalid-source	79)
(defconst mu-error-file-target-equals-source 80)


(defun mua/mu-error (err)
  "Convert an exit code from mu into a string."
  (cond 
    ((eql err mu-error)					"General error")
    ((eql err mu-error-in-parameters)			"Error in parameters")
    ((eql err mu-error-internal)			"Internal error")
    ((eql err mu-error-no-matches)			"No matches")
    ((eql err mu-error-xapian)				"Xapian error")
    ((eql err mu-error-xapian-query)			"Error in query")
    ((eql err mu-error-xapian-dir-not-accessible)	"Database dir not accessible")
    ((eql err mu-error-xapian-not-up-to-date)		"Database is not up-to-date")
    ((eql err mu-error-xapian-missing-data)		"Missing data")
    ((eql err mu-error-xapian-corruption)		"Database seems to be corrupted")
    ((eql err mu-error-xapian-cannot-get-writelock)	"Database is locked")
    ((eql err mu-error-gmime)				"GMime-related error")
    ((eql err mu-error-contacts)			"Contacts-related error")
    ((eql err mu-error-contacts-cannot-retrieve)	"Failed to retrieve contacts")
    ((eql err mu-error-file)				"File error")
    ((eql err mu-error-file-invalid-name)		"Invalid file name")
    ((eql err mu-error-file-cannot-link)		"Failed to link file")
    ((eql err mu-error-file-cannot-open)		"Cannot open file")
    ((eql err mu-error-file-cannot-read)		"Cannot read file")
    ((eql err mu-error-file-cannot-create)		"Cannot create file")
    ((eql err mu-error-file-cannot-mkdir)		"mu-mkdir failed")
    ((eql err mu-error-file-stat-failed)		"stat(2) failed")
    ((eql err mu-error-file-readdir-failed)		"readdir failed")
    ((eql err mu-error-file-invalid-source)		"Invalid source file")
    ((eql err mu-error-file-target-equals-source)	"Source is same as target")
    (t (format						"Unknown error (%d)" err))))


(provide 'mua-mu)

