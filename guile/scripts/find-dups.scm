#!/bin/sh
exec guile -e main -s $0 $@
!#
;;
;; Copyright (C) 2013-2015 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;; INFO: Find duplicate messages
;; INFO: options:
;; INFO:   --muhome=<muhome>: path to mu home dir
;; INFO:   --delete: delete all but the first one (experimental, be careful!)

(use-modules (mu) (mu script) (mu stats))
(use-modules (ice-9 getopt-long) (ice-9 optargs)
  (ice-9 popen) (ice-9 format) (ice-9 rdelim))

(define (md5sum path)
  (let* ((port (open-pipe* OPEN_READ "md5sum" path))
         (md5 (read-delimited " " port)))
    (close-pipe port)
    md5))
 
(define (find-dups delete expr)
  (let ((id-table (make-hash-table 20000)))
    ;; fill the hash with <msgid-size> => <list of paths>
    (mu:for-each-message
      (lambda (msg)
	(let* ((id (format #f "~a-~d" (mu:message-id msg)
		    (mu:size msg)))
	       (lst (hash-ref id-table id)))
	  (if lst
	    (set! lst (cons (mu:path msg) lst))
	    (set! lst (list (mu:path msg))))
	  (hash-set! id-table id lst)))
      expr)
    ;; list all the paths with multiple elements; check the md5sum to
    ;; make 100%-minus-ε sure they are really the same file.
    (hash-for-each
      (lambda (id paths)
	(if (> (length paths) 1)
	  (let ((hash (make-hash-table 10)))
	    (for-each
             (lambda (path)
               (when (file-exists? path)
                     (let* ((md5 (md5sum path)) (lst (hash-ref hash md5)))
                       (if lst
                           (set! lst (cons path lst))
                           (set! lst (list path)))
                       (hash-set! hash md5 lst))))
 	      paths)
	    ;; hash now maps the md5sum to the messages...
	    (hash-for-each
	      (lambda (md5 mpaths)
	    	(if (> (length mpaths) 1)
		  (begin
		    ;;(format #t "md5sum: ~a:\n" md5)
		    (let ((num 1))
		      (for-each
                       (lambda (path)
                         (if (equal? num 1)
			   (format #t "~a\n" path)
			   (begin
			     (format #t "~a: ~a\n" (if delete "deleting" "dup") path)
			     (if delete (delete-file path))))
                         (set! num (+ 1 num)))
			mpaths)))))
	      hash))))
      id-table)))



(define (main args)
  "Find duplicate messages and, potentially, delete the dups.
   Be careful with that!
Interpret argument-list ARGS (like command-line
arguments). Possible arguments are:
  --muhome (path to alternative mu home directory).
  --delete (delete all but the first one). Run mu index afterwards.
  --expr   (expression to constrain search)."
  (setlocale LC_ALL "")
  (let* ((optionspec   '( (muhome     (value #t))
                          (delete     (value #f))
			  (expr       (value #t))
			  (help       (single-char #\h) (value #f))))
	  (options (getopt-long args optionspec))
	  (help (option-ref options 'help #f))
	  (delete (option-ref options 'delete #f))
	  (expr (option-ref options 'expr #t))
	  (muhome (option-ref options 'muhome #f)))
    (mu:initialize muhome)
    (find-dups delete expr)))


;; Local Variables:
;; mode: scheme
;; End:









