;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

;; some guile/scheme functions to get various statistics of my mu
;; message store.

(use-modules (ice-9 optargs) (ice-9 popen))

(define-module (mu contacts)
  :use-module (mu log)
  :use-module (mu store)
  :use-module (mu msg)
  :use-module (ice-9 format)
  :use-module (srfi srfi-1)
  :export
  (mu:contacts:list
   mu:contacts:convert
   mu:contacts:export))

(define (mu:contacts:hash)
  "Create a hash of all the contacts (name . email) in the store. Each entry looks like
   email-address => #(<name> <freq> <tstamp>)."
  (let ((contacts-hash (make-hash-table 2048))) ;; the contacts hash
    (mu:store:for-each
      (lambda (msg)
	(for-each
	  (lambda (contact)
	    (let* ((tstamp (mu:msg:date msg))
		    ;; the contact we just found
		    (name (car contact))
		    (email (cadr contact))
		    ;; the contact found in the hash
		    (entry (hash-ref contacts-hash email))
		    (hash-name   (and entry (vector-ref entry 0)))
		    (hash-freq   (and entry (vector-ref entry 1)))
		    (hash-tstamp (and entry (vector-ref entry 2)))
		    ;; we don't use last-seen yet
		    (last-seen   (if (and hash-tstamp (> hash-tstamp tstamp))
				   hash-tstamp
				   tstamp)))
	      (if (not entry)
		(hash-set! contacts-hash email (vector name 1 tstamp))
		;; we replace the name field if either:
		;; 1) the timestamp is newer and the name is non-empty, or
		;; 2) the current name is empty
		(if (and (> tstamp hash-tstamp) name (> (string-length name) 0))
		  (hash-set! contacts-hash email (vector name (1+ hash-freq) tstamp))
		  ;; otherwise, only update the freq, and possibly the last-seen
		  (hash-set! contacts-hash email
		    (vector hash-name (1+ hash-freq) hash-tstamp))))))
	  (append (mu:msg:to msg) (mu:msg:from msg) (mu:msg:cc msg) (mu:msg:bcc msg))))
      "")
    contacts-hash))

(define* (mu:contacts:list #:optional (sortfunc #f))
  "Get an unsorted list of contacts (each of which is a contact-vector
#(<email> <name> <freq> <tstamp>). If SORTFUNC is provided, sort the
list using SORT-FUNC. SORT-FUNC takes as arguments two contact-vectors
and returns #t if the first one is smaller than the second one."
  (let* ((lst (hash-map->list
		(lambda (email vec)
		  (vector email
		    (vector-ref vec 0)
		    (vector-ref vec 1)
		    (vector-ref vec 2)))
		(mu:contacts:hash)))
	  (lst (if (not sortfunc)
		 lst
		 (sort lst sortfunc))))
    lst))

(define (mu:contacts:convert contact format)
  "Convert a contact vector CONTACT into FORMAT, where format is a
symbol, either 'org-contact, 'mutt-alias, 'bbdb, 'wl, or 'plain."
  (let* ( (email  (vector-ref contact 0))
	  (name   (or (vector-ref contact 1) email))
	  (freq   (vector-ref contact 2))
	  (tstamp (vector-ref contact 3))
	  (nick   (email))) ;; FIXME
    (case format
      ('mutt-alias
	(format #f "alias ~a ~a <~a>\n" nick name email))
      ('org-contact
	(format #f "* ~a\n:PROPERTIES:\n:EMAIL:~a\n:NICK:~a\n:END:\n\n"
	  name nick email))
      ('wl ;; wanderlust
	(format #f "~a \"~a\" \"~a\"\n" email nick name))
      ('plain
	(format #f "~a <~a>\n" name email))
      (else (error "unsupported format ~s" format)))))

(define* (mu:contacts:export format #:optional (sortfunc #f) (maxnum #f))
  "Write contacts to standard output, optionally sorted with SORTFUNC and optionally only the first MAXNUM entries."
  (let* ((clist (mu:contacts:list sortfunc))
	  (clist (if maxnum (take clist maxnum) clist)))
    (for-each
      (lambda (contact)
	(mu:contacts:convert contact format))
      clist)))
