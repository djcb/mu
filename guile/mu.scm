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

(define-module (mu)
  :use-module (oop goops)
  :use-module (mu log)
  :use-module (mu msg)
  :use-module (mu contact)
  :export
  (for-each-contact
   for-each-message)) ;; note, defined in libguile-mu (in c)

(load-extension "libguile-mu" "mu_guile_init")

(define* (for-each-contact proc #:optional (expr ""))
  "Execute PROC for each contact. PROC receives a <contact> instance
as parameter. If EXPR is specified, only consider contacts in messages
matching EXPR."
  (let ((c-hash (make-hash-table 4096)))
    (for-each-message
      (lambda (msg)
	(for-each
	  (lambda (name-addr)
	    (let ((contact (make <contact>
			     #:name      (car name-addr)
			     #:email     (cdr name-addr)
			     #:timestamp (mu:msg:date msg))))
	      (update-contacts-hash c-hash contact)))
	  (append (mu:msg:to msg) (mu:msg:from msg) (mu:msg:cc msg)
	    (mu:msg:bcc msg))))
      expr)
    ;; c-hash now contains a map of email->contact
    (hash-for-each
      (lambda (email contact) (proc contact)) c-hash)))

(define-method (update-contacts-hash c-hash (nc <contact>))
  "Update the contacts hash with a new and/or existing contact."
  ;; xc: existing-contact, nc: new contact
  (let ((xc (hash-ref c-hash (email nc))))
    (if (not xc) ;; no existing contact with this email address?
      (hash-set! c-hash (email nc) nc) ;; store the new contact.
      ;; otherwise:
      (begin
	;; 1) update the frequency for the existing contact
	(set! (frequency xc) (1+ (frequency xc)))
	;; 2) update the name if the new one is not empty and its timestamp is newer
	;;    in that case, also update the timestamp
	(if (and (name nc) (> (string-length (name nc)))
	      (> (timestamp nc) (timestamp xc)))
	  (set! (name xc) (name nc))
	  (set! (timestamp xc) (timestamp nc)))
	;; 3) update last-seen with timestamp, if x's timestamp is newer
	(if (> (timestamp nc) (last-seen xc))
	  (set! (last-seen xc) (timestamp nc)))
	;; okay --> now xc has been updated; but it back in the hash
	(hash-set! c-hash (email xc) xc)))))
