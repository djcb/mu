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

(define-module (mu contact)
  :use-module (oop goops)
  :use-module (mu message)
  :export (
	    <mu-contact>
	    name email
	    ;;
	    mu:for-each-contact
	    ;;
	    contacts
	    ;;
	    <mu-contact-with-stats>
	    frequency last-seen
	    ))

(define-class <mu-contact> ()
  (name #:init-value #f  #:accessor name  #:init-keyword #:name)
  (email #:init-value #f #:accessor email #:init-keyword #:email))

(define-method (contacts (msg <mu-message>) contact-type)
  "Get all contacts for MSG of the given CONTACT-TYPE. MSG is of type <mu-message>,
while contact type is either `mu:to', `mu:cc', `mu:from' or `mu:bcc'
to get the corresponding type of contacts, or #t to get all. Returns a
list of <mu-contact> objects."
  (map (lambda (pair) ;; a pair (na . addr)
	 (make <mu-contact>  #:name (car pair) #:email (cdr pair)))
    (mu:get-contacts (slot-ref msg 'msg) contact-type)))

(define-method (contacts (msg <mu-message>))
  "Get contacts of all types for message MSG as a list of <mu-contact>
objects."
  (contacts msg #t))


(define-class <mu-contact-with-stats> (<mu-contact>)
  (tstamp #:init-value 0 #:accessor timestamp #:init-keyword #:timestamp)
  (last-seen #:init-value 0 #:accessor last-seen)
  (freq #:init-value 1 #:accessor frequency))

(define* (mu:for-each-contact proc #:optional (expr #t))
  "Execute PROC for each contact. PROC receives a <mu-contact> instance
as parameter. If EXPR is specified, only consider contacts in messages
matching EXPR."
  (let ((c-hash (make-hash-table 4096)))
    (mu:for-each-message
      (lambda (msg)
	(for-each
	  (lambda (ct)
	    (let ((ct-ws (make <mu-contact-with-stats>
			   #:name      (name  ct)
			   #:email     (email ct)
			   #:timestamp (date msg))))
	      (update-contacts-hash c-hash ct-ws)))
	  (contacts msg #t)))
      expr)
    (hash-for-each ;; c-hash now contains a map of email->contact
      (lambda (email ct-ws) (proc ct-ws)) c-hash)))

(define-method (update-contacts-hash c-hash (nc <mu-contact-with-stats>))
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
