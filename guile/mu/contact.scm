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
	    <mu:contact>
	    mu:name
	    mu:email
	    mu:contact->string
	    ;;
	    mu:for-each-contact
	    ;;
	    mu:contacts
	    ;;
	    <mu:contact-with-stats>
	    mu:frequency
	    mu:last-seen
	    ))

(define-class <mu:contact> ()
  (name #:init-value #f  #:accessor mu:name  #:init-keyword #:name)
  (email #:init-value #f #:accessor mu:email #:init-keyword #:email))

(define-method (mu:contacts (msg <mu:message>) contact-type)
  "Get all contacts for MSG of the given CONTACT-TYPE. MSG is of type <mu-message>,
while contact type is either `mu:to', `mu:cc', `mu:from' or `mu:bcc'
to get the corresponding type of contacts, or #t to get all. Returns a
list of <mu-contact> objects."
  (map (lambda (pair) ;; a pair (na . addr)
	 (make <mu:contact>  #:name (car pair) #:email (cdr pair)))
    (mu:get-contacts (slot-ref msg 'msg) contact-type)))

(define-method (mu:contacts (msg <mu:message>))
  "Get contacts of all types for message MSG as a list of <mu-contact>
objects."
  (mu:contacts msg #t))

(define-class <mu:contact-with-stats> (<mu:contact>)
  (tstamp #:init-value 0 #:accessor mu:timestamp #:init-keyword #:timestamp)
  (last-seen #:init-value 0 #:accessor mu:last-seen)
  (freq #:init-value 1 #:accessor mu:frequency))

(define* (mu:for-each-contact proc #:optional (expr #t))
  "Execute PROC for each contact. PROC receives a <mu-contact> instance
as parameter. If EXPR is specified, only consider contacts in messages
matching EXPR."
  (let ((c-hash (make-hash-table 4096)))
    (mu:for-each-message
      (lambda (msg)
	(for-each
	  (lambda (ct)
	    (let ((ct-ws (make <mu:contact-with-stats>
			   #:name      (mu:name  ct)
			   #:email     (mu:email ct)
			   #:timestamp (mu:date msg))))
	      (update-contacts-hash c-hash ct-ws)))
	  (mu:contacts msg #t)))
      expr)
    (hash-for-each ;; c-hash now contains a map of email->contact
      (lambda (email ct-ws) (proc ct-ws)) c-hash)))

(define-method (update-contacts-hash c-hash (nc <mu:contact-with-stats>))
  "Update the contacts hash with a new and/or existing contact."
  ;; xc: existing-contact, nc: new contact
  (let ((xc (hash-ref c-hash (mu:email nc))))
    (if (not xc) ;; no existing contact with this email address?
      (hash-set! c-hash (mu:email nc) nc) ;; store the new contact.
      ;; otherwise:
      (begin
	;; 1) update the frequency for the existing contact
	(set! (mu:frequency xc) (1+ (mu:frequency xc)))
	;; 2) update the name if the new one is not empty and its timestamp is newer
	;;    in that case, also update the timestamp
	(if (and (mu:name nc) (> (string-length (mu:name nc)))
	      (> (mu:timestamp nc) (mu:timestamp xc)))
	  (set! (mu:name xc) (mu:name nc))
	  (set! (mu:timestamp xc) (mu:timestamp nc)))
	;; 3) update last-seen with timestamp, if x's timestamp is newer
	(if (> (mu:timestamp nc) (mu:last-seen xc))
	  (set! (mu:last-seen xc) (mu:timestamp nc)))
	;; okay --> now xc has been updated; but it back in the hash
	(hash-set! c-hash (mu:email xc) xc)))))

(define-method (mu:contact->string (contact <mu:contact>) (form <string>))
  "Convert a contact to a string in format FORM, which is a string,
either \"org-contact\", \"mutt-alias\", \"mutt-ab\",
\"wanderlust\" \"plain\"."
  (let* ((name (mu:name contact)) (email (mu:email contact))
	  (nick ;; simplistic nick guessing...
	    (string-map
	      (lambda(kar)
		(if (char-alphabetic? kar) kar #\_))
	      (string-downcase (or name email)))))
    (cond
      ((string= form "plain")
	(format #f "~a~a~a" (or name "") (if name " " "") email))
      ((string= form "org-contact")
	(format #f "* ~s\n:PROPERTIES:\n:EMAIL:~a\n:NICK:~a\n:END:"
	  (or name email) email nick))
      ((string= form "wanderlust")
	(format #f "~a ~s ~s"
	  nick (or name email) email))
      ((string= form "mutt-alias")
	(format #f "alias ~a ~a <~a>"
	  nick (or name email) email))
      ((string= form "mutt-ab")
	(format #f "~a\t~a\t"
	 email (or name "")))
      (else (error "Unsupported format")))))
