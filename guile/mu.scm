;; Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
  :use-module (ice-9 optargs)
  :use-module (texinfo string-utils)
  :export
  ( ;; classes
    <mu:message>
    <mu:contact>
    <mu:part>
    ;; general
;;    mu:initialize
 ;;   mu:initialized?
    mu:log-warning
    mu:log-message
    mu:log-critical
    ;; search funcs
    mu:for-each-message
    mu:for-each-msg
    mu:message-list
    ;; message funcs
    mu:header
    ;; message accessors
    mu:field:bcc
    mu:field:body
    mu:field:cc
    mu:field:date
    mu:field:flags
    mu:field:from
    mu:field:maildir
    mu:field:message-id
    mu:field:path
    mu:field:prio
    mu:field:refs
    mu:field:size
    mu:field:subject
    mu:field:tags
    mu:field:timestamp
    mu:field:to
    ;; deprecated message accessors
    mu:body-html
    mu:body-txt
    ;; contact funcs
    mu:name
    mu:email
    mu:contact->string
    ;;
    mu:for-each-contact
    ;;
    mu:contacts
    ;;
    ;; <mu:contact-with-stats>
    mu:frequency
    mu:last-seen
    ;; parts

    <mu:part>
    ;; message function
    mu:attachments
    mu:parts
    ;; <mu:part> methods
    mu:name
    mu:mime-type
    ;;	    size
    ;; mu:save
    ;; mu:save-as
    ))

;; this is needed for guile < 2.0.4
(setlocale LC_ALL "")

;; load the binary
(load-extension "libguile-mu" "mu_guile_init")
(load-extension "libguile-mu" "mu_guile_message_init")

;; define some dummies so we don't get errors during byte compilation
(eval-when (compile)
  (define mu:c:get-field)
  (define mu:c:get-contacts)
  (define mu:c:for-each-message)
  (define mu:c:get-header)
  (define mu:critical)
  (define mu:c:log)
  (define mu:message)
  (define mu:c:log)
  (define mu:warning)
  (define mu:c:log)
  (define mu:c:get-parts))

(define (mu:log-warning frm . args)
  "Log FRM with ARGS at warning."
  (mu:c:log mu:warning frm args))

(define (mu:log-message frm . args)
  "Log FRM with ARGS at warning."
  (mu:c:log mu:message frm args))

(define (mu:log-critical frm . args)
  "Log FRM with ARGS at warning."
  (mu:c:log mu:critical frm args))

(define-class <mu:message> ()
  (msg  #:init-keyword #:msg)) ;; the MuMsg-smob we're wrapping

(define-syntax define-getter
  (syntax-rules ()
    ((define-getter method-name field)
      (begin
	(define-method (method-name (msg <mu:message>))
	  (mu:c:get-field (slot-ref msg 'msg) field))
	(export method-name)))))

(define-getter mu:bcc	     mu:field:bcc)
(define-getter mu:body	     mu:field:body)
(define-getter mu:cc	     mu:field:cc)
(define-getter mu:date	     mu:field:date)
(define-getter mu:flags	     mu:field:flags)
(define-getter mu:from	     mu:field:from)
(define-getter mu:maildir    mu:field:maildir)
(define-getter mu:message-id mu:field:message-id)
(define-getter mu:path	     mu:field:path)
(define-getter mu:priority   mu:field:prio)
(define-getter mu:references mu:field:refs)
(define-getter mu:size	     mu:field:size)
(define-getter mu:subject    mu:field:subject)
(define-getter mu:tags	     mu:field:tags)
(define-getter mu:timestamp  mu:field:timestamp)
(define-getter mu:to	     mu:field:to)

(define-method (mu:body-html (msg <mu:message>))
  "The HTML body isn't stored separately anymore, so this method can't return
anything useful. We keep it for backwards compatibility."
  #f)

(define-method (mu:body-txt (msg <mu:message>))
  "The text body isn't stored separately anymore. This method is now a synonym
for mu:body."
  (mu:body msg))

(define-method (mu:header (msg <mu:message>) (hdr <string>))
  "Get an arbitrary header HDR from message MSG; return #f if it does
not exist."
  (mu:c:get-header (slot-ref msg 'msg) hdr))

(define* (mu:for-each-message func #:optional (expr #t) (maxresults -1))
  "Execute function FUNC for each message that matches mu search expression EXPR.
If EXPR is not provided, match /all/ messages in the store. MAXRESULTS
specifies the maximum of messages to return, or -1 (the default) for
no limit."
  (mu:c:for-each-message
    (lambda (msg)
      (func (make <mu:message> #:msg msg)))
    expr
    maxresults))

;; backward-compatibility alias
(define mu:for-each-msg mu:for-each-message)

(define* (mu:message-list #:optional (expr #t) (maxresults -1))
  "Return a list of all messages matching mu search expression
EXPR. If EXPR is not provided, return a list of /all/ messages in the
store. MAXRESULTS specifies the maximum of messages to return, or
-1 (the default) for no limit."
  (let ((lst '()))
    (mu:for-each-message
      (lambda (m)
	(set! lst (append! lst (list m)))) expr maxresults)
    lst))

;; contacts
(define-class <mu:contact> ()
  (name #:init-value #f  #:accessor mu:name  #:init-keyword #:name)
  (email #:init-value #f #:accessor mu:email #:init-keyword #:email))

(define-method (mu:contacts (msg <mu:message>) contact-type)
  "Get all contacts for MSG of the given CONTACT-TYPE. MSG is of type <mu-message>,
while contact type is either `mu:contact:to', `mu:contact:cc',
`mu:contact:from' or `mu:contact:bcc' to get the corresponding type of
contacts, or #t to get all.

Returns a list of <mu-contact> objects."
  (map (lambda (pair) ;; a pair (na . addr)
	 (make <mu:contact>  #:name (car pair) #:email (cdr pair)))
    (mu:c:get-contacts (slot-ref msg 'msg) contact-type)))

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
\"wanderlust\", \"quoted\" \"plain\"."
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
      ((string= form  "quoted")
	  (string-append
	    "\""
	    (escape-special-chars
	      (string-append
		(if name
		  (format #f "\"~a\" " name)
		  "")
		(format #f "<~a>" email))
	      "\"" #\\)
	      "\""))
      (else (error "Unsupported format")))))

;; message parts


(define-class <mu:part> ()
  (msgpath   #:init-value #f #:init-keyword #:msgpath)
  (index     #:init-value #f #:init-keyword #:index)
  (name      #:init-value #f #:getter mu:name #:init-keyword #:name)
  (mime-type #:init-value #f #:getter mu:mime-type #:init-keyword #:mime-type)
  (size      #:init-value 0  #:getter mu:size #:init-keyword #:size))

(define-method (get-parts (msg <mu:message>) (files-only <boolean>))
    "Get the part for MSG as a list of <mu:part> objects; if FILES-ONLY is #t,
only get the part with file names."
  (map (lambda (part)
	 (make <mu:part>
	   #:msgpath    (list-ref part 0)
	   #:index      (list-ref part 1)
	   #:name       (list-ref part 2)
	   #:mime-type  (list-ref part 3)
	   #:size       (list-ref part 4)))
    (mu:c:get-parts (slot-ref msg 'msg) files-only)))

(define-method (mu:attachments (msg <mu:message>))
  "Get the attachments for MSG as a list of <mu:part> objects."
  (get-parts msg #t))

(define-method (mu:parts (msg <mu:message>))
  "Get the MIME-parts for MSG as a list of <mu-part> objects."
  (get-parts msg #f))

;; (define-method (mu:save (part <mu:part>))
;;   "Save PART to a temporary file, and return the file name. If the
;; part had a filename, the temporary file's file name will be just that;
;; otherwise a name is made up."
;;   (mu:save-part (slot-ref part 'msgpath) (slot-ref part 'index)))

;; (define-method (mu:save-as (part <mu:part>) (filepath <string>))
;;   "Save message-part PART to file system path PATH."
;;     (copy-file (save part) filepath))
