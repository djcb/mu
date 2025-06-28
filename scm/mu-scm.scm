;; Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

;; Note: this Scheme code depends on being loaded as part of "mu scm"
;; which does so automatically. It is not a general Guile module.

(define-module (mu)
  :use-module (oop goops)
  :use-module (system foreign)
  :use-module (ice-9 optargs)
  #:export (
  ;; classes
  <store>

  mfind
  mcount
  cfind

  <message>
  sexp

  date
  last-change

  message-id
  path
  priority
  subject

  references
  thread-id

  mailing-list

  language
  size

  ;; message flags / predicates
  flags
  flag?
  draft?
  flagged?
  passed?
  replied?
  seen?
  trashed?
  new?
  signed?
  encrypted?
  attach?
  unread?
  list?
  personal?
  calendar?

  ;; contact fields
  from
  to
  cc
  bcc

  ;; message-body
  body
  header

  ;; misc
  options

  ;; helpers
  iso-date->time-t
  time-t->iso-date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some helpers for dealing with plists / alists
(define (plist-for-each func plist)
  "Call FUNC for each key/value in the PLIST.
PLIST is a property-list with alternating key and value.
Stops when FUNC returns #f."
  (when (and (not (null? plist))
	     (func (car plist) (cadr plist)))
    (plist-for-each func (cddr plist))))

(define (plist-find plist key)
  "Find the value for the first occurrence of KEY in PLIST.
If not found, return #f."
  (let ((val #f))
    (plist-for-each
     (lambda (k v)
       (if (eq? k key)
	   (begin
	     (set! val v) #f)
	   #t))
     plist)
    val))

(define (decolonize-symbol sym)
  "Remove :-prefix from symbol."
  (let ((name (symbol->string sym)))
    (if (string-prefix? ":" name)
	(string->symbol (string-drop name 1))
	sym)))

(define (plist->alist plist)
  "Convert a plist into an alist."
  (let ((alist '()))
    (plist-for-each
     (lambda (k v)
       (set! alist
	     (append! alist
		      (list (cons (decolonize-symbol k)
				  v)))))
     plist)
    alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Message
;;
;; A <message> has two slots:
;; plist -->      this is the message sexp cached in the database;
;;                for each message (for mu4e, but we reuse here)
;; object-->      wraps a Mu::Message* as a "foreign object"
;;
;; generally the plist is a bit cheaper, since the mu-message
;; captures a file-deescriptor.

(define-class <message> ()
  (plist #:init-value #f #:init-keyword #:plist)
  (object #:init-value #f #:init-keyword #:object))

(define-method (plist (message <message>))
  "Get the PLIST for this MESSAGE."
  (slot-ref message 'plist))

(define-method (object (message <message>))
  "Get the foreign object for this MESSAGE.
If MESSAGE does not have such an object yet, crate it from the
path of the message."
  (if (not (slot-ref message 'object))
      (slot-set! message 'object
		 (message-object-make (path message))))
  (slot-ref message 'object))

(define-method (find-field (message <message>) field)
  (plist-find (plist message) field))

(define-method (sexp (message <message>))
  "Get the s-expression (plist) for this MESSAGE.

This is an internal data-structure, originally for use with mu4e, but useful
here as well. However, the precise details are not part of mu-scm API."
  (plist message))

(define (emacs-time->epoch-secs lst)
  "Convert emacs-style timestamp LST to a number of seconds since epoch.
If LST is #f, return #f."
  (if lst
      (+ (ash (car lst) 16) (cadr lst))
      #f))

;; Accessor for the fields

(define-method (subject (message <message>))
  "Get the subject for MESSAGE or #f if not found."
  (find-field message ':subject))

(define-method (maildir (message <message>))
  "Get the maildir for MESSAGE or #f if not found."
  (find-field message ':maildir))

(define-method (message-id (message <message>))
  "Get the message-id for MESSAGE or #f if not found."
  (find-field message ':message-id))

(define-method (date (message <message>))
  "Get the date for MESSAGE was sent.
This is the number of seconds since epoch; #f if not found."
  (emacs-time->epoch-secs (find-field message ':date)))

(define-method (last-change (message <message>))
  "Get the date for the last change to MESSAGE.
This is the number of seconds since epoch; #f if not found."
  (emacs-time->epoch-secs (find-field message ':changed)))

(define-method (path (message <message>))
  "Get the file-system path for MESSAGE.
A symbol, either 'high, 'low or 'normal, or #f if not found."
  (find-field message ':path))

(define-method (priority (message <message>))
  "Get the priority for MESSAGE.
A symbol, either 'high, 'low or 'normal, or #f if not found."
  (find-field message ':priority))

(define-method (language (message <message>))
  "Get the ISO-639-1 language code for the MESSAGE as a symbol, if detected.
Return #f otherwise."
  (let ((lang (find-field message ':language)))
    (if lang
	(string->symbol lang)
	#f)))
;; if-let would be nice!

(define-method (size (message <message>))
  "Get the size of the MESSAGE in bytes or #f if not available."
  (find-field message ':size))

(define-method (references (message <message>))
  "Get the list of reference of MESSAGE or #f if not available.
 with the oldest first and the direct parent as the last one. Note, any
reference (message-id) will appear at most once, duplicates and
fake-message-id (see impls) are filtered out. If there are no references, return
#f."
  (find-field message ':references))

(define-method (thread-id (message <message>))
  "Get the oldest (first) reference for MESSAGE, or message-id if there are none.
If neither are available, return #f.
This is method is useful to determine the thread a message is in."
  (let ((refs (references message)))
    (if (and refs (not (null? refs)))
	(car refs)
	(message-id message))))

(define-method (mailing-list (message <message>))
  "Get the mailing-list id for MESSAGE or #f if not available."
  (find-field message ':list))

;;  Flags.

(define-method (flags (message <message>))
  "Get the size of the MESSAGE in bytes or #f if not available."
  (find-field message ':flags))

(define-method (flag? (message <message>) flag)
  "Does MESSAGE have FLAG?"
  (let ((flags
	 (find-field message ':flags)))
    (if flags
	(if (member flag flags) #t #f)
	#f)))

(define-method (draft? (message <message>))
  "Is MESSAGE a draft message?"
  (flag? message 'draft))

(define-method (flagged? (message <message>))
  "Is MESSAGE flagged?"
  (flag? message 'flagged))

(define-method (passed? (message <message>))
  "Has MESSAGE message been 'passed' (forwarded)?"
  (flag? message 'passed))

(define-method (replied? (message <message>))
  "Has MESSAGE been replied to?"
  (flag? message 'replied))

(define-method (seen? (message <message>))
  "Does MESSAGE been 'seen' (read)?"
  (flag? message 'seen))

(define-method (trashed? (message <message>))
  "Has MESSAGE been trashed?"
  (flag? message 'trashed))

(define-method (new? (message <message>))
  "Is MESSAGE new?"
  (flag? message 'new))

(define-method (signed? (message <message>))
  "Has MESSAGE been cryptographically signed?"
  (flag? message 'signed))

(define-method (encrypted? (message <message>))
  "Has MESSAGE been encrypted?"
  (flag? message 'encrypted))

(define-method (attach? (message <message>))
  "Does MESSAGE have an attachment?"
  (flag? message 'attach))

(define-method (unread? (message <message>))
  "Is MESSAGE unread?"
  (flag? message 'unread))

(define-method (list? (message <message>))
  "Is MESSAGE from some mailing-list?"
  (flag? message 'list))

(define-method (personal? (message <message>))
  "Is MESSAGE personal?"
  (flag? message 'personal))

(define-method (calendar? (message <message>))
  "Does MESSAGE have a calender invitation?"
  (flag? message 'calendar))

(define-method (find-contact-field (message <message>) field)
  "Get contact FIELD from MESSAGE as an alist.
Helper method "
  (let ((cs (find-field message field)))
    (if cs
	(map plist->alist cs)
	#f)))

(define-method (from (message <message>))
  "Get the sender (the From: field) for MESSAGE or #f if not found."
  (find-contact-field message ':from))

(define-method (to (message <message>))
  "Get the (intended) recipient for MESSAGE (the To: field) or #f if not found."
  (find-contact-field message ':to))

(define-method (cc (message <message>))
  "Get the (intended) carbon-copy recipient for MESSAGE (the Cc: field) or #f if
not found."
  (find-contact-field message ':cc))

(define-method (bcc (message <message>))
  "Get the (intended) blind carbon-copy recipient for MESSAGE (the Bcc: field) or
#f if not found."
  (find-contact-field message ':bcc))

(define* (body message #:key (html? #f))
  "Get the MESSAGE body or #f if not found.
If #:html is non-#f, instead search for the HTML body.
Requires the full message."
  (message-body (object message) html?))

(define-method (header (message <message>) (field <string>))
  "Get the raw MESSAGE header FIELD or #f if not found.
FIELD is case-insensitive and should not have the ':' suffix.
Requires the full message."
  (message-header (object message) field))

;; Store
;;
;; Note: we have a %default-store, which is the store we opened during
;; startup; for now that's the only store supported, but we keep things
;; open.
;;
;; Since it's the default store, we'd like to call the methods without
;; explicitly using %default-store; with GOOPS, we cannot pass a default for
;; that, nor can we use keyword arguments (I think?). So use define* for that.

;; the 'store-object' is a foreign object wrapping a const Store*.
(define-class <store> ()
  (store-object #:init-keyword #:store-object #:getter store-object))

;;  not exported
(define-method (make-store store-object)
  "Make a store from some STORE-OBJECT."
  (make <store> #:store-object store-object))

(define %default-store
  ;; %default-store-object is defined in mu-scm-store.cc
  (make-store %default-store-object))

(define* (mfind query
	       #:key
	       (store %default-store)
	       (related? #f)
	       (skip-dups? #f)
	       (sort-field 'date)
	       (reverse? #f)
	       (max-results #f))
   "Find messages matching some query.

The query is mandatory, the other (keyword) arguments are optional.
(mfind QUERY
    #:store       %default-store. Leave at default.
    #:related?    include related messages?  Default: false
    #:skip-dups?  skip duplicates? Default: false
    #:sort-field? field to sort by, a symbol. Default: date
    #:reverse?    sort in descending order (z-a)
    #:max-results max. number of matches. Default: false (unlimited))."
   (map (lambda (plist)
	  (make <message> #:plist plist))
	(store-mfind (store-object store) query
		     related? skip-dups? sort-field reverse? max-results)))

(define* (mcount
	  #:key
	  (store %default-store))
  "Get the number of messages."
  (store-mcount (store-object store)))

(define* (cfind pattern
		#:key
		(store %default-store)
		(personal? #f)
		(after #f)
		(max-results #f))
  "Find contacts matching some regex pattern, similar to mu-cfind(1).

The pattern is mandatory; the other (keyword) arguments are optional.
(cfind PATTERN
    #:store       %default-store. Leave at default.
    #:personal?   only include 'personal' contacts. Default: all
    #:after       only include contacts last seen time_t: Default all
    #:max-results max. number of matches. Default: false (unlimited))."
  (store-cfind (store-object store) pattern personal? after max-results))

;;; Misc

(define (options)
  "Get an alist with the general options this instance of \"mu\" started with.
These are based on the command-line arguments, environment etc., see
the mu-scm(1) manpage for details.

The alist maps symbols to values; a value of #f indicates that the value
is at its default."
  %options)

;;; Helpers

(define* (iso-date->time-t isodate)
  "Convert an ISO-8601 ISODATE to a number of seconds since epoch.

ISODATE is a string with the strftime format \"%FT%T\", i.e.,
yyyy-mm-ddThh:mm:ss or any prefix there of. The 'T', ':', '-' or any non-numeric
characters re optional.

ISODATE is assumed to represent some UTC date."
  (let* ((tmpl "00000101000000")
	 (isodate (string-filter char-numeric? isodate)) ;; filter out 'T' ':' '-' etc
	 (isodate					 ;; fill out isodate
	  (if (> (string-length tmpl) (string-length isodate))
	      (string-append isodate (substring tmpl (string-length isodate)))
	      isodate)))
    ;;(format #t "~a\n" isodate)
    (car (mktime (car (strptime "%Y%m%d%H%M%S" isodate)) "Z"))))

(define-method (time-t->iso-date time-t)
  "Convert a time_t (second-since-epoch) value TIME-T to an ISO-8601
string for the corresponding UTC time.

If TIME-T is #f, return an empty string of the same length."
  (if time-t
      (strftime "%FT%T" (gmtime time-t))
      "                   "))
