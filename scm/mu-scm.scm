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
  :use-module (rnrs bytevectors)
  :use-module (ice-9 optargs)
  :use-module (ice-9 format)
  :use-module (ice-9 binary-ports)
  #:export (
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Mime-parts
	    <mime-part>
	    mime-part->alist
	    make-port
	    filename
	    write-to-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Message
	    <message>
	    make-message

	    message->alist

	    date
	    changed

	    message-id
	    path
	    priority
	    subject
	    labels

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

	    mime-parts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Store
	    <store>
	    mfind
	    mcount
	    cfind
	    labels
	    personal?
	    store->alist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Other

	    ;; misc
	    %options
	    ;;	    %preferences

	    ;; logging
	    debug
	    info
	    warning
	    critical

	    ;; helpers
	    string->time
	    time->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpers

(define (set-documentation! symbol docstring)
  "Set the docstring for symbol in current module to docstring.
This is useful for symbols that do not support docstrings directly, such
as (define foo 123) and, apparently, define-method."
  ;; https://git.wolfsden.cz/guile-wolfsden/tree/wolfsden/documentation.scm
  (set-object-property! (module-ref (current-module) symbol)
			'documentation docstring))

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

(define (emacs-time->epoch-secs lst)
  "Convert emacs-style timestamp LST to a number of seconds since epoch.
If LST is #f, return #f."
  (if lst
      (+ (ash (car lst) 16) (cadr lst))
      #f))

(define (plist->alist plist)
  "Convert a plist into an alist.
This is specific for message plists."
  (let ((alist '()))
    (plist-for-each
     (lambda (k v)
       (let ((key (decolonize-symbol k)))
	 (set! alist
	       (append! alist
			(list (cons key
				    (cond
				     ((member key '(from to cc bcc))
				      (map plist->alist v))
				     ((member key '(date changed))
				      (emacs-time->epoch-secs v))
				     (else v))))))))
     plist)
    alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIME-parts
(define-class <mime-part> ()
  (cc-mimepart  #:init-value #f #:init-keyword #:mimepart #:getter cc-mimepart)
  (alist        #:init-value #f #:init-keyword #:alist #:getter mime-part->alist))

(set-documentation!
 '<mime-part>
 "A <mime-part> represents the information about a message's MIME-part.
It has a few slots:
  - cc-mimepart: a 'foreign object' wrapping a GMimePart*
  - alist: the association-list representation of the MIME-part.")

(define* (make-port mime-part #:key (content-only? #t) (decode? #t))
  "Create a read port for MIME-PART.
If CONTENT-ONLY? is #t, only include the contents, not headers.
If DECODE? is #t, decode the content (from e.g., base64); in that case,
CONTENT-ONLY? is implied to be #t."
  (cc-mime-make-stream-port (cc-mimepart mime-part) content-only? decode?))

(define-method (filename (mime-part <mime-part>))
  (let ((alist (mime-part->alist mime-part)))
    (or (assoc-ref  alist 'filename)
	(format #f "mime-part-~d" (assoc-ref alist 'index)))))

(set-documentation! 'filename
  "Determine the file-name for MIME-part.
Either the 'filename' field in the mime-part and if that does not exist, use
'mime-part-<index>' with <index> being the number of the mime-part.")


(define* (make-output-file mime-part #:key (path #f) (overwrite? #f))
  "Create a port for the file to write MIME-PART to.

PATH is file-name or path to the file name. If not specified, use the 'filename'
field in the mime-part and if that does not exist, use 'mime-part-<index>' with
<index> being the number of the mime-part.

OVERWRITE? specifies whether existing files by the same name or overwritten.
Otherwise, trying to overwrite an existing file raises an error."
  (let* ((alist (mime-part->alist mime-part))
	 (path (or path (filename mime-part))))
    ;; we need an fd-based port since we want to support overwrite?
    (open path
	  (logior O_WRONLY O_CREAT O_TRUNC (if overwrite? O_EXCL 0)) #o644)))

(define* (write-to-file mime-part #:key (path #f) (overwrite? #f))
  "Write MIME-PART to a file.

PATH is the path/filename for the file. If not specified, use the 'filename'
field in the mime-part and if that does not exist, use 'mime-part-<index>' with
<index> being the number of the mime-part.

OVERWRITE? specifies whether existing files by the same name or overwritten.
Otherwise, trying to overwrite an existing file raises an error."
  (let* ((input (make-port mime-part))
	 (output (make-output-file mime-part
				   #:path path #:overwrite? overwrite?))
	 (buf (make-bytevector 4096)) ;; just a guess...
	 (bytes 0))
    (while (not (eof-object? bytes)) ;; XXX do this in a more elegant way.
      (set! bytes (get-bytevector-n! input buf 0 (bytevector-length buf)))
      (put-bytevector output buf 0 (if (eof-object? bytes) 0 bytes)))
    (close input)
    (close output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message
(define-class <message> ()
  (cc-message #:init-value #f #:init-keyword #:cc-message)
  (serialized #:init-value #f #:init-keyword #:serialized)
  (parts #:init-value #f #:init-keyword #:parts)
  (alist #:init-value #f))

(set-documentation!
 '<message>
 "A <message> represents the information about a message.

Exactly what information depends on how the object came to be;
these are the slots:

- cc-message: this is a foreign-object representing the mu
  message object, and needs to be passed to some 'cc-' methods.
- parts: a list of <mime-part> objects
- data: this is a string containing an Emacs-style property list
  which is cached for each message in the store; this was
  originally added for use in mu4e, but we re-use it here.
- alist: an association list; an alist with properties, as
  created from the data (converted from the plist)

A message that came from a search such as 'mfind' initially only
has the data, but when a message is loaded from file, either
through make-message or by calling a function that needs a
full message, such as header or body, the cc-message is initialized.")

(define-method (make-message (path <string>))
  "Create a <message> from file at PATH."
  (make <message> #:cc-message (cc-message-make path)))

(define-method (message->alist (message <message>))
  "Get an association-list (alist) representation for MESSAGE."
  (when (not (slot-ref message 'alist))
    (let* ((serialized
	    (or (slot-ref message 'serialized)
		(cc-message-plist (slot-ref message 'cc-message))))
	   ;; parse the serialized message (the mu4e plist)
	   ;; and convert into alist. We need to _quote_ the
	   ;; the serialized string before we can parse it.
	   (alist (plist->alist (eval-string (string-append "'" serialized)))))
      (slot-set! message 'alist alist)
      (slot-set! message 'serialized #f))) ;; no longer needed
  (slot-ref message 'alist))

(define-method (cc-message (message <message>))
  "Get the foreign object for this MESSAGE.
If MESSAGE does not have such an object yet, create it from the
path of the message."
  (if (not (slot-ref message 'cc-message))
      (slot-set! message 'cc-message (cc-message-make (path message))))
  (slot-ref message 'cc-message))

;; Accessors for the fields

(define-method (subject (message <message>))
  "Get the subject for MESSAGE or #f if not found."
  (assoc-ref (message->alist message) 'subject))

(define-method (maildir (message <message>))
  "Get the maildir for MESSAGE or #f if not found."
  (assoc-ref (message->alist message) 'maildir))

(define-method (message-id (message <message>))
  "Get the message-id for MESSAGE or #f if not found."
  (assoc-ref (message->alist message) 'message-id))

(define-method (date (message <message>))
  "Get the timestamp for MESSAGE was sent.
This is the number of seconds since epoch; #f if not found."
  (assoc-ref (message->alist message) 'date))

(define-method (changed (message <message>))
  "Get the timestamp for the last change to MESSAGE.
This is the number of seconds since epoch; #f if not found."
  (assoc-ref (message->alist message) 'changed))

(define-method (path (message <message>))
  "Get the file-system path for MESSAGE.
A symbol, either 'high, 'low or 'normal, or #f if not found."
  (assoc-ref (message->alist message) 'path))

(define-method (priority (message <message>))
  "Get the priority for MESSAGE.
A symbol, either 'high, 'low or 'normal, or #f if not found."
  (assoc-ref (message->alist message) 'priority))

(define-method (language (message <message>))
  "Get the ISO-639-1 language code for the MESSAGE as a symbol, if detected.
Return #f otherwise."
  (let ((lang ( (assoc-ref (message->alist message) 'language))))
    (if lang
	(string->symbol lang)
	#f)))
;; if-let would be nice!

(define-method (size (message <message>))
  "Get the size of the MESSAGE in bytes or #f if not available."
  (assoc-ref (message->alist message) 'size))

(define-method (references (message <message>))
  "Get the list of reference of MESSAGE or #f if not available.
 with the oldest first and the direct parent as the last one. Note, any
reference (message-id) will appear at most once, duplicates and
fake-message-id (see impls) are filtered out. If there are no references, return
#f."
  (assoc-ref (message->alist message) 'references))

(define-method (labels (message <message>))
  "Get the list of labels for MESSAGE or #f if not available."
  (assoc-ref (message->alist message) 'labels))

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
  (assoc-ref (message->alist message) 'list))

;;  Flags.

(define-method (flags (message <message>))
  "Get the size of the MESSAGE in bytes or #f if not available."
  (assoc-ref (message->alist message) 'flags))

(define-method (flag? (message <message>) flag)
  "Does MESSAGE have some FLAG?"
  (let ((flgs (flags message)))
    (if flgs
	(if (member flag flgs) #t #f)
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

(define-method (from (message <message>))
  "Get the sender (the From: field) for MESSAGE or #f if not found."
  (assoc-ref (message->alist message) 'from))

(define-method (to (message <message>))
  "Get the (intended) recipient for MESSAGE (the To: field) or #f if not found."
  (assoc-ref (message->alist message) 'to))

(define-method (cc (message <message>))
  "Get the (intended) carbon-copy recipient for MESSAGE (the Cc: field) or #f if
not found."
  (assoc-ref (message->alist message) 'cc))

(define-method (bcc (message <message>))
  "Get the (intended) blind carbon-copy recipient for MESSAGE (the Bcc: field) or
#f if not found."
  (assoc-ref (message->alist message) 'bcc))

(define* (body message #:key (html? #f))
  "Get the MESSAGE body or #f if not found
If #:html is non-#f, instead search for the HTML body.
Requires the full message."
  (cc-message-body (cc-message message) html?))

(define-method (header (message <message>) (field <string>))
  "Get the raw MESSAGE header FIELD or #f if not found.
FIELD is case-insensitive and should not have the ':' suffix.
Requires the full message."
  (cc-message-header (cc-message message) field))

(define-method (mime-parts (message <message>))
  "Get the MIME-parts for this message.
This is a list of <mime-part> objects."
  (map (lambda (mimepart-alist)
	 (make <mime-part>
	   #:mimepart (car mimepart-alist)
	   #:alist (cdr mimepart-alist)))
       (cc-message-parts (cc-message message))))

;; Store
;;
;; Note: we have a %default-store, which is the store we opened during startup;
;; for now that's the only store supported, but we keep things open.
;;
;; Since it's the default store, we'd like to call the methods without
;; explicitly using %default-store; with GOOPS, we cannot pass a default for
;; that, nor can we use keyword arguments (I think?). So use define* for that.
(define-class <store> ()
  (cc-store #:init-keyword #:cc-store #:getter cc-store)
  (alist #:init-value #f))

(set-documentation!
 '<store>
 "A <store> represents mu's message store (database).

It has a few slots:
- cc-store: this is a foreign-object for a Mu::Store*.
  It needs to be passed to some 'cc-' methods.
- alist: an association list; this is the cached representation
  of some store properties.")

;;  not exported
(define-method (make-store store-obj)
  "Make a store from some STORE-OBJ.
STORE-OBJ a 'foreign-object' for a mu Store pointer."
  (make <store> #:cc-store store-obj))

(define %default-store
  (make-store %cc-default-store))

(set-documentation! '%default-store  "Default store.")

(set-documentation! '%cc-default-store
 "Default store object.
This is defined in the C++ code, and represents a \"foreign\" Store* object.")

(define-method (store->alist (store <store>))
  "Get an alist-representation for some STORE."
  (when (not (slot-ref store 'alist))
    (slot-set! store 'alist (cc-store-alist (cc-store store))))
  (slot-ref store 'alist))

(define-method (store->alist)
  "Get an alist-representation from the default store."
  (store->alist %default-store))

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
    (map (lambda (data)
	   (make <message> #:serialized data))
	 (cc-store-mfind (cc-store store) query
			 related? skip-dups? sort-field
			 reverse? max-results)))

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
  (cc-store-cfind (cc-store store) pattern personal? after max-results))

(define-method (mcount (store <store>))
 ;; "Get the number of messages in STORE."
  (cc-store-mcount (cc-store store)))

(define-method (mcount)
  "Get the number of messages in the default store."
  (mcount %default-store))

(define-method (personal? (store <store>) (address <string>))
  "Does the given email ADDRESS match the personal addresses in STORE?
I.e., the personal addresses / regular expressions as specified during `mu
init'."
  (cc-store-is-personal (cc-store store) address))

(define-method (personal? (address <string>))
  "Does the given email ADDRESS match the personal addresses?
I.e., the personal addresses / regular expressions as specified during `mu
init'. Uses the default-store."
  (personal? %default-store address))

(define-method (labels (store <store>))
  "Get the list of all labels in STORE."
  (cc-store-all-labels (cc-store store)))

(define-method (labels)
  "Get the list of all labels in the default store."
  (labels %default-store))

;;; Misc

;; Get an alist with the general options this instance of \"mu\" started with.
;; These are based on the command-line arguments, environment etc., see the
;; mu-scm(1) manpage for details.
;;
;; The alist maps symbols to values; a value of #f indicates that the value is at
;; its default.
%options ;; defined in c++
(set-documentation! '%options
		    "Alist with the command-line parameters.")

(define %preferences
  '( (short-date  . "%F %T")
     (utc?        . #f)))
;; XXX; not exposed yet. Perhaps we need a "fluid" here?
(set-documentation! '%preferences
  "Alist with user-preferences.
- short-date: a strftime-compatibie string for the display
	      format of short dates.
- utc?       : whether to assume use UTC for dates/times")

(define (value-or-preference val key)
  "If VAL is the symbol 'preference, return the value for KEY from %preferences.
Otherwise, return VAL."
  (if (eq? val 'preference)
      (assoc-ref %preferences key)
      val))

;;; Helpers

(define* (string->time datestr #:key (utc? 'preference))
  "Convert an ISO-8601-style DATESTR to a number of seconds since epoch.
(like time_t, (current-time).

ISODATE is a string with the strftime format \"%FT%T\", i.e.,
yyyy-mm-ddThh:mm:ss or any prefix there of. The 'T', ':', '-' or any non-numeric
characters re optional.

UTC? determines whether ISODATE should be interpreted as an UTC time.

The input date format is fixed."
 ;; XXX If not set, read the default from the %preferences variable.
  (let* ((utc? (value-or-preference utc? 'utc?))
	 (tmpl "00000101000000")
	 (datestr (string-filter char-numeric? datestr)) ;; filter out 'T' ':' '-' etc
	 (datestr					 ;; fill out datestr
	  (if (> (string-length tmpl) (string-length datestr))
	      (string-append datestr (substring tmpl (string-length datestr)))
	      datestr))
	 (sbdtime (car (strptime "%Y%m%d%H%M%S" datestr))))
    (car (if utc?
	     (mktime sbdtime "UTC")
	     (mktime sbdtime)))))

(define* (time->string time-t #:key (format 'preference) (utc? 'preference))
  "Convert a time_t (second-since-epoch) value TIME-T into a string.

FORMAT is the strftime-compatible format-string UTC? determines whether to use
UTC time.

 If TIME-T is #f, return #f."
;; If not specified, both are determined from %preferences ('short-date
;; and 'utc?, respectively).
  (let* ((format (value-or-preference format 'short-date))
	 (utc? (value-or-preference utc? 'utc?)))
    (if time-t
	(let ((t (if utc? (gmtime time-t) (localtime time-t))))
	  (strftime format t))
	#f)))


;; Logging to mu's log

(define (mlog level frmstr . args)
  "Log to the mu logger.

LEVEL is the logging-level, a symbol one of:
  debug, info, warning, critical

FRMSTR is a format string like `(format)', and the ARGS are the parameters for
that format string."
  (let ((msg (if (null? args)
		 frmstr
		 (apply format #f frmstr args))))
    (cc-log level msg)))

(define (info frm . args)
  "Log a message at info level to the mu logger.
FRM is the format string and ARGS are its arguments, see `(format)' for details
on the precise format."
  (apply mlog level-info frm args))

(define (warning frm . args)
  "Log a message at warning level to the mu logger.
FRM is the format string and ARGS are its arguments, see `(format)' for details
on the precise format."
  (apply mlog level-warning frm args))

(define (critical frm . args)
  "Log a message at critical level to the mu logger.
FRM is the format string and ARGS are its arguments, see `(format)' for details
on the precise format."
  (apply mlog level-critical frm args))

(define (debug frm . args)
  "Log a message at debug level to the mu logger.
FRM is the format string and ARGS are its arguments, see `(format)' for details
on the precise format."
  (apply mlog level-debug frm args))
