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
  :use-module (mu message)
  :use-module (mu contact)
  :export
  (mu:for-each-contact
   mu:for-each-message
   mu:message-list
   mu:tabulate-messages
   mu:average-messages))

(load-extension "libguile-mu" "mu_guile_init")

(define* (mu:for-each-contact proc #:optional (expr #t))
  "Execute PROC for each contact. PROC receives a <mu-contact> instance
as parameter. If EXPR is specified, only consider contacts in messages
matching EXPR."
  (let ((c-hash (make-hash-table 4096)))
    (mu:for-each-message
      (lambda (msg)
	(for-each
	  (lambda (name-addr)
	    (let ((contact (make <mu-contact>
			     #:name      (car name-addr)
			     #:email     (cdr name-addr)
			     #:timestamp (date msg))))
	      (update-contacts-hash c-hash contact)))
	  (contacts msg #t)))
      expr)
    ;; c-hash now contains a map of email->contact
    (hash-for-each
      (lambda (email contact) (proc contact)) c-hash)))

(define-method (update-contacts-hash c-hash (nc <mu-contact>))
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

(define* (mu:for-each-message func #:optional (expr #t))
  "Execute function FUNC for each message that matches mu search expression EXPR.
If EXPR is not provided, match /all/ messages in the store."
  (let ((my-func
	  (lambda (msg)
	    (func (make <mu-message> #:msg msg)))))
    (mu:internal:for-each-message my-func expr)))

(define* (mu:message-list #:optional (expr #t))
  "Return a list of all messages matching mu search expression
EXPR. If EXPR is not provided, return a list of /all/ messages in the store."
  (let ((lst '()))
    (mu:for-each-message
      (lambda (m)
	(set! lst (append! lst (list m)))) expr)
    lst))

(define* (mu:tabulate-messages func #:optional (expr #t))
  "Execute FUNC for each message matching EXPR, and return an alist
with maps each result of FUNC to its frequency. FUNC is a function
takes a <mu-message> instance as its argument. For example, to tabulate messages by weekday,
one could use:
   (mu:tabulate-messages (lambda(msg) (tm:wday (localtime (date msg)))))
."
  (let ((table '()))
    (mu:for-each-message
      (lambda(msg)
	(let* ((val (func msg))
		(old-freq (or (assoc-ref table val) 0)))
	  (set! table (assoc-set! table val (1+ old-freq)))))
      expr)
    table))


(define* (mu:average-messages func #:optional (expr #t))
  "Execute FUNC for each message matching EXPR, and return the average value of the results of FUNC.
 FUNC is a function that takes a <mu-message> instance as its
argument, and returns some number. For example, to get the average message size of messages related to icecream:
   (mu:average (lambda(msg) (size msg)) \"icecream\" ."
(let ((count 0) (sum 0))
  (mu:for-each-message
    (lambda (msg)
      (set! count (+1 count))
      (set! sum (+ sum (func msg))))
    expr)
  (if (= count 0)
    0
    (exact->inexact (/ sum count)))))

