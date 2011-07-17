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

;; note, this is a rather inefficient way to calculate the number; for
;; demonstration purposes only
(define* (mu:stats:count #:optional (EXPR ""))
  "Count the total number of messages. If the optional EXPR is
provided, only count the messages that match it.\n"
  (mu:store:for-each (lambda(msg) #f) EXPR))

(define* (mu:stats:average FUNC #:optional (EXPR ""))
  "Count the average of the result of applying FUNC on all
messages. If the optional EXPR is provided, only consider the messages
that match it.\n"
  (let* ((sum 0)
	  (n (mu:store:for-each
	       (lambda(msg) (set! sum (+ sum (FUNC msg)))) EXPR)))
    (if (= n 0) 0 (exact->inexact (/ sum n)))))

(define* (mu:stats:average-size #:optional (EXPR ""))
  "Calculate the average message size. If the optional EXPR is
provided, only consider the messages that match it.\n"
  (mu:stats:average (lambda(msg) (mu:msg:size msg)) EXPR))

(define* (mu:stats:average-recipient-number #:optional (EXPR ""))
  "Calculate the average number of recipients (To: + CC: + Bcc:). If
the optional EXPR is provided, only consider the messages that match
it.\n"
  (mu:stats:average (lambda(msg)
		      (+(length (mu:msg:to msg))
			(length (mu:msg:cc msg))
			(length (mu:msg:bcc msg)))) EXPR))

(define* (mu:stats:frequency FUNC #:optional (EXPR ""))
  "FUNC is a function that takes a mMsg, and returns the frequency of
the different values this function returns. If FUNC returns a list,
update the frequency table for each element of this list. If the
optional EXPR is provided, only consider messages that match it.\n"
  (let ((table '()))
    (mu:store:for-each
      (lambda(msg)
	;; note, if val is not already a list, turn it into a list
	;; then, take frequency for each element in the list
	(let* ((val (FUNC msg)) (vals (if (list? val) val (list val))))
	  (for-each
	    (lambda (val)
	      (let ((freq (assoc-ref table val)))
		(set! table (assoc-set! table val
			      (+ 1 (if (eq? freq #f) 0 freq)))))) vals))) EXPR)
    table))
 
    
(define* (mu:stats:per-weekday #:optional (EXPR ""))
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (weekday . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (tm:wday (localtime (mu:msg:date msg)))) EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of weekday

(define* (mu:stats:per-month #:optional (EXPR ""))
  "Count the total number of messages for each month (0-11 for
Jan..Dec). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (month . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (tm:mon (localtime (mu:msg:date msg)))) EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of month

(define* (mu:stats:per-hour #:optional (EXPR ""))
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (weekday . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (tm:hour (localtime (mu:msg:date msg)))) EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of hour

	   
(define* (mu:stats:per-year #:optional (EXPR ""))
  "Count the total number of messages for each year since 1970. If the
optional EXPR is provided, only count the messages that match it. The
result is a list of pairs (year . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (+ 1900 (tm:year (localtime (mu:msg:date msg)))))
		  EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of year


(define* (mu:stats:top-n FUNC N #:optional (EXPR ""))
  "Get the Top-N frequency of the result of FUNC applied on each
message. If the optional EXPR is provided, only consider the messages
that match it."
  (let* ((freq (mu:stats:frequency FUNC EXPR))
	  (top (sort freq (lambda (a b) (< (cdr b) (cdr a) )))))
    (list-head top (min (length freq) N))))

(define* (mu:stats:top-n-to #:optional (N 10) (EXPR ""))
  "Get the Top-N To:-recipients. If the optional N is not provided,
use 10. If the optional EXPR is provided, only consider the messages
that match it."
  (mu:stats:top-n
    (lambda (msg) (mu:msg:to msg)) N EXPR))

(define* (mu:stats:top-n-from #:optional (N 10) (EXPR ""))
  "Get the Top-N senders (From:). If the optional N is not provided,
use 10. If the optional EXPR is provided, only consider the messages
that match it."
  (mu:stats:top-n
    (lambda (msg) (mu:msg:from msg)) N EXPR))

(define* (mu:stats:top-n-subject #:optional (N 10) (EXPR ""))
  "Get the Top-N subjects. If the optional N is not provided,
use 10. If the optional EXPR is provided, only consider the messages
that match it."
  (mu:stats:top-n
    (lambda (msg) (mu:msg:subject msg)) N EXPR))

(define* (mu:stats:table pairs #:optional (port (current-output-port)))
  "display a list of PAIRS in a table-like fashion"
  (let ((maxlen 0))
    (for-each ;; find the widest in the first col
      (lambda (pair)
	(set! maxlen
	  (max maxlen (string-length (format #f "~s " (car pair)))))) pairs)
    (for-each
      (lambda (pair)
	(let ((first (format #f "~s" (car pair)))
	       (second (format #f "~s" (cdr pair))))
	  (display (format #f "~A~v_~A\n"  
		     first (- maxlen (string-length first)) second) port)))
      pairs)))

(define (mu:stats:export pairs)
  "Export PAIRS to a temporary file, return its name. The data can
then be used in, e.g., R and gnuplot."
  (let* ((datafile (tmpnam))
	  (output (open datafile (logior O_CREAT O_WRONLY) #O0644)))
    (mu:stats:table pairs output)
    datafile))
