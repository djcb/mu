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


(define-module (mu stats)
  :use-module (ice-9 optargs)
  :use-module (ice-9 popen)
  :use-module (ice-9 format)
  :use-module (mu log)
  :use-module (mu store)
  :use-module (mu msg)
  :export
  (
    mu:stats:count
    mu:stats:average
    mu:stats:average-size
    mu:stats:average-recipient-number
    mu:stats:frequency
    mu:stats:per-weekday
    mu:stats:per-month
    mu:stats:per-hour
    mu:stats:per-year
    mu:stats:top-n
    mu:stats:top-n-to
    mu:stats:top-n-from
    mu:stats:top-n-subject
    mu:stats:table
    mu:stats:histogram
    mu:stats:export
    mu:plot:per-month
    mu:plot:per-weekday
    mu:plot:per-year
    mu:plot:per-hour
    ))

;; note, this is a rather inefficient way to calculate the number; for
;; demonstration purposes only...
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

(define* (mu:plot:per-weekday #:optional (EXPR ""))
  (let* ((datafile (mu:stats:export (mu:stats:per-weekday EXPR)))
	  (gnuplot (open-pipe "gnuplot -p" OPEN_WRITE)))
    ;; note, we cannot use the weekday "%a" support in gnuplot because
    ;; demands the field to be a date field ('set xdata time' etc.)
    ;; for that to work, but we cannot use that since gnuplot does not
    ;; support weekdays ('%w') as a date field in its input
    (display (string-append
	       "reset\n"
	       "set xtics (\"Sun\" 0, \"Mon\" 1, \"Tue\" 2, \"Wed\" 3,"
	                  "\"Thu\" 4, \"Fri\" 5, \"Sat\" 6);\n"
	       "set xlabel \"Weekday\"\n"
	       "set ylabel \"# of messages\"\n"
	       "set boxwidth 0.9\n") gnuplot)
    (display (string-append "plot \"" datafile "\" using 1:2 with boxes fs solid\n")
      gnuplot)
    (close-pipe gnuplot)))


(define* (mu:stats:per-month #:optional (EXPR ""))
  "Count the total number of messages for each month (1-12 for
Jan..Dec). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (month . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) ;; note the 1+
		    (1+ (tm:mon (localtime (mu:msg:date msg))))) EXPR)))
    (sort stats
      (lambda(a b)
	(< (car a) (car b)))))) ;; in order ofmonth


(define* (mu:plot:per-month #:optional (EXPR ""))
  (let* ((datafile (mu:stats:export (mu:stats:per-month EXPR)))
	  (gnuplot (open-pipe "gnuplot -p" OPEN_WRITE)))
    (display (string-append
	       "reset\n"
	       "set xtics (\"Jan\" 1, \"Feb\" 2, \"Mar\" 3, \"Apr\" 4,"
	                  "\"May\" 5, \"Jun\" 6, \"Jul\" 7, \"Aug\" 8,"
	                  "\"Sep\" 9, \"Oct\" 10, \"Nov\" 11, \"Dec\" 12);\n"
	       "set xlabel \"Month\"\n"
	       "set ylabel \"# of messages\"\n"
	       "set boxwidth 0.9\n") gnuplot)
    (display (string-append "plot \"" datafile "\" using 1:2 with boxes fs solid\n")
      gnuplot)
    (close-pipe gnuplot)))


(define* (mu:stats:per-hour #:optional (EXPR ""))
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (weekday . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (tm:hour (localtime (mu:msg:date msg)))) EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of hour

(define* (mu:plot:per-hour #:optional (EXPR ""))
  (let* ((datafile (mu:stats:export (mu:stats:per-hour EXPR)))
	  (gnuplot (open-pipe "gnuplot -p" OPEN_WRITE)))
    (display (string-append
	       "reset\n"
	       "set xlabel \"Hour\"\n"
	       "set ylabel \"# of messages\"\n"
	       "set boxwidth 0.9\n") gnuplot)
    (display (string-append "plot \"" datafile "\" using 1:2 with boxes fs solid\n")
      gnuplot)
    (close-pipe gnuplot)))




(define* (mu:stats:per-year #:optional (EXPR ""))
  "Count the total number of messages for each year since 1970. If the
optional EXPR is provided, only count the messages that match it. The
result is a list of pairs (year . frequency).\n"
  (let* ((stats (mu:stats:frequency
		  (lambda (msg) (+ 1900 (tm:year (localtime (mu:msg:date msg)))))
		  EXPR)))
    (sort stats (lambda(a b) (< (car a) (car b)))))) ;; in order of year

(define* (mu:plot:per-year #:optional (EXPR ""))
  (let* ((datafile (mu:stats:export (mu:stats:per-year EXPR)))
	  (gnuplot (open-pipe "gnuplot -p" OPEN_WRITE)))
    (display (string-append
	       "reset\n"
	       "set xlabel \"Year\"\n"
	       "set ylabel \"# of messages\"\n"
	       "set boxwidth 0.9\n") gnuplot)
    (display (string-append "plot \"" datafile "\" using 1:2 with boxes fs solid\n")
      gnuplot)
    (close-pipe gnuplot)))

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
  "Display a list of PAIRS in a table-like fashion."
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

;; (define* (mu:stats:histogram pairs #:optional (port (current-output-port)))
;;   "Display a histogram of the list of cons pairs; the car of each pair
;; is used for the x-asxis, while the cdr represents the y value."
;;   (let ((pairs ;; pairs may be unsorted, so let's sort first
;; 	  (sort (pairs) (lambda(x1 x2) (< x1 x2)))))

(define (mu:stats:export pairs)
  "Export PAIRS to a temporary file, return its name. The data can
then be used in, e.g., R and gnuplot."
  (let* ((datafile (tmpnam))
	  (output (open datafile (logior O_CREAT O_WRONLY) #O0644)))
    (mu:stats:table pairs output)
    (close output)
    datafile))
