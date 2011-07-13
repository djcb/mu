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
(define (mu:stats:count EXPR)
  "Count the total number of messages. If the optional EXPR is
provided, only count the messages that match it.\n"
  (mu:store:foreach (lambda(msg) #f) EXPR))


(define (mu:stats:average FUNC EXPR)
  "Count the average of the result of applying FUNC on all
messages. If the optional EXPR is provided, only consider the messages
that match it.\n"
  (let* ((sum 0)
	  (n (mu:store:foreach
	       (lambda(msg) (set! sum (+ sum (FUNC msg)))) EXPR)))
    (if (= n 0) 0 (exact->inexact (/ sum n)))))


(define (mu:stats:average-size EXPR)
  "Calculate the average message size. If the optional EXPR is
provided, only consider the messages that match it.\n"
  (mu:stats:average (lambda(msg) (mu:msg:size msg)) EXPR))


(define (mu:stats:average-recipient-number EXPR)
  "Calculate the average number of recipients (To: + CC: + Bcc:). If
the optional EXPR is provided, only consider the messages that match
it.\n"
  (mu:stats:average (lambda(msg)
		      (+(length (mu:msg:to msg))
			(length (mu:msg:cc msg))
			(length (mu:msg:bcc msg)))) EXPR))

(define (mu:stats:frequency FUNC EXPR)
  "FUNC is a function that takes a Msg and returns some number between
0 and <MAX. If the optional EXPR is provided, only consider messages
that match it.\n"
  (let ((table '()))
    (mu:store:foreach
      (lambda(msg)
	(let* ((val (FUNC msg)) (freq (assoc-ref table val)))
	  (set! table (assoc-set! table val
			(+ 1 (if (eq? freq #f) 0 freq)))))) "")
    (sort table (lambda(a b) (< (car a) (car b))))))

(define (mu:stats:per-weekday EXPR)
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (weekday . frequency).\n"
  (mu:stats:frequency
    (lambda (msg) (tm:wday (localtime (mu:msg:date msg)))) EXPR))

(define (mu:stats:per-month EXPR)
  "Count the total number of messages for each month (0-11 for
Jan..Dec). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (month . frequency).\n"
  (mu:stats:frequency
    (lambda (msg) (tm:mon (localtime (mu:msg:date msg)))) EXPR))

(define (mu:stats:per-hour EXPR)
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat). If the optional EXPR is provided, only count the messages
that match it. The result is a list of pairs (weekday . frequency).\n"
  (mu:stats:frequency
    (lambda (msg) (tm:hour (localtime (mu:msg:date msg)))) EXPR))
	   
(define (mu:stats:per-year EXPR)
  "Count the total number of messages for each year since 1970. If the
optional EXPR is provided, only count the messages that match it. The
result is a list of pairs (year . frequency).\n"
  (mu:stats:frequency
    (lambda (msg) (+ 1900 (tm:year (localtime (mu:msg:date msg))))) EXPR))
