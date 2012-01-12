;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

(define-module (mu stats)
  :use-module (oop goops)
  :use-module (mu message)
  :export ( mu:tabulate-messages
	    mu:average-messages))


(define* (mu:tabulate-messages func #:optional (expr #t))
  "Execute FUNC for each message matching EXPR, and return an alist
with maps each result of FUNC to its frequency. FUNC is a function
takes a <mu-message> instance as its argument. For example, to
tabulate messages by weekday, one could use:
   (mu:tabulate-messages (lambda(msg) (tm:wday (localtime (date msg)))))."
  (let ((table '()))
    (mu:for-each-message
      (lambda(msg)
	(let* ((val (func msg))
		(old-freq (or (assoc-ref table val) 0)))
	  (set! table (assoc-set! table val (1+ old-freq)))))
      expr)
    table))


(define* (mu:average-messages func #:optional (expr #t))
  "Execute FUNC for each message matching EXPR, and return the average
value of the results of FUNC.  FUNC is a function that takes a
<mu-message> instance as its argument, and returns some number. For
example, to get the average message size of messages related to
icecream:  (mu:average (lambda(msg) (size msg)) \"icecream\" ."
(let ((count 0) (sum 0))
  (mu:for-each-message
    (lambda (msg)
      (set! count (+1 count))
      (set! sum (+ sum (func msg))))
    expr)
  (if (= count 0)
    0
    (exact->inexact (/ sum count)))))
