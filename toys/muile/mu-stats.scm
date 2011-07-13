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
;;
;;

;; some guile/scheme functions to get various statistics of my mu
;; message store.

;; note, this is a rather inefficient way to calculate the number; for
;; demonstration purposes only
(define (mu:stats:count EXPR)
  "Count the total number of messages. If the optional EXPR is
provided, only count the messages that match it.\n"
  (mu:store:foreach (lambda(msg) #f) EXPR))


(define (mu:stats:average-size EXPR)
  "Count the total number of messages. If the optional EXPR is
provided, only count the messages that match it.\n"
  (let* ((sum 0)
	  (n (mu:store:foreach
	       (lambda(msg) (set! sum (+ sum (mu:msg:size msg)))) EXPR)))
    (if (= n 0) 0 (exact->inexact (/ sum n)))))






