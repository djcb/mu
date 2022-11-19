#!/bin/sh
exec guile -e main -s $0 $@
!#
;; Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; INFO: Histogram of the number of messages per time-unit
;; INFO: Options:
;; INFO:   --query=<query>:   limit to messages matching query
;; INFO:   --muhome=<muhome>: path to mu home dir
;; INFO:   --time-unit:       hour|day|month|year|month-year
;; INFO:   --output:          the output format, such as "png", "wxt"
;; INFO:                      (depending on the environment)

(use-modules (mu) (mu stats) (mu plot)
	     (ice-9 getopt-long) (ice-9 format))

(define (per-hour expr output)
  "Count the total number of messages per hour that match EXPR.
OUTPUT corresponds to the output format, as per gnuplot's 'set terminal'."
  (mu:plot-histogram
    (sort
      (mu:tabulate
	(lambda (msg)
	  (tm:hour (localtime (mu:date msg)))) expr)
      (lambda (x y) (< (car x) (car y))))
    (format #f "Messages per hour matching ~a" expr)
    "Hour" "Messages" output))


(define (per-day expr output)
  "Count the total number of messages for each weekday (0-6 for
Sun..Sat) that match EXPR. OUTPUT corresponds to the output format, as
per gnuplot's 'set terminal'."
  (mu:plot-histogram
    (mu:weekday-numbers->names
      (sort (mu:tabulate
	      (lambda (msg)
		(tm:wday (localtime (mu:date msg)))) expr)
	(lambda (x y) (< (car x) (car y)))))
    (format #f "Messages per weekday matching ~a" expr)
    "Day" "Messages" output))

(define (per-month expr output)
  "Count the total number of messages per month that match EXPR.
OUTPUT corresponds to the output format, as per gnuplot's 'set terminal'."
  (mu:plot-histogram
    (mu:month-numbers->names
      (sort
	(mu:tabulate
	  (lambda (msg)
	    (tm:mon (localtime (mu:date msg)))) expr)
	(lambda (x y) (< (car x) (car y)))))
    (format #f "Messages per month matching ~a" expr)
    "Month" "Messages" output))

(define (per-year expr output)
  "Count the total number of messages per year that match EXPR. OUTPUT corresponds
to the output format, as per gnuplot's 'set terminal'."
  (mu:plot-histogram
   (sort (mu:tabulate
	  (lambda (msg)
	    (+ 1900 (tm:year (localtime (mu:date msg))))) expr)
	 (lambda (x y) (< (car x) (car y))))
   (format #f "Messages per year matching ~a" expr)
   "Year" "Messages"  output))


(define (per-year-month expr output)
  "Count the total number of messages for each year and month that match EXPR.
OUTPUT corresponds to the output format, as per gnuplot's 'set terminal'."
  (mu:plot-histogram
    (sort (mu:tabulate
	    (lambda (msg)
	      (string->number
		(format #f "~d~2'0d"
		  (+ 1900 (tm:year (localtime (mu:date msg))))
		  (tm:mon (localtime (mu:date msg))))))
	    expr)
      (lambda (x y) (< (car x) (car y))))
    (format #f "Messages per year/month matching ~a" expr)
    "Year/Month" "Messages" output))

(define (main args)
  (let* ((optionspec
	  '((time-unit (value #t))
	    (query     (value #t))
	    (muhome    (value #t))
	    (output    (value #t))
	    (help      (single-char #\h) (value #f))))
	 (options (getopt-long args optionspec))
	 (help (option-ref options 'help #f))
	 (time-unit (option-ref options 'time-unit "year"))
	 (muhome (option-ref options 'muhome #f))
	 (query (option-ref options 'query ""))
	 (output (option-ref options 'output "dumb"))
	 (rest (option-ref options '() #f))
	 (func
	  (cond
	   ((equal? time-unit "hour") per-hour)
	   ((equal? time-unit "day")  per-day)
	   ((equal? time-unit "month") per-month)
	   ((equal? time-unit "year") per-year)
	   ((equal? time-unit "year-month") per-year-month)
	   (else #f))))
    (setlocale LC_ALL "")
    (unless func
      (display "error: unknown time-unit\n")
      (set! help #t))
    (if help
	(begin
	  (display
	   (string-append "parameters: [--help] [--output=dumb|png|wxt] "
			  "[--muhome=<muhome>] [--query=<query>]"
			  "[--time-unit=hour|day|month|year|year-month]"))
	  (newline))
	(begin
	  (mu:initialize muhome)
	  (func query output)))))

;; Local Variables:
;; mode: scheme
;; End:
