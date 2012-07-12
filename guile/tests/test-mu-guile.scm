#!/bin/sh
exec guile -e main -s $0 $@
!#

;; Copyright (C) 2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
(setlocale LC_ALL "")

(use-modules (ice-9 getopt-long) (ice-9 optargs) (ice-9 popen) (ice-9 format))
(use-modules (mu) (mu message) (mu stats) (mu plot))

(define (n-results-or-exit query n)
  "Run QUERY, and exit 1 if the number of results != N."
  (let ((lst (mu:message-list query)))
    (if (not (= (length lst) n))
      (begin
	(simple-format (current-error-port) "Query: \"~A\"; expected ~A, got ~A\n"
	  query n (length lst))
	(exit 1)))))

(define (test-queries)
  "Test a bunch of queries (or die trying)."

  (n-results-or-exit "hello" 1)
  (n-results-or-exit "f:john fruit" 1)
  (n-results-or-exit "f:soc@example.com" 1)
  (n-results-or-exit "t:alki@example.com" 1)
  (n-results-or-exit "t:alcibiades" 1)
  (n-results-or-exit "f:soc@example.com OR f:john" 2)
  (n-results-or-exit "f:soc@example.com OR f:john OR t:edmond" 3)
  (n-results-or-exit "t:julius" 1)
  (n-results-or-exit "s:dude" 1)
  (n-results-or-exit "t:dantès" 1)
  (n-results-or-exit "file:sittingbull.jpg" 1)
  (n-results-or-exit "file:custer.jpg" 1)
  (n-results-or-exit "file:custer.*" 1)
  (n-results-or-exit "j:sit*" 1)
  (n-results-or-exit "mime:image/jpeg" 1)
  (n-results-or-exit "mime:text/plain" 12)
  (n-results-or-exit "y:text*" 12)
  (n-results-or-exit "y:image*" 1)
  (n-results-or-exit "mime:message/rfc822" 2))


(define (str-equal-or-exit s1 s2)
  "S1 == S2 or exit 1."
  ;; (format #t "'~A' <=> '~A'\n" s1 s2)
  (if (not (string= s1 s2))
    (begin
      (simple-format (current-error-port) "Message: expected \"~A\", got \"~A\"\n"
	s1 s2)
      (exit 1))))

(define (test-message)
  "Test functions for a particular message."
  (let ((msg (car (mu:message-list "hello"))))
    (str-equal-or-exit (mu:subject msg) "Fwd: rfc822")
    (str-equal-or-exit (mu:to      msg) "martin")
    (str-equal-or-exit (mu:from    msg) "foobar <foo@example.com>")))


(define (test-stats)
  "Test statistical functions."
  )


(define (main args)

  (let* ((optionspec  '((muhome  (value #t))
			 (test (value #t))))
	  (options (getopt-long args optionspec))
	  (muhome (option-ref options 'muhome #f))
	  (test   (option-ref options 'test #f)))

    (mu:initialize muhome)

    (if test
      (cond
	((string= test "queries") (test-queries))
	((string= test "message") (test-message))
	((string= test "stats")   (test-stats))
	(#t (exit 1))))))


;; Local Variables:
;; mode: scheme
;; End:
