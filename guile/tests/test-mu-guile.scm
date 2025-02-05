#!/bin/sh
exec guile -e main -s $0 $@
!#

;; Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

(use-modules (srfi srfi-1))
(use-modules (ice-9 getopt-long) (ice-9 optargs) (ice-9 popen) (ice-9 format))
(use-modules (mu) (mu stats))

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
  (n-results-or-exit "mime:text/plain" 14)
  (n-results-or-exit "y:text*" 14)
  (n-results-or-exit "y:image*" 1)
  (n-results-or-exit "mime:message/rfc822" 2))

(define (error-exit msg . args)
  "Print error and exit."
  (let ((msg (apply format #f msg args)))
    (simple-format (current-error-port) "*ERROR*: ~A\n" msg)
    (exit 1)))

(define (str-equal-or-exit got exp)
  "S1 == S2 or exit 1."
  ;; (format #t "'~A' <=> '~A'\n" s1 s2)
  (if (not (string= exp got))
    (error-exit "Expected \"~A\", got \"~A\"\n" exp got)))

(define (test-message)
  "Test functions for a particular message."

  (let ((msg (car (mu:message-list "hello"))))
    (str-equal-or-exit (mu:subject msg) "Fwd: rfc822")
    (str-equal-or-exit (mu:to msg) "martin")
    (str-equal-or-exit (mu:from msg) "foobar <foo@example.com>")
    (str-equal-or-exit (mu:body msg) "Hello world, forwarding some RFC822 message\n")
    (str-equal-or-exit (mu:header msg "X-Mailer") "Ximian Evolution 1.4.5")
    ;; issue #2802
    (str-equal-or-exit (mu:body msg) "Hello world, forwarding some RFC822 message\n")

    (if (not (equal? (mu:priority msg) mu:prio:normal))
	(error-exit "Expected ~A, got ~A"  (mu:priority msg) mu:prio:normal)))

  (let ((msg (car (mu:message-list "atoms"))))
    (str-equal-or-exit (mu:subject msg) "atoms")
    (str-equal-or-exit (mu:to      msg) "Democritus <demo@example.com>")
    (str-equal-or-exit (mu:from    msg) "Richard P. Feynman <rpf@example.com>")
    ;;(str-equal-or-exit (mu:header msg "Content-Transfer-Encoding") "8bit")
    (str-equal-or-exit (mu:body msg)
		       (string-join
			'("If, in some cataclysm, all scientific knowledge were to be destroyed,"
			  "and only one sentence passed on to the next generation of creatures,"
			  "what statement would contain the most information in the fewest words?"
			  "I believe it is the atomic hypothesis (or atomic fact, or whatever you"
			  "wish to call it) that all things are made of atoms — little particles"
			  "that move around in perpetual motion, attracting each other when they"
			  "are a little distance apart, but repelling upon being squeezed into"
			  "one another. In that one sentence you will see an enormous amount of"
			  "information about the world, if just a little imagination and thinking"
			  "are applied.\n") "\n"))
    (str-equal-or-exit (mu:body-txt msg) (mu:body msg))
    (let ((got (mu:body-html msg)))
      (if got
	  (error-exit "Expected #f, got ~a" got)))

    (if (not (equal? (mu:priority msg) mu:prio:high))
      (error-exit "Expected ~a, got ~a"  (mu:priority msg) mu:prio:high))))

(define (num-equal-or-exit got exp)
  "S1 == S2 or exit 1."
  ;; (format #t "'~A' <=> '~A'\n" s1 s2)
  (if (not (= exp got))
    (error-exit "Expected \"~S\", got \"~S\"\n" exp got)))

(define (test-stats)
  "Test statistical functions."
  ;; average
  (num-equal-or-exit (mu:average mu:size) 41299/7)
  (num-equal-or-exit (floor (mu:stddev mu:size)) 12637.0)
  (num-equal-or-exit (mu:max mu:size) 46308)
  (num-equal-or-exit (mu:min mu:size) 111))

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
