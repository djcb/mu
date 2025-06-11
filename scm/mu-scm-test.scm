;; unit tests

(use-modules (mu) (srfi srfi-64))

(define (test-basic)
  (test-begin "test-basic")

  (test-equal "mcount" 19 (mcount))
  (test-equal "cfind" 29 (length (cfind "")))
  (test-equal "mfind" 19 (length (mfind "")))

  (test-end "test-basic"))

(define (test-basic-mfind)

  (test-begin "test-basic-mfind")

  (let ((msg (car (mfind ""))))
    ;; size
    (test-equal 490 (size msg))
    ;; message-id
    (test-equal "abcd$efgh@example.com" (message-id msg))
    ;; subject
    (test-equal "Greetings from Lothlórien" (subject msg))
    ;; from
    (test-equal 1 (length (from msg)))
    (let ((sender (car (from msg))))
      (test-equal "Frodo Baggins" (assoc-ref sender 'name))
      (test-equal "frodo@example.com" (assoc-ref sender 'email)))
    ;; to
    (test-equal 1 (length (to msg)))
    (let ((recip (car (to msg))))
      (test-equal "Bilbo Baggins" (assoc-ref recip 'name))
      (test-equal "bilbo@anotherexample.com" (assoc-ref recip 'email)))

    ;; no date
    (test-assert (not (date msg)))

    ;; flags
    (test-equal '(unread) (flags msg))
    (test-assert (unread? msg))
    (test-assert (not (seen? msg)))
    (test-assert (not (new? msg))))

  (test-end "test-basic-mfind"))

(define (test-mfind)
  (test-begin "test-mfind")
  (let ((msg (car (mfind "" #:sort-field 'date #:reverse? #t))))

    (test-equal "test with multi to and cc" (subject msg) )
    (test-equal "2016-05-15T16:57:25" (time-t->iso-date (date msg))))

  (test-end "test-mfind"))

(define (test-helpers)
  (test-begin "test-helpers")
  (test-equal 1750077792 (iso-date->time-t "2025-06-16T12:43:12"))
  (test-equal 1750075200 (iso-date->time-t "2025-06-16T12"))

  (test-equal "2025-06-16T12:43:12" (time-t->iso-date 1750077792))
  (test-equal "                   " (time-t->iso-date #f))
  (test-end "test-helpers"))

(define* (main _ #:rest args)
  (let ((runner (test-runner-simple)))
    (test-with-runner runner
      (test-begin "mu-scm-tests")

      (test-basic)
      (test-basic-mfind)
      (test-mfind)
      (test-helpers)

      (test-end "mu-scm-tests")
      (exit (test-runner-fail-count runner)))))
