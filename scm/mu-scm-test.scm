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
    (test-equal "2016-05-15 16:57:25"
      (time->string (date msg) #:format "%F %T" #:utc? #t)))

  (test-end "test-mfind"))


(define (test-message-full)
  (test-begin "test-message-full")

  (let ((msg (cadr (mfind ""))))
    (test-equal "Motörhead" (header msg "Subject"))
    (test-equal "Mü <testmu@testmu.xx>" (header msg "From"))
    (test-equal #f (header msg "Bla"))

    (test-equal (string-append "\nTest for issue #38, where apparently searching for "
			       "accented words in subject,\nto etc. fails.\n\n"
			       "What about here? Queensrÿche. Mötley Crüe.\n\n\n")
      (body msg))
    (test-equal #f (body msg #:html? #t))

    (test-end "test-message-full")))

(define (test-message-more)
  (test-begin "test-message-more")
  (let ((msg (car (mfind "to:dfgh@floppydisk.nl"))))
    (test-equal "Re: xyz" (subject msg))
    (test-equal "Mozilla Thunderbird 1.0.7 (X11/20051010)" (header msg "User-Agent"))
    (test-equal '("439C1136.90504@euler.org" "4399DD94.5070309@euler.org"
		  "20051209233303.GA13812@gauss.org" "439B41ED.2080402@euler.org"
		  "439A1E03.3090604@euler.org" "20051211184308.GB13513@gauss.org")
      (references msg))
      (test-equal "439C1136.90504@euler.org" (thread-id msg)))

  (let ((msg (car (mfind "subject:\"gcc include search order\""))))
    (test-equal "gcc include search order" (subject msg))
    (test-equal "klub" (header msg "precedence"))
    (test-equal "gcc-help.gcc.gnu.org" (mailing-list msg))
    (test-equal #f (references msg))
    (test-equal "3BE9E6535E3029448670913581E7A1A20D852173@emss35m06.us.lmco.com" (message-id msg))
    (test-equal "3BE9E6535E3029448670913581E7A1A20D852173@emss35m06.us.lmco.com" (thread-id msg)))
  (test-end "test-message-more"))

(define (test-options)
  (test-begin "test-options")
  (let ((opts %options))
    (test-assert (>= (length opts) 4))
    (test-equal (assoc-ref opts 'quiet) #f)
    (test-equal (assoc-ref opts 'debug) #f)
    (test-equal (assoc-ref opts 'verbose) #f)
    (test-equal (assoc-ref opts 'muhome) #f))
    (test-end "test-options"))

(define (test-helpers)
  (test-begin "test-helpers")
  (setenv "TZ" "Europe/Helsinki")
  (tzset)
  (test-equal 1750077792 (string->time "2025-06-16T15:43:12" #:utc? #f))
  (test-equal 1750077792 (string->time "2025-06-16 12:43:12" #:utc? #t))
  (test-equal 1750075200 (string->time "2025-06-16  12" #:utc? #t))

  (test-equal "2025-06-16 12:43:12" (time->string 1750077792 #:utc? #t))
  (test-equal "2025-06-16 15:43:12" (time->string 1750077792 #:utc? #f))
  (test-equal "12:43:12" (time->string 1750077792 #:utc? #t #:format "%T"))

  ;; (define old-prefs %preferences)
  ;; (define %preferences '((utc? . #t) (short-date  . "%T %F")))
  ;; (test-equal "12:43:12 2025-06-16" (time->string 1750077792))
  ;; (set! %preferences old-prefs)

  (test-equal #f (time->string #f))
  (test-end "test-helpers"))

(define* (main _ #:rest args)
  (let ((runner (test-runner-simple)))
    (test-with-runner runner
      (test-begin "mu-scm-tests")

      (test-basic)
      (test-basic-mfind)
      (test-mfind)
      (test-message-full)
      (test-message-more)
      (test-options)
      (test-helpers)

      (test-end "mu-scm-tests")
      (exit (test-runner-fail-count runner)))))
