;; unit tests

(use-modules (mu) (srfi srfi-64)
	     (ice-9 textual-ports))

(define (test-store)
  (test-begin "test-store")

  (test-equal "mcount" 19 (mcount))
  (test-equal "cfind" 29 (length (cfind "")))
  (test-equal "mfind" 19 (length (mfind "")))

  (test-assert (personal? "user@example.com"))
  (test-assert (not (personal? "user@anotherexample.com")))

  (let ((info (store->alist)))
    (test-equal 50000 (assoc-ref info 'batch-size))
    (test-equal 100000000 (assoc-ref info 'max-message-size)))

  (test-end "test-store"))

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

    ;; cc, bc
    (test-equal '() (cc msg))
    (test-equal '() (bcc msg))

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

  (let* ((msg (car (mfind "subject:\"gcc include search order\"")))
	 (alist (message->alist msg)))
    (test-equal "gcc include search order" (subject msg))
    (test-equal "klub" (header msg "precedence"))
    (test-equal "gcc-help.gcc.gnu.org" (mailing-list msg))
    (test-equal '() (references msg))
    (test-equal "3BE9E6535E3029448670913581E7A1A20D852173@emss35m06.us.lmco.com" (message-id msg))
    (test-equal "3BE9E6535E3029448670913581E7A1A20D852173@emss35m06.us.lmco.com" (thread-id msg))

    ;; alist
    (test-equal "gcc include search order" (assoc-ref alist 'subject))
    (test-equal 'normal (assoc-ref alist 'priority))
    (test-equal '((email . "anon@example.com") (name . "Mickey Mouse"))
      (car  (assoc-ref alist 'from)))

    ;; cc, bc, labels
    (test-equal '() (cc msg))
    (test-equal '() (bcc msg))
    (test-equal '() (labels msg)))

  (test-end "test-message-more"))

(define (test-message-parts)
  (test-begin "test-message-parts")
  (let* ((msg (car (mfind "flag:attach")))
	 (parts (mime-parts msg)))
    (test-equal 3 (length parts))
    (test-equal
	'(((index . 0) (content-type . "text/plain") (size . 1174))
	  ((index . 1) (content-type . "text/x-vcard") (attachment? . #t) (size . 306)
	   (filename . "mihailim.vcf"))
	  ((index . 2) (content-type . "text/plain") (size . 153)))
      (map (lambda (part) (mime-part->alist part)) parts))

    (let ((port (make-port (car parts))))
      (test-assert (port? port))
      (test-assert (input-port? port))
      (test-assert (not (port-closed? port)))
      (test-equal "Marco Bambini wrote:" (get-string-n port 20)))

    (test-end "test-message-parts")))

(define (test-message-labels)
  (test-begin "test-message-labels")
  (let* ((perfmsgs (mfind "label:performance")))
    (test-equal 4 (length perfmsgs))
    (for-each (lambda (msg)
		(test-equal 1 (length (labels msg)))
		(test-equal "performance" (car (labels msg))))
	      perfmsgs))
    (test-end "test-message-labels"))

(define (test-message-new)
  (test-begin "test-message-new")
  (let ((msg (make-message (format #f "~a/testdir2/Foo/cur/mail5" (getenv "MU_TESTDATADIR"))))
	(tmpdir (getenv "MU_TESTTEMPDIR")))
    (test-equal "pics for you" (subject msg))
    (test-equal
	'(((index . 0) (content-type . "text/plain") (size . 27))
	  ((index . 1) (content-type . "image/jpeg") (size . 23881) (filename . "sittingbull.jpg"))
	  ((index . 2) (content-type . "image/jpeg") (size . 21566) (filename . "custer.jpg")))
      (map (lambda (part) (mime-part->alist part)) (mime-parts msg)))

    (test-equal "mime-part-0" (filename (list-ref (mime-parts msg) 0)))
    (test-equal "sittingbull.jpg" (filename (list-ref (mime-parts msg) 1)))
    (test-equal "custer.jpg" (filename (list-ref (mime-parts msg) 2)))

    (let* ((part (list-ref (mime-parts msg) 1))
	   (alist (mime-part->alist part))
	   (fname (format #f "~a/~a" tmpdir (assoc-ref alist 'filename))))
      (write-to-file part #:path fname)
      (test-assert (access? fname R_OK))
      ;; note, the 23881 is the _encoded_ size.
      (test-equal 17674 (stat:size (stat fname))))

    ;;(write-to-file (list-ref (mime-parts msg) 1))
    (test-end "test-message-new")))

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

      (test-store)
      (test-basic-mfind)
      (test-mfind)
      (test-message-full)
      (test-message-more)
      (test-message-parts)
      (test-message-labels)
      (test-message-new)
      (test-options)
      (test-helpers)

      (test-end "mu-scm-tests")
      (exit (test-runner-fail-count runner)))))
