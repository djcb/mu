(require 'ert)

(ert-deftest mu4e-test-remove-mail-header-separator ()
  "`mail-header-separator` is removed correctly"
  (let* ((header "To: mail@example.com")
         (sep "--example-separator--")
         (testeq
          (lambda (orig removed)
            (with-temp-buffer
              (insert orig)
              ;; shadow the separator the mu4e function sees
              (make-local-variable 'mail-header-separator)
              (setq mail-header-separator sep)
              (mu4e~draft-remove-mail-header-separator)
              (should (equal (buffer-string) removed))))))
    ;; empty mail equal to header
    (funcall testeq
             (concat header "\n" sep)
             (concat header "\n"))
    ;; message starts directly
    (funcall testeq
             (concat header "\n" sep "\n" "message1")
             ;; needs one empty line
             (concat header "\n\n" "message1"))
    ;; empty line is kept
    (funcall testeq
             (concat header "\n" sep "\n\n" "message2")
             (concat header "\n\n" "message2"))))


(provide 'mu4e-tests)
