;;; -*- no-byte-compile: t; -*-
((nil . ((tab-width . 8)
         (fill-column . 80)
         ;; (commment-fill-column . 80)
         (emacs-lisp-docstring-fill-column . 65)
         (bug-reference-url-format . "https://github.com/djcb/mu/issues/%s")
         ;;   uncomment below (or put in .dir-locals-2.el)
         ;;   to dtrt for M-x compile
         ;;
         ;; (eval . (setq compile-command
         ;;               (format "make -C %s"
         ;;                       (shell-quote-argument
         ;;                        (locate-dominating-file "." "Makefile")))))
         ))
 (c-mode . ((c-file-style . "linux")
            (indent-tabs-mode . t)
            (mode . bug-reference-prog)))
 (c-ts-mode . ((indent-tabs-mode . t)
               ;; (c-ts-indent-style . linux)
               ;; (c-ts-indent-offset . 8)
               (mode . bug-reference-prog)))
 (c++-mode . ((c-file-style . "linux")
              (fill-column . 100)
              ;; (comment-fill-column . 80)
              (mode . bug-reference-prog)))
 (c++-ts-mode . ((indent-tabs-mode . t)
                 ;; (c-ts-indent-style . linux)
                 ;; (c-ts-indent-offset . 8)
                 (mode . bug-reference-prog)))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (mode . bug-reference-prog)))
 (lisp-data-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode . ((mode . bug-reference-prog)))
 (org-mode . ((mode . bug-reference))))
