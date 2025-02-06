;;; mu4e-transient.el --- -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;;; Commentary:
;;;
;;; Define "transients" for some mu4e functionality.

;;; Code:
(require 'mu4e)
(require 'mu4e-bookmarks)
(require 'mu4e-compose)
(require 'mu4e-draft)
(require 'mu4e-headers)
(require 'mu4e-helpers)

(require 'gnus-ml)

(require 'transient)

;; Helpers.

(defun mu4e--toggle-description(name val)
  "Return a string for a transient toggle with NAME.
Show On/Off based on value VAL."
  (concat name " "
          (propertize (if val "On " "Off")
                      'face 'transient-value)))

(defmacro mu4e--define-toggle-suffix (name symbol &optional toggle-func)
  "Define a transient suffix to toggle some value.
Show the suffix with NAME. SYMBOL is the symbol for the value to
toggle.

TOGGLE-FUNC is the function to do the toggling. If nil, a simple
toggle-function is created."
  (let* ((symname (symbol-name symbol))
         (ifname (intern (format "mu4e--suffix-toggle-%s" symname)))
         (docstring (format "Toggle the value of `%s'" symname))
         (toggle-func (or toggle-func
                          `(lambda ()
                             (interactive)
                             (setq ,symbol (not ,symbol))))))
    `(progn
       (transient-define-suffix ,ifname () ,docstring
         :transient t
         :if (lambda () (boundp (quote ,symbol)))
         :description (lambda () (mu4e--toggle-description ,name ,symbol))
         (interactive)
         (call-interactively ,toggle-func)))))

;; Transients

(transient-define-prefix mu4e--prefix-docs-links()
  "Mu4e documentation menu."
  [["Documentation"
    ("A" "About mu4e"          mu4e-about)
    ("N" "News"                mu4e-news)
    ("I" "Ideas"               mu4e-ideas)
    ("H" "Manual"              mu4e-display-manual)
    ("o" "Online manual" (lambda ()  (interactive)
                           (browse-url "https://www.djcbsoftware.nl/code/mu/mu4e/")))]
   ["Links"
    ("h" "Mu homepage"  (lambda () (interactive)
                          (browse-url "https://www.djcbsoftware.nl/code/mu/")))
    ("m" "Mu4e homepage" (lambda () (interactive)
                           (browse-url "https://www.djcbsoftware.nl/code/mu/")))
    ("r" "Mu repository" (lambda ()  (interactive)
                           (browse-url "https://github.com/djcb/mu/")))
    ("o" "Mu Issues"     (lambda ()  (interactive)
                           (browse-url "https://github.com/djcb/mu/issues")))]])

(mu4e--define-toggle-suffix "Debug client" mu4e-debug #'mu4e-toggle-logging)
(mu4e--define-toggle-suffix "Debug server" mu4e-mu-debug)
(mu4e--define-toggle-suffix "Report render-time" mu4e-headers-report-render-time)
(mu4e--define-toggle-suffix "Temp-file optimization" mu4e-mu-allow-temp-file)

(transient-define-prefix mu4e--prefix-debug-tweaks()
  "Mu4e debugging and tweaks menu."
  [["Debug"
    ("$" mu4e--suffix-toggle-mu4e-debug)
    ("r" mu4e--suffix-toggle-mu4e-headers-report-render-time)
    ("L" "Toggle logging" mu4e-toggle-logging)
    ("l" "Log buffer" mu4e-show-log)]
   ["Info"
    ("," "Message sexp"   mu4e-sexp-at-point
     :inapt-if-not (lambda () (mu4e-message-at-point 'nowarn)))
    ("a" "Analyze query" mu4e-analyze-last-query)
    ("c" "Known contacts"  mu4e-contacts-info)]
   ["Server (needs restart)"
    ("d" mu4e--suffix-toggle-mu4e-mu-debug)
    ("t" mu4e--suffix-toggle-mu4e-mu-allow-temp-file)]])

(mu4e--define-toggle-suffix "format=flowed" mu4e-compose-format-flowed)
(mu4e--define-toggle-suffix "Forward as MIME" message-forward-as-mime)

;; (transient-define-suffix mu4e--suffix-message-forward-show-mml (args)
;;   "Show MML when forwarding?
;; See `message-forward-show-mml' for details."
;;   :transient t
;;   :description (lambda ()
;;                  (let ((val (pcase message-forward-as-mime
;;                               ('nil "Off")
;;                               ('best "Best")
;;                               (_    "On")))))
;;                  (concat "Show MML "
;;                          (propertize  val 'face 'transient-value)))
;;   ;;:choices '("On" "Off" "Best")

;;   )

;;(setq message-forward-as-mime 'best)

;;(mu4e--define-toggle-suffix "Show MML"  message-forward-show-mml)

(transient-define-prefix mu4e--prefix-compose-options()
  "Mu4e debugging menu."
  [["Formatting"
    ("f" mu4e--suffix-toggle-mu4e-compose-format-flowed)]
   ["Forwarding"
    ("m" mu4e--suffix-toggle-message-forward-as-mime)
    ;; handle 'best'
    ;;("M" mu4e--suffix-toggle-message-forward-show-mml)
    ]])

(transient-define-prefix mu4e--prefix-mailing-list()
  "Mu4e documentation menu."
  [[
    :description
    (lambda ()
      (let* ((msg (mu4e-message-at-point 'no-error))
             (ml (plist-get msg :list)))
        (format "Mailing list %s" (or ml ""))))
    ("s" "subscribe"   gnus-mailing-list-subscribe)
    ("u" "unsubscribe" gnus-mailing-list-unsubscribe)
    ("h" "help"        gnus-mailing-list-help)
    ;;("a" "archive")
]])

(transient-define-prefix mu4e-transient-menu()
  "Mu4e main menu."
  [["Main"
    ("M" "Main screen"         mu4e)
    ("U" "Update mail"         mu4e-update-mail-and-index)
    (";" "Change context"      mu4e-context-switch
     :inapt-if-nil mu4e-contexts
     :description
     (lambda ()
       (let ((ctx (mu4e-context-current)))
         (format "Context %s"
                 (if ctx (propertize (mu4e-context-name ctx)
                                     'face 'transient-value) "")))))
    ("d" "Docs & links"      mu4e--prefix-docs-links)
    ("D" "Debug/tweaks..."   mu4e--prefix-debug-tweaks)
    ("M-q" "Quit"             mu4e-quit)]
   ["Search"
    ("b" "Bookmark"            mu4e-search-bookmark)
    ("j" "Maildir"             mu4e-search-maildir)
    ("c" "Choose query"        mu4e-search-query)
    ("s" "Search"              mu4e-search)]
   ["Composition"
    ("C" "New message"         mu4e-compose-new)
    ("R" "Reply"               mu4e-compose-reply :if mu4e-message-p)
    ("W" "Reply-to-all"        mu4e-compose-wide-reply :if mu4e-message-p)
    ("F" "Forward"             mu4e-compose-forward :if mu4e-message-p)
    ;; only draft messages can be edited
    ("E" "Edit draft"          mu4e-compose-edit
     :if mu4e-message-p
     :inapt-if-not (lambda ()
                     (member 'draft
                             (mu4e-message-field
                              (mu4e-message-at-point 'nowarn) :flags))))
    ;; you can only supersede your own messages
    ("S" "Supersede"           mu4e-compose-supersede
     :if mu4e-message-p
     :inapt-if-not mu4e--message-is-yours-p)
    ("X" "Resend"          mu4e-compose-resend :if mu4e-message-p)
    ("o" "Options..."      mu4e--prefix-compose-options)]
   ["" ;; composition that requires an existing message
    :if           (lambda ()
                    (memq major-mode '(mu4e-headers-mode mu4e-view-mode)))
    :inapt-if-not (lambda ()
                    (mu4e-message-at-point 'nowarn))
    ]
   ["View"
    :if (lambda () (eq major-mode 'mu4e-view-mode))
    ("m" "Mailing list..."  mu4e--prefix-mailing-list)]])

(provide 'mu4e-transient)
;;; mu4e-transient.el ends here
