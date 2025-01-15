;;; mu4e-transient.el --- -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;;; Commentary:
;;;
;;; Define "transients" for some mu4e menus.

;;; Code:
(require 'mu4e)
(require 'mu4e-bookmarks)
(require 'mu4e-compose)
(require 'mu4e-draft)

(require 'transient)

(transient-define-prefix mu4e-transient-menu-docs-links()
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

(transient-define-prefix mu4e-transient-menu-debug()
  "Mu4e debugging menu."
  [["Debugging"
    ("$" "Toggle logging" mu4e-toggle-logging)
    ("," "Message sexp"   mu4e-sexp-at-point
     :inapt-if-not (lambda () (mu4e-message-at-point 'nowarn)))
    ("l" "Last query sexp" mu4e-analyze-last-query)]])

(transient-define-prefix mu4e-transient-menu()
  "Mu4e main menu."
  [["Basics"
    ("M" "Main menu"           mu4e)
    ("U" "Update mail"         mu4e-update-mail-and-index)
    (";" "Switch context"      mu4e-context-switch
     :inapt-if-nil mu4e-contexts)
    ("M-q" "Quit mu4e"         mu4e-quit)]
   ["Search"
    ("b" "Bookmark"            mu4e-search-bookmark)
    ("j" "Maildir"             mu4e-search-maildir)
    ("c" "Choose query"        mu4e-search-query)
    ("s" "Search"              mu4e-search-query)]
   ["Composition"
    ("C" "New mail"            mu4e-compose-new)]
   ["" ;; composition that requires an existing message
    :if           (lambda () (memq major-mode '(mu4e-headers-mode mu4e-view-mode)))
    :inapt-if-not (lambda () (mu4e-message-at-point 'nowarn))
    ("R" "Reply"               mu4e-compose-reply)
    ("W" "Reply-to-all"        mu4e-compose-wide-reply)
    ("F" "Forward"             mu4e-compose-forward)
    ;; only draft messages can be edited
    ("E" "Edit draft"          mu4e-compose-edit
     :inapt-if-not (lambda ()
           (member 'draft
                   (mu4e-message-field
                    (mu4e-message-at-point 'nowarn) :flags))))
    ;; you can only supersede your own messages
    ("S" "Supersede"           mu4e-compose-supersede
     :inapt-if-not mu4e--message-is-yours-p)
    ("X" "Resend"          mu4e-compose-resend)]
   ["Misc"
    ("d" "Docs & links"    mu4e-transient-menu-docs-links)
    ("D" "Debugging"       mu4e-transient-menu-debug)]])

(provide 'mu4e-transient)
;;; mu4e-transient.el ends here
