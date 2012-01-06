;;; org-mu4e -- Support for links to mu4e messages/queries from within org-mode
;;
;; Copyright (C) 2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: outlines, hypermedia, calendar, mail
;; Version: 0.0

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'mu4e))

(defun org-mu4e-store-link ()
  "Store a link to a mu4e query or message."
  (cond
    ;; storing links to queries
    ((eq major-mode 'mu4e-hdrs-mode)
      (let* ((query mu4e-last-expr)
	      desc link)
	(org-store-link-props :type "mu4e" :query query)
	(setq
	  desc (org-make-link "mu4e:query:" query)
	  link desc)
	(org-add-link-props :link link :description desc)
	link))

      ;; storing links to messages
    ((eq major-mode 'mu4e-view-mode)
      (let* ((msg mu4e-current-msg)
	      (msgid   (or (plist-get msg :message-id) "<none>"))
	      (subject (or (plist-get msg :subject) "No subject"))
	      link)
	(org-store-link-props :type "mu4e" :link link
	  :message-id msgid :subject subject)
	(setq link (org-make-link "mu4e:msgid:" msgid))
	(org-add-link-props :link link :description subject)
	link))))

(org-add-link-type "mu4e" 'org-mu4e-open)
(add-hook 'org-store-link-functions 'org-mu4e-store-link)

(defun org-mu4e-open (path)
  "Open the mu4e message (for paths starting with 'msgid:') or run
the query (for paths starting with 'query:')."
  (require 'mu4e)
  (cond
    ((string-match "^msgid:\\(.+\\)" path)
      (mu4e-view-message-with-msgid (match-string 1 path)))
    ((string-match "^query:\\(.+\\)" path)
      (mu4e-search (match-string 1 path)))
    (t (message "mu4e: unrecognized link type '%s'" path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-mu4e)

;;; org-mu4e.el ends here
