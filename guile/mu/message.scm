;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

(define-module (mu message)
  :use-module (oop goops)
  :export ( ;; classes
	    <mu:message>
	    mu:for-each-message
	    mu:message-list
	    ;; internal
	    mu:get-header
	    mu:get-field
	    mu:for-each-msg-internal
	    ;; message funcs
	    header
	    ;; other symbols
	    mu:field:bcc
	    mu:field:body-html
	    mu:field:body-txt
	    mu:field:cc
	    mu:field:date
	    mu:field:flags
	    mu:field:from
	    mu:field:maildir
	    mu:field:message-id
	    mu:field:path
	    mu:field:prio
	    mu:field:refs
	    mu:field:size
	    mu:field:subject
	    mu:field:tags
	    mu:field:to))

(load-extension "libguile-mu" "mu_guile_message_init")

(define-class <mu:message> ()
  (msg  #:init-keyword #:msg)) ;; the MuMsg-smob we're wrapping

(define-syntax define-getter
  (syntax-rules ()
    ((define-getter method-name field)
      (begin
	(define-method (method-name (msg <mu:message>))
	  (mu:get-field (slot-ref msg 'msg) field))
	(export method-name)))))

(define-getter mu:bcc	     mu:field:bcc)
(define-getter mu:body-html  mu:field:body-html)
(define-getter mu:body-txt   mu:field:body-txt)
(define-getter mu:cc	     mu:field:cc)
(define-getter mu:date	     mu:field:date)
(define-getter mu:flags	     mu:field:flags)
(define-getter mu:from	     mu:field:from)
(define-getter mu:maildir    mu:field:maildir)
(define-getter mu:message-id mu:field:message-id)
(define-getter mu:path	     mu:field:path)
(define-getter mu:priority   mu:field:prio)
(define-getter mu:references mu:field:refs)
(define-getter mu:size	     mu:field:size)
(define-getter mu:subject    mu:field:subject)
(define-getter mu:tags	     mu:field:tags)
(define-getter mu:to	     mu:field:to)


(define-method (header (msg <mu:message>) (hdr <string>))
  "Get an arbitrary header HDR from message MSG; return #f if it does
not exist."
  (mu:get-header (slot-ref msg 'msg) hdr))

(define* (mu:for-each-message func #:optional (expr #t))
  "Execute function FUNC for each message that matches mu search expression EXPR.
If EXPR is not provided, match /all/ messages in the store."
    (mu:for-each-msg-internal
      (lambda (msg)
	(func (make <mu:message> #:msg msg)))
      expr))

(define* (mu:message-list #:optional (expr #t))
  "Return a list of all messages matching mu search expression
EXPR. If EXPR is not provided, return a list of /all/ messages in the store."
  (let ((lst '()))
    (mu:for-each-message
      (lambda (m)
	(set! lst (append! lst (list m)))) expr)
    lst))
