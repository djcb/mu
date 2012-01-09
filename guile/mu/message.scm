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
	    <mu-message>
	    mu:for-each-message
	    mu:message-list
	    ;; internal
	    mu:get-header
	    mu:get-field
	    ;; message funcs
	    body
	    header
	    ;; other symbols
	    mu:bcc
	    mu:body-html
	    mu:body-txt
	    mu:cc
	    mu:date
	    mu:flags
	    mu:from
	    mu:maildir
	    mu:message-id
	    mu:path
	    mu:prio
	    mu:refs
	    mu:size
	    mu:subject
	    mu:tags
	    mu:to))

(load-extension "libguile-mu" "mu_guile_message_init")

(define-class <mu-message> ()
  (msg  #:init-keyword #:msg)) ;; the MuMsg-smob we're wrapping

(define-syntax define-getter
  (syntax-rules ()
    ((define-getter method-name field)
      (begin
	(define-method (method-name (msg <mu-message>))
	  (mu:get-field (slot-ref msg 'msg) field))
	(export method-name)))))

(define-getter bcc	  mu:bcc)
(define-getter body-html  mu:body-html)
(define-getter body-txt	  mu:body-txt)
(define-getter cc	  mu:cc)
(define-getter date	  mu:date)
(define-getter flags	  mu:flags)
(define-getter from	  mu:from)
(define-getter maildir	  mu:maildir)
(define-getter message-id mu:message-id)
(define-getter path	  mu:path)
(define-getter priority	  mu:prio)
(define-getter references mu:refs)
(define-getter size	  mu:size)
(define-getter subject	  mu:subject)
(define-getter tags	  mu:tags)
(define-getter to	  mu:to)

(define-method (header (msg <mu-message>) (hdr <string>))
  "Get an arbitrary header HDR from message MSG."
  (mu:get-header (slot-ref msg 'msg) hdr))

(define* (mu:for-each-message func #:optional (expr #t))
  "Execute function FUNC for each message that matches mu search expression EXPR.
If EXPR is not provided, match /all/ messages in the store."
  (let ((my-func
	  (lambda (msg)
	    (func (make <mu-message> #:msg msg)))))
    (mu:for-each-msg-internal my-func expr)))

(define* (mu:message-list #:optional (expr #t))
  "Return a list of all messages matching mu search expression
EXPR. If EXPR is not provided, return a list of /all/ messages in the store."
  (let ((lst '()))
    (mu:for-each-message
      (lambda (m)
	(set! lst (append! lst (list m)))) expr)
    lst))
