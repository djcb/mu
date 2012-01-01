;;
;; Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

;; some guile/scheme functions to get various statistics of my mu
;; message store.

(define-module (mu contact)
  :use-module (oop goops)
  :export ( ;; classes
	    <mu-contact>
	    ;; contact methods
	    name email timestamp frequency last-seen
	    ))

(define-class <mu-contact> ()
  (name #:init-value #f #:accessor name #:init-keyword #:name)
  (email #:init-value #f #:accessor email #:init-keyword #:email)
  (tstamp #:init-value 0 #:accessor timestamp #:init-keyword #:timestamp)
  (last-seen #:init-value 0 #:accessor last-seen)
  (freq #:init-value 1 #:accessor frequency))
