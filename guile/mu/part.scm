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

(define-module (mu part)
  :use-module (oop goops)
  :use-module (mu)
  :use-module (mu message)
  :export (;; get-part
	    ;; classes
	    <mu-part>
	    ;; message function
	    attachments
	    parts
	    ;; <mu-part> methods
	    index
	    name
	    mime-type
;;	    size
	    save
	    save-as))

(define-class <mu-part> ()
  (msgpath   #:init-value #f #:init-keyword #:msgpath)
  (index     #:init-value #f #:init-keyword #:index)
  (name      #:init-value #f #:getter name #:init-keyword #:name)
  (mime-type #:init-value #f #:getter mime-type #:init-keyword #:mime-type)
  (size      #:init-value 0  #:getter size #:init-keyword #:size))

(define-method (get-parts (msg <mu-message>) (files-only <boolean>))
    "Get the part for MSG as a list of <mu-part> objects; if FILES-ONLY is #t,
only get the part with file names."
  (map (lambda (part)
	 (make <mu-part>
	   #:msgpath    (list-ref part 0)
	   #:index      (list-ref part 1)
	   #:name       (list-ref part 2)
	   #:mime-type  (list-ref part 3)
	   #:size       (list-ref part 4)))
    (mu:get-parts (slot-ref msg 'msg) files-only)))

(define-method (attachments (msg <mu-message>))
  "Get the attachments for MSG as a list of <mu-part> objects."
  (get-parts msg #t))

(define-method (parts (msg <mu-message>))
  "Get the MIME-parts for MSG as a list of <mu-part> objects."
  (get-parts msg #f))

(define-method (save (part <mu-part>))
  "Save PART to a temporary file, and return the file name. If the
part had a filename, the temporary file's file name will be just that;
otherwise a name is made up."
  (mu:save-part (slot-ref part 'msgpath) (slot-ref part 'index)))

(define-method (save-as (part <mu-part>) (filepath <string>))
  "Save message-part PART to file system path PATH."
    (copy-file (save part) filepath))
