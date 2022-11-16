#!/bin/sh
exec guile -e main -s $0 $@
!#
;; Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


;; INFO: Count the number of messages matching some query
;; INFO: options:
;; INFO:   --query=<query>:   limit to messages matching query
;; INFO:   --muhome=<muhome>: path to mu home dir (optional)

(use-modules (mu) (mu script) (mu stats))

(define (count expr output)
  "Print the total number of messages matching the query EXPR.
OUTPUT is ignored."
  (display (mu:count expr))
  (newline))

(define (main args)
  (mu:run-stats args count))

;; Local Variables:
;; mode: scheme
;; End:
