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

(define-module (mu)
  :export
  ( mu:initialize))

;; :use-module (oop goops)
  ;; :export
  ;; (mu:for-each-contact
  ;;  mu:for-each-message
  ;;  mu:message-list
  ;;  mu:tabulate-messages
  ;;  mu:average-messages
  ;;   <mu-message>
  ;;   ;; message funcs
  ;;   body
  ;;   header
  ;;   contacts

  ;;   ;; classes
  ;;   <mu-contact>
  ;;   ;; contact methods
  ;;   name email timestamp frequency last-seen

(load-extension "libguile-mu" "mu_guile_init")
