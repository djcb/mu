;; Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
(use-modules (system repl server)
	     (ice-9 threads))
(use-modules (mu))

;; when a socket path is defined, listen on it (blocking)
;; after printing UNIX-CONNECT:<socket-file>\n on stdout
(let ((socket-path (getenv "MU_SCM_SOCKET_PATH")))
  (when socket-path
    (format #t "~a\n" socket-path)
    (run-server
     (make-unix-domain-server-socket #:path socket-path))))

(display "Welcome to the mu shell!\n\n")
