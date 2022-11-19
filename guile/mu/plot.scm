;;
;; Copyright (C) 2011-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

(define-module (mu plot)
  :use-module (mu)
  :use-module (ice-9 popen)
  :export ( mu:plot ;; alias for mu:plot-histogram
	    mu:plot-histogram
	    ))

(define (export-pairs pairs)
  "Write a temporary file with the list of PAIRS in table format, and
return the file name."
  (let* ((output (mkstemp "/tmp/mu-guile-XXXXXX" "w"))
	 (datafile (port-filename output)))
    (for-each
     (lambda(pair)
       (display (format #f "~a ~a\n" (car pair) (cdr pair)) output))
     pairs)
    (close output)
    datafile))

(define (find-program-in-path prog)
  "Find exutable program PROG in PATH; return the full path, or #f if
not found."
  (let* ((path (parse-path (getenv "PATH")))
	  (progpath (search-path path prog)))
    (if (not progpath)
      #f
      (if (access? progpath X_OK) ;; is
	progpath
	#f))))

(define* (mu:plot-histogram data title x-label y-label
	   #:optional (output "dumb") (extra-gnuplot-opts '()))
  "Plot DATA with TITLE, X-LABEL and X-LABEL using the gnuplot
program. DATA is a list of cons-pairs (X . Y).

 OUTPUT is a string
that determines the type of output that gnuplot produces, depending on
the system. Which options are available depends on the particulars for
the gnuplot installation, but typical examples would be \"dumb\" for
text-only display, \"wxterm\" to write to a graphical window, or
\"png\" to write a PNG-image to stdout.

EXTRA-GNUPLOT-OPTS is a list
of any additional options for gnuplot."
  (if (not (find-program-in-path "gnuplot"))
    (error "cannot find 'gnuplot' in path"))
  (when (zero? (length data))
    (error "No data for plotting"))
  (let* ((datafile (export-pairs data))
	 (gnuplot (open-pipe "gnuplot -p" OPEN_WRITE))
	 (recipe
	   (string-append
	    "reset\n"
	    "set term " (or output "dumb") "\n"
	    "set title \"" title "\"\n"
	    "set xlabel \"" x-label "\"\n"
	    "set ylabel \"" y-label "\"\n"
	    "set boxwidth 0.9\n"
	    (string-join extra-gnuplot-opts "\n")
	    "plot \"" datafile "\" using 2:xticlabels(1) with boxes fs solid title \"\"\n")))
    (display recipe gnuplot)
    (close-pipe gnuplot)))

;; backward compatibility
(define mu:plot mu:plot-histogram)
