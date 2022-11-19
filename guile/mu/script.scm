;; Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
(define-module (mu script)
  :export (mu:run-stats))

(use-modules (ice-9 getopt-long) (ice-9 optargs) (ice-9 popen) (ice-9 format))
(use-modules (mu) (mu stats) (mu plot))

(define (help-and-exit)
  "Show some help."
  (display
    (string-append "usage: script [--help] [--textonly] "
      "[--muhome=<muhome>] [--query=<query>")
    (newline))
  (exit 0))

(define (mu:run-stats args func)
  "Run some statistics function.
Interpret argument-list ARGS (like command-line
arguments). Possible arguments are:
  --help (show some help and exit)
  --muhome (path to alternative mu home directory)
  --output (a string describing the output, e.g. \"dumb\", \"png\" \"wxt\")
  searchexpr (a search query)
then call FUNC with args SEARCHEXPR and OUTPUT."
  (setlocale LC_ALL  "")
  (let* ((optionspec '((muhome  (value #t))
		       (query   (value #t))
		       (output  (value #f))
		       (time-unit (value #t)) ;; Ignore.
		       (help    (single-char #\h) (value #f))))
	 (options (getopt-long args optionspec))
	 (query (option-ref options 'query #f))
	 (help (option-ref options 'help #f))
	 (output (option-ref options 'output #f))
	 (muhome (option-ref options 'muhome #f))
	 (restargs (option-ref options '() #f)))
    (if help (help-and-exit))
    (mu:initialize muhome)
    (func (or query "") output)))

;; Local Variables:
;; mode: scheme
;; End:
