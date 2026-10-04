;;; mu-man-texi.el --- Export the mu man-pages to Texinfo -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dirk-Jan C. Binnema

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Turn the org-mode sources of the mu man-pages into a single Texinfo file, for
;; including in the mu4e manual.
;;
;; The plain `texinfo' exporter maps each headline to a node and a sectioning
;; command, which does not work well for man-pages, so re-derive something
;; a bit more reasonable.

;;; Code:

(require 'ox-texinfo)

(defconst mu-man-texi-skip-sections
  '("AUTHOR" "REPORTING BUGS" "COPYRIGHT")
  "Top-level man-page sections to leave out.")

(defvar mu-man-texi--pages nil
  "List of (NAME . SECTION) for the man-pages being exported.")

(defun mu-man-texi--page (file)
  "Get (NAME . SECTION) for man-page org-FILE."
  (let ((base (file-name-base file))) ;; e.g., "mu-find.1"
    (cons (file-name-sans-extension base)
          (file-name-extension base))))

(defun mu-man-texi--node (name)
  "Get the Texinfo node name for man-page NAME."
  (format "%s man-page" name))

(defun mu-man-texi--man-link (name section)
  "Return an org-snippet for referring to man-page NAME in SECTION.
Format it like man(1) would."
;; We use @link rather than @ref, since the latter adds
;; \"see\"/\"*note\" to the info-output, which doesn't fit in
;; running text.
;;; man-link is sometimes used with "mu cmd" rather than "mu-cmd".
  (let* ((page (string-replace " " "-" (string-trim name)))
         (section (string-trim section))
         (label (format "@strong{%s}(%s)" name section)))
    (format "@@texinfo:%s@@"
            (if (member (cons page section) mu-man-texi--pages)
                (format "@link{%s,%s}" (mu-man-texi--node page) label)
              label))))

(defun mu-man-texi--prepare (_backend)
  "Prepare the buffer for export."
  (org-export-expand-include-keyword)
  (goto-char (point-min))
  (while (re-search-forward "^#\\+macro: +man-link .*$" nil t)
    (replace-match
     "#+MACRO: man-link (eval (mu-man-texi--man-link $1 $2))" t t)))

(defun mu-man-texi--plain-text (text _backend _info)
  "Filter for plain TEXT.
Remove zero-width spaces, and keep Texinfo from turning -- into a
dash."
  (string-replace "--" "-@w{}-"
                  (string-replace (string #x200b) "" text)))

(defun mu-man-texi--headline (headline contents info)
  "Transcode HEADLINE with CONTENTS into Texinfo; INFO is the plist.
Top-level headlines become headings; second-level ones become
table items. Headlines in `mu-man-texi-skip-sections' are dropped."
  (let ((level (org-export-get-relative-level headline info))
        (title (org-export-data (org-element-property :title headline) info))
        (contents (or contents "")))
    (cond
     ((= level 1)
      (unless (member (org-element-property :raw-value headline)
                      mu-man-texi-skip-sections)
        (format "@subheading %s\n\n%s" title contents)))
     (t ;; wrap consecutive sibling headlines in a table.
      (let ((first (not (eq (org-element-type
                             (org-export-get-previous-element headline info))
                            'headline)))
            (last (not (eq (org-element-type
                            (org-export-get-next-element headline info))
                           'headline))))
        (concat (when first "@table @asis\n")
                (format "@item %s\n%s" title contents)
                (when last "@end table\n\n")))))))

(defun mu-man-texi--section (_section contents _info)
  "Transcode a section with CONTENTS.
Unlike the `texinfo' backend, do not add a menu, since our
headlines are not nodes."
  contents)

(defun mu-man-texi--smaller (block)
  "Turn Texinfo BLOCK (@example/@lisp) into its smaller variant.
This only affects printed (PDF) output, where many of the lines
in the man-page examples would not fit otherwise."
  (replace-regexp-in-string
   "^@\\(end \\)?\\(example\\|lisp\\)$" "@\\1small\\2" block))

(defun mu-man-texi--example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK with CONTENTS; INFO is the plist."
  (mu-man-texi--smaller
   (org-texinfo-example-block example-block contents info)))

(defun mu-man-texi--src-block (src-block contents info)
  "Transcode SRC-BLOCK with CONTENTS; INFO is the plist."
  (mu-man-texi--smaller
   (org-texinfo-src-block src-block contents info)))

(defun mu-man-texi--literal-bullets-p (plain-list)
  "Non-nil if PLAIN-LIST is ordered, but not numbered 1, 2, 3, ...
E.g., the list of exit codes. Org (and Texinfo) would renumber
those, but man(1) shows them as they are, and so do we."
  (and (eq (org-element-property :type plain-list) 'ordered)
       (let ((n 0))
         (seq-some (lambda (item)
                     (/= (string-to-number (org-element-property :bullet item))
                         (setq n (1+ n))))
                   (org-element-contents plain-list)))))

(defun mu-man-texi--plain-list (plain-list contents info)
  "Transcode PLAIN-LIST with CONTENTS; INFO is the plist.
Lists with literal bullets become a table."
  (if (mu-man-texi--literal-bullets-p plain-list)
      (format "@table @asis\n%s@end table" contents)
    (org-texinfo-plain-list plain-list contents info)))

(defun mu-man-texi--item (item contents info)
  "Transcode ITEM with CONTENTS; INFO is the plist."
  (if (mu-man-texi--literal-bullets-p (org-export-get-parent item))
      (format "@item %s\n%s" (string-trim (org-element-property :bullet item))
              (or contents ""))
    (org-texinfo-item item contents info)))

(org-export-define-derived-backend 'mu-man-texi 'texinfo
  :translate-alist '((headline . mu-man-texi--headline)
                     (section . mu-man-texi--section)
                     (example-block . mu-man-texi--example-block)
                     (src-block . mu-man-texi--src-block)
                     (plain-list . mu-man-texi--plain-list)
                     (item . mu-man-texi--item))
  :filters-alist '((:filter-plain-text . mu-man-texi--plain-text)))

(defun mu-man-texi--export (file)
  "Export man-page org-FILE to a Texinfo string, with its own node."
  (let* ((page (mu-man-texi--page file))
         (name (car page))
         (section (cdr page))
         (org-export-with-sub-superscripts '{})
         (org-export-with-toc nil)
         ;; man renders underlined text in italics
         (org-texinfo-text-markup-alist
          (cons '(underline . "@emph{%s}") org-texinfo-text-markup-alist))
         (org-export-before-processing-functions
          (list #'mu-man-texi--prepare)))
    (with-temp-buffer
      (insert-file-contents file)
      (setq default-directory (file-name-directory (expand-file-name file)))
      (org-mode)
      (concat
       (format "@page\n@node %s\n@appendixsec %s(%s)\n\n"
               (mu-man-texi--node name) name section)
       (org-export-as 'mu-man-texi nil nil t)
       "\n"))))

(defun mu-man-texi-export (output files)
  "Export man-page org-FILES into a single Texinfo file OUTPUT.
The result has a menu, followed by a node for each of the FILES."
  (let ((mu-man-texi--pages (mapcar #'mu-man-texi--page files)))
    (with-temp-file output
      (insert "@c generated by mu-man-texi.el; do not edit\n\n"
              "@menu\n")
      (dolist (page mu-man-texi--pages)
        (insert (format "* %s:: %s(%s)\n"
                        (mu-man-texi--node (car page)) (car page) (cdr page))))
      (insert "@end menu\n\n")
      (dolist (file files)
        (insert (mu-man-texi--export file))))))

(defun mu-man-texi-batch ()
  "Batch entry point: OUTPUT FILE... in `command-line-args-left'."
  (let ((output (pop command-line-args-left))
        (files command-line-args-left))
    (setq command-line-args-left nil)
    (mu-man-texi-export output files)))

(provide 'mu-man-texi)
;;; mu-man-texi.el ends here
