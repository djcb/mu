;;; mu4e-view-html.el -- part of mu4e  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Render a message as html. The message's html part is used if it has one;
;; otherwise, the text-part is converted.

;;; Code:

(require 'mm-decode)
(require 'gnus-art)
(require 'mu4e-helpers)
(require 'mu4e-message)

;;; HTML fragments

(defconst mu4e--view-html-meta
  (concat "<meta charset=\"utf-8\">"
          "<meta http-equiv=\"Content-Security-Policy\" content=\""
          ;; note: no "default-src 'none'"; webkit does not like it
          "script-src 'none'; object-src 'none'; frame-src 'none'; "
          "connect-src 'none'; media-src 'none'; form-action 'none'; "
          "style-src 'unsafe-inline'; img-src data:\">")
  "HTML meta tags for the head of the document. Block scripts /
remote stuff.")

(defconst mu4e--view-html-head-template "<head>%s</head>"
  "Template for the head of an HTML document.
The %s is replaced by the head.")

(defconst mu4e--view-html-document-template
  (concat "<html>" mu4e--view-html-head-template "<body>%s</body></html>")
  "Template for a complete HTML document.
The %s are replaced by the head content and the body content.")

(defconst mu4e--view-html-plain-text-style
  "<style>pre{font-family:monospace;white-space:pre-wrap}</style>"
  "Style for the <pre> block.")

(defconst mu4e--view-html-plain-text-template "<pre>%s</pre>"
  "Template for wrapping a text message body.")

(defconst mu4e--view-html-link-template "<a href=\"%s\">%s</a>"
  "Template for an HTML link.
%s %s for link, description.")

(defconst mu4e--view-html-headers-pre
  (concat "<div style=\"font-family:sans-serif;font-size:90%;padding:8px;"
          "margin-bottom:8px;border-bottom:1px solid #ccc;"
          "background:#f8f8f8\">\n")
  "HTML fragment pre message-headers.")

(defconst mu4e--view-html-headers-post
  ;; the empty paragraph is invisible in web-browsers but a nice empty
  ;; line in shr.
  "</div>\n<p style=\"margin:0\"></p>\n"
  "HTML fragment post header.")

(defconst mu4e--view-html-header-template "<b>%s:</b> %s<br>\n"
  "Template for a header.
The %s, %s for key, value.")

;;; Escaping and links

(defun mu4e--view-html-escape (text)
  "Escape TEXT for including in HTML."
  (seq-reduce (lambda (text pair)
                (replace-regexp-in-string (car pair) (cdr pair) text t t))
              '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))
              text))

(defconst mu4e--view-url-regexp
  (rx "http" (? "s") "://"
      (* (any "-a-zA-Z0-9._~%#?&=/+:;@!$*(),'"))
      (any "-a-zA-Z0-9_~%#&=/+@$'"))
  "Regexp matching URLs.")

(defconst mu4e--view-email-regexp
  (rx (any "a-zA-Z0-9") (* (any "-a-zA-Z0-9._%+"))
      "@" (+ (any "-a-zA-Z0-9.")) "." (>= 2 alpha))
  "Regexp matching e-mail addresses.")

(defconst mu4e--view-linkable-regexp
  (rx (or (regexp mu4e--view-url-regexp)
          (regexp mu4e--view-email-regexp)))
  "Regexp matching linkable things.")

(defun mu4e--view-linkable-url (match)
  "Return the URL for MATCH."
  (if (string-match-p (rx bos (regexp mu4e--view-email-regexp) eos)
                      match)
      (concat "mailto:" match)
    match))

(defun mu4e--view-linkify-html (text)
  "Turn URLs/e-mail addresses into HTML links."
  (replace-regexp-in-string
   mu4e--view-linkable-regexp
   (lambda (match)
     (format mu4e--view-html-link-template
             (mu4e--view-linkable-url match) match))
   text t t))

;;; MIME parts

(defun mu4e--view-mime-part (handles media-type)
  "Find the first MIME part with MEDIA-TYPE in HANDLES.
Return the handle, or nil."
  (cond
   ((not (listp handles)) nil)
   ((bufferp (car handles))
    (when (equal (mm-handle-media-type handles) media-type)
      handles))
   (t (seq-some (lambda (handle)
                  (mu4e--view-mime-part handle media-type))
                (cdr handles)))))

(defun mu4e--view-mime-part-string (handle)
  "Return MIME part HANDLE string."
  (let* ((charset (mail-content-type-get (mm-handle-type handle) 'charset))
         (coding (and charset (mm-charset-to-coding-system charset)))
         (coding (if (memq coding '(nil ascii)) 'utf-8 coding)))
    (decode-coding-string (mm-get-part handle) coding)))

(defun mu4e--view-cid-parts (handles)
  "Collect all MIME parts in HANDLES with Content-ID.
Return alist of (CID . HANDLE) pairs."
  (cond
   ((not (listp handles)) nil)
   ((bufferp (car handles))
    (when-let* ((id (mm-handle-id handles)))
      ;; strip the angle brackets from the content-id
      (list (cons (replace-regexp-in-string
                   (rx (or (seq bos "<") (seq ">" eos))) "" id)
                  handles))))
   (t (seq-mapcat #'mu4e--view-cid-parts (cdr handles)))))

(defun mu4e--view-attachment-names (handles)
  "Get filenames of the attachment MIME parts in HANDLES."
  (cond
   ((not (listp handles)) nil)
   ((bufferp (car handles))
    (when-let* ((name (mm-handle-filename handles)))
      (list name)))
   (t (seq-mapcat #'mu4e--view-attachment-names (cdr handles)))))

(defun mu4e--view-resolve-cids (html cid-parts)
  "Replace \"cid:\" references in HTML with data:-URIs.
Return updated HTML."
  (dolist (part cid-parts html)
    (let* ((handle (cdr part))
           (data-uri (format "data:%s;base64,%s"
                             (mm-handle-media-type handle)
                             (base64-encode-string
                              (mm-get-part handle) t))))
      (setq html (replace-regexp-in-string
                  (regexp-quote (concat "cid:" (car part)))
                  data-uri html t t)))))

;;; Document structure

(defun mu4e--view-html-insert-after-tag (html tag text)
  "Insert TEXT after the first (opening) TAG in HTML.
Return updated HTML, or nil."
  (let ((case-fold-search t)
        (regexp (rx-to-string
                 `(seq "<" ,tag (or ">" (seq space (* (not (any ">"))) ">")))
                 t)))
    (when (string-match regexp html)
      (replace-match (concat (match-string 0 html) text) t t html))))

(defun mu4e--view-html-inject-meta (html)
  "Insert `mu4e--view-html-meta' into the head of HTML.
Add head or doc structure. Return updated HTML."
  (or (mu4e--view-html-insert-after-tag html "head" mu4e--view-html-meta)
      (mu4e--view-html-insert-after-tag
       html "html" (format mu4e--view-html-head-template
                           mu4e--view-html-meta))
      (format mu4e--view-html-document-template
              mu4e--view-html-meta html)))

(defun mu4e--view-html-prepend-headers (html headers)
  "Insert the HEADERS block into HTML."
  (or (mu4e--view-html-insert-after-tag html "body" headers)
      (mu4e--view-html-insert-after-tag html "/head" headers)
      (concat headers html)))

;;; Message headers

(defun mu4e--view-html-headers (headers)
  "Create HTML block for HEADERS.
HEADERS is an alist of (NAME . VALUE)."
  (concat
   mu4e--view-html-headers-pre
   (mapconcat
    (lambda (header)
      (format mu4e--view-html-header-template
              (mu4e--view-html-escape (car header))
              (mu4e--view-html-escape (cdr header))))
    headers "")
   mu4e--view-html-headers-post))

;;; Rendering

(defun mu4e--view-html-body (handles)
  "Return HTML for the message body in HANDLES, or nil.
Use html-part or text-part if there is none."
  (if-let* ((handle (mu4e--view-mime-part handles "text/html")))
      (mu4e--view-mime-part-string handle)
    (when-let* ((handle (mu4e--view-mime-part handles "text/plain")))
      (format mu4e--view-html-document-template
              mu4e--view-html-plain-text-style
              (format mu4e--view-html-plain-text-template
                      (mu4e--view-linkify-html
                       (mu4e--view-html-escape
                        (mu4e--view-mime-part-string handle))))))))

(defun mu4e--view-html-render (handles &optional headers)
  "Return a self-contained HTML document for MIME HANDLES, or nil.
Prepend HEADERS if non-nil."
  (when-let* ((html (mu4e--view-html-body handles)))
    (when-let* ((cid-parts (mu4e--view-cid-parts handles)))
      (setq html (mu4e--view-resolve-cids html cid-parts)))
    (setq html (mu4e--view-html-inject-meta html))
    (if headers
        (mu4e--view-html-prepend-headers html headers)
      html)))

(defun mu4e-view-html-text (msg &optional headers)
  "Return an HTML rendering of MSG as a string.
Either the html part or text if there isn't. Prepend headers, if
any.

Return text or nil if the message has neither an html nor a
plain-text part."
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-readable-path msg) nil nil nil t)
    ;; just continue if some of the decoding fails.
    (ignore-errors (run-hooks 'gnus-article-decode-hook))
    (let ((handles (mm-dissect-buffer t t)))
      (unwind-protect
          (mu4e--view-html-render
           handles
           (when headers
             (when-let* ((attachments (mu4e--view-attachment-names handles)))
               (setq headers
                     (append headers
                             (list (cons "Attachments"
                                         (string-join attachments ", "))))))
             (mu4e--view-html-headers headers)))
        (mm-destroy-parts handles)))))

(defun mu4e--view-html-temp-file (html)
  "Save HTML to a temporary file and return its path."
  (let* ((temporary-file-directory (or mu4e--temp-dir
                                       temporary-file-directory))
         (tmpfile (make-temp-file "mu4e-msg-" nil ".html"))
         (coding-system-for-write 'utf-8))
    (with-temp-file tmpfile
      (insert html))
    tmpfile))

(provide 'mu4e-view-html)
;;; mu4e-view-html.el ends here
