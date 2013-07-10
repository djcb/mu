(require 'coolj)

(defun mu4e~wash-fix-microsoft ()
  (save-excursion
    (when (string-match-p "[]" (buffer-string))
      (recode-region (point-min) (point-max) 'windows-1252 'iso-8859-1))))

(defun mu4e~wash-cr ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match "" t t))
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (replace-match "\n" t t)))))

(defun mu4e~wash-ansi-colors ()
  (save-excursion
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun mu4e~wash-elide-blank-lines ()
  
  ;; Make all blank lines empty.
  (goto-char (point-min))
  (while (re-search-forward "^[[:space:]\t]+$" nil t)
    (replace-match "" nil t))

  ;; Replace multiple empty lines with a single empty line.
  (goto-char (point-min))
  (while (re-search-forward "^\n\\(\n+\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))

  ;; Remove a leading blank line.
  (goto-char (point-min))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0)))

  ;; Remove a trailing blank line.
  (goto-char (point-max))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0))))

(defun mu4e~wash-tidy-citations ()
  "(from notmuch) Improve the display of cited regions of a message.

Perform several transformations on the message body:

- Remove lines of repeated citation leaders with no other
  content,
- Remove citation leaders standing alone before a block of cited
  text,
- Remove citation trailers standing alone after a block of cited
  text."

  ;; Remove lines of repeated citation leaders with no other content.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\{2,\\}" nil t)
    (replace-match "\\1"))

  ;; Remove citation leaders standing alone before a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(\n\\|^[^>].*\\)\n\\(^>[> ]*\n\\)" nil t)
    (replace-match "\\1\n"))

  ;; Remove citation trailers standing alone after a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\(^$\\|^[^>].*\\)" nil t)
    (replace-match "\\2")))

(defun mu4e~wash-wrap-long-lines ()
  "(from notmuch) Wrap long lines in the message.

If `mu4e-wash-wrap-lines-length' is a number, this will wrap
the message lines to the minimum of the width of the window or
its value. Otherwise, this function will wrap long lines in the
message at the window width. When doing so, citation leaders in
the wrapped text are maintained."

  (let* ((coolj-wrap-follows-window-size nil)
	 (limit (if (numberp mu4e-wash-wrap-lines-length)
		    (min mu4e-wash-wrap-lines-length
			 (window-width))
		  (window-width)))
	 (fill-column (- limit
			 ;; 2 to avoid poor interaction with
			 ;; `word-wrap'.
			 2)))
    (coolj-wrap-region (point-min) (point-max))))

(provide 'mu4e-wash)
