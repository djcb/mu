;; mu4e-proc.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2016 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'mu4e-vars)
(require 'mu4e-utils)
(require 'mu4e-meta)


(require 'mu4e-proc-mu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal vars

(defconst mu4e~proc-name "*mu4e-proc*"
  "Name of the server process, buffer.")
(defvar mu4e~proc-process nil
  "The mu-server process.")

;; dealing with the length cookie that precedes expressions
(defconst mu4e~cookie-pre "\376"
  "Each expression we get from the backend (mu server) starts with
a length cookie:
  <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-post "\377"
    "Each expression we get from the backend (mu server) starts with
a length cookie:
  <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-matcher-rx
  (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)" mu4e~cookie-post)
  "Regular expression matching the length cookie.
Match 1 will be the length (in hex).")

(defun mu4e~proc-running-p  ()
  "Whether the mu process is running."
  (when (and mu4e~proc-process
	  (memq (process-status mu4e~proc-process)
	    '(run open listen connect stop)))
    t))

(defsubst mu4e~proc-eat-sexp-from-buf ()
  "'Eat' the next s-expression from `mu4e~proc-buf'.
Note: this is a string, not an emacs-buffer. `mu4e~proc-buf gets
its contents from the mu-servers in the following form:
   <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>
Function returns this sexp, or nil if there was none.
`mu4e~proc-buf' is updated as well, with all processed sexp data
removed."
  (ignore-errors ;; the server may die in the middle...
    ;; mu4e~cookie-matcher-rx:
    ;;  (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)]" mu4e~cookie-post)
    (let ((b (string-match mu4e~cookie-matcher-rx mu4e~proc-buf))
	   (sexp-len) (objcons))
      (when b
	(setq sexp-len (string-to-number (match-string 1 mu4e~proc-buf) 16))
	;; does mu4e~proc-buf contain the full sexp?
	(when (>= (length mu4e~proc-buf) (+ sexp-len (match-end 0)))
	  ;; clear-up start
	  (setq mu4e~proc-buf (substring mu4e~proc-buf (match-end 0)))
	  ;; note: we read the input in binary mode -- here, we take the part
	  ;; that is the sexp, and convert that to utf-8, before we interpret
	  ;; it.
	  (setq objcons (read-from-string
			  (decode-coding-string
			    (substring mu4e~proc-buf 0 sexp-len)
			    'utf-8 t)))
	  (when objcons
	    (setq mu4e~proc-buf (substring mu4e~proc-buf sexp-len))
	    (car objcons)))))))


(defun mu4e~proc-filter (proc str)
  "A process-filter for the 'mu server' output.
It accumulates the strings into valid sexps by checking of the
';;eox' end-of-sexp marker, and then evaluating them.

The server output is as follows:

   1. an error
      (:error 2 :message \"unknown command\")
      ;; eox
   => this will be passed to `mu4e-error-func'.

   2a. a message sexp looks something like:
 \(
  :docid 1585
  :from ((\"Donald Duck\" . \"donald@example.com\"))
  :to ((\"Mickey Mouse\" . \"mickey@example.com\"))
  :subject \"Wicked stuff\"
  :date (20023 26572 0)
  :size 15165
  :references (\"200208121222.g7CCMdb80690@msg.id\")
  :in-reply-to \"200208121222.g7CCMdb80690@msg.id\"
  :message-id \"foobar32423847ef23@pluto.net\"
  :maildir: \"/archive\"
  :path \"/home/mickey/Maildir/inbox/cur/1312254065_3.32282.pluto,4cd5bd4e9:2,\"
  :priority high
  :flags (new unread)
  :attachments ((2 \"hello.jpg\" \"image/jpeg\") (3 \"laah.mp3\" \"audio/mp3\"))
  :body-txt \" <message body>\"
\)
;; eox
   => this will be passed to `mu4e-header-func'.

  2b. After the list of message sexps has been returned (see 2a.),
  we'll receive a sexp that looks like
  (:found <n>) with n the number of messages found. The <n> will be
  passed to `mu4e-found-func'.

  3. a view looks like:
  (:view <msg-sexp>)
  => the <msg-sexp> (see 2.) will be passed to `mu4e-view-func'.

  4. a database update looks like:
  (:update <msg-sexp> :move <nil-or-t>)

   => the <msg-sexp> (see 2.) will be passed to
   `mu4e-update-func', :move tells us whether this is a move to
   another maildir, or merely a flag change.

  5. a remove looks like:
  (:remove <docid>)
  => the docid will be passed to `mu4e-remove-func'

  6. a compose looks like:
  (:compose <reply|forward|edit|new> [:original<msg-sexp>] [:include <attach>])
  `mu4e-compose-func'."
  (mu4e-log 'misc "* Received %d byte(s)" (length str))
  (setq mu4e~proc-buf (concat mu4e~proc-buf str)) ;; update our buffer
  (let ((sexp (mu4e~proc-eat-sexp-from-buf)))
    (with-local-quit
      (while sexp
	(mu4e-log 'from-server "%S" sexp)
	(cond
	  ;; a header plist can be recognized by the existence of a :date field
	  ((plist-get sexp :date)
	    (funcall mu4e-header-func sexp))

	  ;; the found sexp, we receive after getting all the headers
	  ((plist-get sexp :found)
	    (funcall mu4e-found-func (plist-get sexp :found)))

	  ;; viewing a specific message
	  ((plist-get sexp :view)
	    (funcall mu4e-view-func (plist-get sexp :view)))

	  ;; receive an erase message
	  ((plist-get sexp :erase)
	    (funcall mu4e-erase-func))

	  ;; receive a :sent message
	  ((plist-get sexp :sent)
	    (funcall mu4e-sent-func
	      (plist-get sexp :docid)
	      (plist-get sexp :path)))

	  ;; received a pong message
	  ((plist-get sexp :pong)
	    (funcall mu4e-pong-func
	      (plist-get sexp :props)))

	  ;; received a contacts message
	  ;; note: we use 'member', to match (:contacts nil)
	  ((plist-member sexp :contacts)
	    (funcall mu4e-contacts-func
	      (plist-get sexp :contacts)))

	  ;; something got moved/flags changed
	  ((plist-get sexp :update)
	    (funcall mu4e-update-func
	      (plist-get sexp :update) (plist-get sexp :move)))

	  ;; a message got removed
	  ((plist-get sexp :remove)
	    (funcall mu4e-remove-func (plist-get sexp :remove)))

	  ;; start composing a new message
	  ((plist-get sexp :compose)
	    (funcall mu4e-compose-func
	      (plist-get sexp :compose)
	      (plist-get sexp :original)
	      (plist-get sexp :include)))

	  ;; do something with a temporary file
	  ((plist-get sexp :temp)
	    (funcall mu4e-temp-func
	      (plist-get sexp :temp)   ;; name of the temp file
	      (plist-get sexp :what)   ;; what to do with it
	                               ;; (pipe|emacs|open-with...)
	      (plist-get sexp :docid)  ;; docid of the message
	      (plist-get sexp :param)));; parameter for the action

	  ;; get some info
	  ((plist-get sexp :info)
	    (funcall mu4e-info-func sexp))

	  ;; receive an error
	  ((plist-get sexp :error)
	    (funcall mu4e-error-func
	      (plist-get sexp :error)
	      (plist-get sexp :message)))

	  (t (mu4e-message "Unexpected data from server [%S]" sexp)))

	(setq sexp (mu4e~proc-eat-sexp-from-buf))))))

(defun mu4e~escape (str)
  "Escape STRING for transport -- put it in quotes, and escape existing quotation.
In particular, backslashes and double-quotes."
  (let ((esc (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" esc))))

(provide 'mu4e-proc)
;; End of mu4e-proc.el
