;;; twittering-url-shortener.el --- Functions for using URL shorteners with twittering-mode

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;	Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
;;	Xavier Maillard <xavier@maillard.im>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Identity: $Id: a2fc6eb695ad0994e986ab0413e53f335d9a947b $
;; Keywords: twitter web
;; URL: http://twmode.sf.net/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Functions for using URL shorteners with twittering-mode

;;; Code:

(defcustom twittering-tinyurl-service 'tinyurl
  "*Shorten URI service to use.

This must be one of key symbols of `twittering-tinyurl-services-map'.

To use bit.ly or j.mp services, you have to configure
`twittering-bitly-login' and `twittering-bitly-api-key'."
  :type '(radio (symbol :tag "bit.ly"
			:value bit.ly)
		(symbol :tag "goo.gl"
			:value goo.gl)
		(symbol :tag "is.gd"
			:value is.gd)
		(symbol :tag "j.mp"
			:value j.mp)
		(symbol :tag "migre.me"
			:value migre.me)
		(symbol :tag "tinyurl"
			:value tinyurl)
		(symbol :tag "toly"
			:value toly))
  :group 'twittering-mode)

(defcustom twittering-tinyurl-services-map
  '((bit.ly twittering-make-http-request-for-bitly
	    (lambda (service reply)
	      (if (string-match "\n\\'" reply)
		  (substring reply 0 (match-beginning 0))
		reply)))
    (goo.gl
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" '(("Content-Type" . "application/json"))
	"https://www.googleapis.com/urlshortener/v1/url"
	(concat "{\"longUrl\": \"" longurl "\"}")))
     (lambda (service reply)
       (when (string-match "\"id\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\""
			   reply)
	 (match-string 1 reply))))
    (is.gd . "http://is.gd/create.php?format=simple&url=")
    (j.mp twittering-make-http-request-for-bitly
	  (lambda (service reply)
	    (if (string-match "\n\\'" reply)
		(substring reply 0 (match-beginning 0))
	      reply)))
    (migre.me . "http://migre.me/api.txt?url=")
    (tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" nil
	"http://to.ly/api.php"
	(concat "longurl=" (twittering-percent-encode longurl))))))
  "Alist of URL shortening services.
The key is a symbol specifying the service.
The value is a string or a list consisting of two elements at most.

If the value is a string, `(concat THE-FIRST-ELEMENT longurl)' is used as the
URL invoking the service.
If the value is a list, it is interpreted as follows.
The first element specifies how to make a HTTP request for shortening a URL.
If the first element is a string, `(concat THE-FIRST-ELEMENT longurl)' is
used as the URL invoking the service.
If the first element is a function, it is called as `(funcall THE-FIRST-ELEMENT
service-symbol longurl)' to obtain a HTTP request alist for invoking the
service, which must be generated by `twittering-make-http-request'.

The second element specifies how to post-process a HTTP reply by the HTTP
request.
If the second element is nil, the reply is directly used as a shortened URL.
If the second element is a function, it is called as `(funcall
THE-SECOND-ELEMENT service-symbol HTTP-reply-string)' and its result is used
as a shortened URL."
  :type 'alist
  :group 'twittering-mode)

(defcustom twittering-bitly-login nil
  "*The login name for URL shortening service bit.ly and j.mp."
  :type '(choice (const nil)
		 string)
  :group 'twittering-mode)

(defcustom twittering-bitly-api-key nil
  "*API key for `bit.ly' and `j.mp' URL shortening services."
  :type '(choice (const nil)
		 string)
  :group 'twittering-mode)

(defun twittering-tinyurl-get (longurl &optional service)
  "Shorten LONGURL with the service specified by `twittering-tinyurl-service'."
  (let* ((service (or service twittering-tinyurl-service))
	 (api (cdr (assq service twittering-tinyurl-services-map)))
	 (request-generator (when (listp api) (elt api 0)))
	 (post-process (when (listp api) (elt api 1)))
	 (encoded-url (twittering-percent-encode longurl))
	 (request
	  (cond
	   ((stringp api)
	    (twittering-make-http-request-from-uri
	     "GET" nil (concat api encoded-url)))
	   ((stringp request-generator)
	    (twittering-make-http-request-from-uri
	     "GET" nil (concat request-generator encoded-url)))
	   ((functionp request-generator)
	    (funcall request-generator service longurl))
	   (t
	    (error "%s is invalid. try one of %s"
		   (symbol-name service)
		   (mapconcat (lambda (x) (symbol-name (car x)))
			      twittering-tinyurl-services-map ", "))
	    nil)))
	 (additional-info `((longurl . ,longurl))))
    (cond
     ((null request)
      (error "Failed to generate a HTTP request for shortening %s with %s"
	     longurl (symbol-name service))
      nil)
     (t
      (lexical-let ((result 'queried))
	(let ((proc
	       (twittering-send-http-request
		request additional-info
		(lambda (proc status connection-info header-info)
		  (let ((status-line (cdr (assq 'status-line header-info)))
			(status-code (cdr (assq 'status-code header-info))))
		    (twittering-case-string
		     status-code
		     (("200")
		      (setq result (buffer-string))
		      nil)
		     (t
		      (setq result nil)
		      (format "Response: %s" status-line)))))
		(lambda (proc status connection-info)
		  (when (and (not (twittering-process-alive-p proc))
			     (eq result 'queried))
		    (setq result nil))))))
	  (twittering-wait-while nil 0.1
				 (and (eq result 'queried)
				      (twittering-process-alive-p proc)))
	  (when (and (eq result 'queried)
		     (not (twittering-process-alive-p proc)))
	    ;; If the process has been dead, wait a moment because
	    ;; Emacs may be in the middle of evaluating the sentinel.
	    (twittering-wait-while 10 0.1
				   (eq result 'queried)
				   nil
				   ;; Reset `result' on timeout.
				   (setq result nil))))
	(let ((processed-result (if (and result (functionp post-process))
				    (funcall post-process service result)
				  result)))
	  (if processed-result
	      processed-result
	    (error "Failed to shorten a URL %s with %s"
		   longurl (symbol-name service))
	    nil)))))))

(defun twittering-tinyurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (twittering-tinyurl-get (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

(defun twittering-make-http-request-for-bitly (service longurl)
  "Make a HTTP request for URL shortening service bit.ly or j.mp.
Before calling this, you have to configure `twittering-bitly-login' and
`twittering-bitly-api-key'."
  (let* ((query-string
	  (mapconcat
	   (lambda (entry)
	     (concat (car entry) "=" (cdr entry)))
	   `(("login" . ,twittering-bitly-login)
	     ("apiKey" . ,twittering-bitly-api-key)
	     ("format" . "txt")
	     ("longUrl" . ,(twittering-percent-encode longurl)))
	   "&"))
	 (prefix
	  (cdr (assq service '((bit.ly . "http://api.bit.ly/v3/shorten?")
			       (j.mp . "http://api.j.mp/v3/shorten?")))))
	 (uri (concat prefix query-string)))
    (twittering-make-http-request-from-uri "GET" nil uri)))

(provide 'twittering-url-shortener)

;;; twittering-url-shortener.el ends here
