;;; twittering-oauth.el --- OAuth functions for twittering-mode

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

;; OAuth functions for twittering-mode

;;; Code:

(require 'twittering-http)

(defun twittering-has-oauth-access-token-p ()
  (let* ((required-entries '("oauth_token"
			     "oauth_token_secret"
			     "user_id"
			     "screen_name"))
	 (value-list
	  (mapcar
	   (lambda (key)
	     (cdr (assoc key twittering-oauth-access-token-alist)))
	   required-entries)))
    (null (remove t (mapcar 'stringp value-list)))))

(defun twittering-oauth-url-encode (str &optional coding-system)
  "Encode string according to Percent-Encoding defined in RFC 3986."
  (let ((coding-system (or (when (and coding-system
				      (coding-system-p coding-system))
			     coding-system)
			   'utf-8)))
    (mapconcat
     (lambda (c)
       (cond
	((or (and (<= ?A c) (<= c ?Z))
	     (and (<= ?a c) (<= c ?z))
	     (and (<= ?0 c) (<= c ?9))
	     (eq ?. c)
	     (eq ?- c)
	     (eq ?_ c)
	     (eq ?~ c))
	 (char-to-string c))
	(t (format "%%%02X" c))))
     (encode-coding-string str coding-system)
     "")))

(defun twittering-oauth-unhex (c)
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (- c ?0))
   ((and (<= ?A c) (<= c ?F))
    (+ 10 (- c ?A)))
   ((and (<= ?a c) (<= c ?f))
    (+ 10 (- c ?a)))
   ))

(defun twittering-oauth-url-decode (str &optional coding-system)
  (let* ((coding-system (or (when (and coding-system
				       (coding-system-p coding-system))
			      coding-system)
			    'utf-8))
	 (substr-list (split-string str "%"))
	 (head (car substr-list))
	 (tail (cdr substr-list)))
    (decode-coding-string
     (concat
      head
      (mapconcat
       (lambda (substr)
	 (if (string-match "\\`\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\(.*\\)\\'"
			   substr)
	     (let* ((c1 (string-to-char (match-string 1 substr)))
		    (c0 (string-to-char (match-string 2 substr)))
		    (tail (match-string 3 substr))
		    (ch (+ (* 16 (twittering-oauth-unhex c1))
			   (twittering-oauth-unhex c0))))
	       (concat (char-to-string ch) tail))
	   substr))
       tail
       ""))
     coding-system)))

(defun twittering-oauth-make-signature-base-string (method base-url parameters)
  ;; "OAuth Core 1.0a"
  ;; http://oauth.net/core/1.0a/#anchor13
  (let* ((sorted-parameters (copy-sequence parameters))
	 (sorted-parameters
	  (sort sorted-parameters
		(lambda (entry1 entry2)
		  (string< (car entry1) (car entry2))))))
    (concat
     method
     "&"
     (twittering-oauth-url-encode base-url)
     "&"
     (mapconcat
      (lambda (entry)
	(let ((key (car entry))
	      (value (cdr entry)))
	  (concat (twittering-oauth-url-encode key)
		  "%3D"
		  (twittering-oauth-url-encode value))))
      sorted-parameters
      "%26"))))

(defun twittering-oauth-make-random-string (len)
  (let* ((table
	  (concat
	   "0123456789"
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	   "abcdefghijklmnopqrstuvwxyz"))
	 (n (length table))
	 (l 0)
	 (result (make-string len ?0)))
    (while (< l len)
      (aset result l (aref table (random n)))
      (setq l (1+ l)))
    result))

(defun twittering-sha1 (&rest args)
  "Return the SHA1 (Secure Hash Algorithm) of an object.

This is equivalent to the function `sha1' except that
`coding-system-for-read' and `coding-system-for-write' are bound to the
symbol `binary'.

The function `sha1' uses an external program for large object. However,
the coding system for transferring data from/to the program is not fixed,
at least in the implementation distributed with GNU Emacs 21.4.1, 22.2.1
and 23.2.1.
Therefore, the result from the function `sha1' may depend on the current
coding system.

This function avoid the dependency by binding `coding-system-for-read' and
`coding-system-for-write' to the symbol `binary'."
  (require 'sha1)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (apply 'sha1 args)))

;;;
;;; The below function is derived from `hmac-sha1' retrieved
;;; from http://www.emacswiki.org/emacs/HmacShaOne.
;;;
(defun twittering-hmac-sha1 (key message)
  "Return an HMAC-SHA1 authentication code for KEY and MESSAGE.

KEY and MESSAGE must be unibyte strings.  The result is a unibyte
string.  Use the function `encode-hex-string' or the function
`base64-encode-string' to produce human-readable output.

See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
on the HMAC-SHA1 algorithm.

The Emacs multibyte representation actually uses a series of
8-bit values under the hood, so we could have allowed multibyte
strings as arguments.  However, internal 8-bit values don't
correspond to any external representation \(at least for major
version 22).  This makes multibyte strings useless for generating
hashes.

Instead, callers must explicitly pick and use an encoding for
their multibyte data.  Most callers will want to use UTF-8
encoding, which we can generate as follows:

  (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
        (unibyte-value (encode-coding-string value 'utf-8 t)))
    (twittering-hmac-sha1 unibyte-key unibyte-value))

For keys and values that are already unibyte, the
`encode-coding-string' calls just return the same string."
;;; Return an HMAC-SHA1 authentication code for KEY and MESSAGE.
;;;
;;; KEY and MESSAGE must be unibyte strings.  The result is a unibyte
;;; string.  Use the function `encode-hex-string' or the function
;;; `base64-encode-string' to produce human-readable output.
;;;
;;; See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
;;; on the HMAC-SHA1 algorithm.
;;;
;;; The Emacs multibyte representation actually uses a series of
;;; 8-bit values under the hood, so we could have allowed multibyte
;;; strings as arguments.  However, internal 8-bit values don't
;;; correspond to any external representation \(at least for major
;;; version 22).  This makes multibyte strings useless for generating
;;; hashes.
;;;
;;; Instead, callers must explicitly pick and use an encoding for
;;; their multibyte data.  Most callers will want to use UTF-8
;;; encoding, which we can generate as follows:
;;;
;;; (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
;;;       (unibyte-value (encode-coding-string value 'utf-8 t)))
;;; (hmac-sha1 unibyte-key unibyte-value))
;;;
;;; For keys and values that are already unibyte, the
;;; `encode-coding-string' calls just return the same string.
;;;
;;; Author: Derek Upham - sand (at) blarg.net
;;;
;;; Copyright: This code is in the public domain.
  (require 'sha1)
  (when (multibyte-string-p key)
    (error "key must be unibyte"))
  (when (multibyte-string-p message)
    (error "message must be unibyte"))

  ;; The key block is always exactly the block size of the hash
  ;; algorithm.  If the key is too small, we pad it with zeroes (or
  ;; instead, we initialize the key block with zeroes and copy the
  ;; key onto the nulls).  If the key is too large, we run it
  ;; through the hash algorithm and use the hashed value (strange
  ;; but true).

  (let ((+hmac-sha1-block-size-bytes+ 64)) ; SHA-1 uses 512-bit blocks
    (when (< +hmac-sha1-block-size-bytes+ (length key))
      (setq key (twittering-sha1 key nil nil t)))

    (let ((key-block (make-vector +hmac-sha1-block-size-bytes+ 0)))
      (dotimes (i (length key))
	(aset key-block i (aref key i)))

      (let ((opad (make-vector +hmac-sha1-block-size-bytes+ #x5c))
	    (ipad (make-vector +hmac-sha1-block-size-bytes+ #x36)))

	(dotimes (i +hmac-sha1-block-size-bytes+)
	  (aset ipad i (logxor (aref ipad i) (aref key-block i)))
	  (aset opad i (logxor (aref opad i) (aref key-block i))))

	(when (fboundp 'unibyte-string)
	  ;; `concat' of Emacs23 (and later?) generates a multi-byte
	  ;; string from a vector of characters with eight bit.
	  ;; Since `opad' and `ipad' must be unibyte, we have to
	  ;; convert them by using `unibyte-string'.
	  ;; We cannot use `string-as-unibyte' here because it encodes
	  ;; bytes with the manner of UTF-8.
	  (setq opad (apply 'unibyte-string (mapcar 'identity opad)))
	  (setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))

	(twittering-sha1 (concat opad
				 (twittering-sha1 (concat ipad message)
						  nil nil t))
			 nil nil t)))))

(defun twittering-oauth-auth-str (method base-url query-parameters oauth-parameters key)
  "Generate the value for HTTP Authorization header on OAuth.
QUERY-PARAMETERS is an alist for query parameters, where name and value
must be encoded into the same as they will be sent."
  (let* ((parameters (append query-parameters oauth-parameters))
	 (base-string
	  (twittering-oauth-make-signature-base-string method base-url parameters))
	 (key (if (multibyte-string-p key)
		  (string-make-unibyte key)
		key))
	 (base-string (if (multibyte-string-p base-string)
			  (string-make-unibyte base-string)
			base-string))
	 (signature
	  (base64-encode-string (twittering-hmac-sha1 key base-string))))
    (concat
     "OAuth "
     (mapconcat
      (lambda (entry)
	(concat (car entry) "=\"" (cdr entry) "\""))
      oauth-parameters
      ",")
     ",oauth_signature=\"" (twittering-oauth-url-encode signature) "\"")))

(defun twittering-oauth-auth-str-request-token (url query-parameters consumer-key consumer-secret &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_callback" . "oob")
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0")))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-exchange-token (url query-parameters consumer-key consumer-secret request-token request-token-secret verifier &optional oauth-parameters)
  (let ((key (concat consumer-secret "&" request-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,request-token)
	       ("oauth_verifier" . ,verifier)))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-access (method url query-parameters consumer-key consumer-secret access-token access-token-secret &optional oauth-parameters)
  "Generate a string for Authorization in HTTP header on OAuth.
METHOD means HTTP method such as \"GET\", \"POST\", etc. URL means a simple
URL without port number and query parameters.
QUERY-PARAMETERS means an alist of query parameters such as
'((\"status\" . \"test%20tweet\")
  (\"in_reply_to_status_id\" . \"12345678\")),
where name and value must be encoded into the same as they will be sent.
CONSUMER-KEY and CONSUMER-SECRET specifies the consumer.
ACCESS-TOKEN and ACCESS-TOKEN-SECRET must be authorized before calling this
function."
  (let ((key (concat consumer-secret "&" access-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,access-token)))))
    (twittering-oauth-auth-str method url query-parameters oauth-params key)))

;; "Using xAuth | dev.twitter.com"
;; http://dev.twitter.com/pages/xauth
(defun twittering-xauth-auth-str-access-token (url query-parameters consumer-key consumer-secret username password &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0"))))
	(query-params
	 (append query-parameters
		 `(("x_auth_mode" . "client_auth")
		   ("x_auth_password"
		    . ,(twittering-oauth-url-encode password))
		   ("x_auth_username"
		    . ,(twittering-oauth-url-encode username))))))
    (twittering-oauth-auth-str "POST" url query-params oauth-params key)))

;; "OAuth Core 1.0a"
;; http://oauth.net/core/1.0a/#response_parameters
(defun twittering-oauth-make-response-alist (str)
  (mapcar
   (lambda (entry)
     (let* ((pair (split-string entry "="))
	    (name-entry (car pair))
	    (value-entry (cadr pair))
	    (name (and name-entry (twittering-oauth-url-decode name-entry)))
	    (value (and value-entry
			(twittering-oauth-url-decode value-entry))))
       `(,name . ,value)))
   (split-string str "&")))

(defun twittering-oauth-get-response-alist (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (search-forward-regexp
	   "\\`\\(\\(HTTP/1\\.[01]\\|HTTP/2\\(?:\\.0\\)?\\) \\([0-9][0-9][0-9]\\)\\(.*?\\)\\)\r?\n"
	   nil t)
      (let ((status-line (match-string 1))
	    (http-version (match-string 2))
	    (status-code (match-string 3))
	    (reason-phrase (match-string 4)))
	(cond
	 ((not (string-match "2[0-9][0-9]" status-code))
	  (message "Response: %s" status-line)
	  nil)
	 ((search-forward-regexp "\r?\n\r?\n" nil t)
	  (let ((beg (match-end 0))
		(end (point-max)))
	    (twittering-oauth-make-response-alist (buffer-substring beg end))))
	 (t
	  (message "Response: %s" status-line)
	  nil))))))

(defun twittering-oauth-get-token-alist-url (url auth-str post-body)
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . ,(format "%d" (length post-body)))))
	 (url-request-data post-body)
	 (coding-system-for-read 'utf-8-unix))
    (lexical-let ((result 'queried))
      (let ((buffer
	     (url-retrieve
	      url
	      (lambda (&rest args)
		(let* ((status (if (< 21 emacs-major-version)
				   (car args)
				 nil))
		       (callback-args (if (< 21 emacs-major-version)
					  (cdr args)
					args))
		       (response-buffer (current-buffer)))
		  (setq result
			(twittering-oauth-get-response-alist response-buffer))
		  )))))
	(while (eq result 'queried)
	  (sleep-for 0.1))
	(unless twittering-debug-mode
	  (kill-buffer buffer))
	result))))

(defun twittering-oauth-get-token-alist (url auth-str &optional post-body)
  (let ((request
	 (twittering-make-http-request-from-uri
	  "POST"
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded"))
	  url post-body)))
    (lexical-let ((result 'queried))
      (let ((proc
	     (twittering-send-http-request
	      request nil
	      (lambda (proc status connection-info header-info)
		(let ((status-line (cdr (assq 'status-line header-info)))
		      (status-code (cdr (assq 'status-code header-info))))
		  (twittering-case-string
		   status-code
		   (("200")
		    (when twittering-debug-mode
		      (let ((buffer (current-buffer)))
			(with-current-buffer (twittering-debug-buffer)
			  (insert-buffer-substring buffer))))
		    (setq result
			  (twittering-oauth-make-response-alist
			   (buffer-string)))
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
				 (setq result nil)))
	result))))

(defun twittering-oauth-get-request-token (url consumer-key consumer-secret)
  (let ((auth-str
	 (twittering-oauth-auth-str-request-token
	  url nil consumer-key consumer-secret)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-exchange-request-token (url consumer-key consumer-secret request-token request-token-secret verifier)
  (let ((auth-str
	 (twittering-oauth-auth-str-exchange-token
	  url nil
	  consumer-key consumer-secret
	  request-token request-token-secret verifier)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-get-access-token (request-token-url authorize-url-func access-token-url consumer-key consumer-secret consumer-name)
  "Return an alist of authorized access token.
The function retrieves a request token from the site specified by
REQUEST-TOKEN-URL. Then, The function asks a WWW browser to authorize the
token by calling `browse-url'. The URL for authorization is calculated by
calling AUTHORIZE-URL-FUNC with the request token as an argument.
AUTHORIZE-URL-FUNC is called as `(funcal AUTHORIZE-URL-FUNC request-token)',
where the request-token is a string.
After calling `browse-url', the function waits for user to input the PIN code
that is displayed in the browser. The request token is authorized by the
PIN code, and then it is exchanged for the access token on the site
specified by ACCESS-TOKEN-URL.
CONSUMER-KEY and CONSUMER-SECRET specify the consumer.
CONSUMER-NAME is displayed at the guide of authorization.

The access token is returned as a list of a cons pair of name and value
like following:
 ((\"oauth_token\"
  . \"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\")
  (\"oauth_token_secret\"
   . \"J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA\")
  (\"user_id\" . \"819797\")
  (\"screen_name\" . \"episod\"))
."
  (let* ((request-token-alist
	  (twittering-oauth-get-request-token
	   request-token-url consumer-key consumer-secret))
	 (request-token (cdr (assoc "oauth_token" request-token-alist)))
	 (request-token-secret
	  (cdr (assoc "oauth_token_secret" request-token-alist)))
	 (authorize-url (funcall authorize-url-func request-token))
	 (str
	  (concat
	   (propertize "Authorization via OAuth\n" 'face 'bold)
	   "\n"
	   "1.Allow access by " consumer-name " on the below site.\n"
	   "\n  "
	   (propertize authorize-url 'url authorize-url 'face 'bold)
	   "\n"
	   "\n"
	   (when twittering-oauth-invoke-browser
	     (concat
	      "  Emacs invokes your browser by the function `browse-url'.\n"
	      "  If the site is not opened automatically, you have to open\n"
	      "  the site manually.\n"
	      "\n"))
	   "2.After allowing access, the site will display the PIN code."
	   "\n"
	   "  Input the PIN code "
	   (propertize "at the below minibuffer." 'face 'bold))))
    (cond
     (request-token-alist
      (with-temp-buffer
	(switch-to-buffer (current-buffer))
	(let* ((str-height (length (split-string str "\n")))
	       (height (max 0 (- (/ (- (window-text-height) 1) 2)
				 (/ str-height 2)))))
	  (insert (make-string height ?\n) str)
	  (if twittering-oauth-invoke-browser
	      (browse-url authorize-url)
	    (when (y-or-n-p "Open authorization URL with browser? (using `browse-url')")
	      (browse-url authorize-url)))
	  (let* ((pin
		  (block pin-input-block
		    (while t
		      (let ((pin-input (read-string "Input PIN code: ")))
			(when (string-match "^\\s-*\\([0-9]+\\)\\s-*$" pin-input)
			  (return-from pin-input-block
			    (match-string 1 pin-input)))))))
		 (verifier pin))
	    (twittering-oauth-exchange-request-token
	     access-token-url
	     consumer-key consumer-secret
	     request-token request-token-secret verifier)))))
     (t
      (error "Failed to retrieve a request token")
      nil))))

(defun twittering-xauth-get-access-token (access-token-url consumer-key consumer-secret username password)
  (let ((auth-str
	 (twittering-xauth-auth-str-access-token
	  access-token-url nil consumer-key consumer-secret
	  username password))
	(post-body
	 (mapconcat (lambda (pair)
		      (format "%s=%s" (car pair)
			      (twittering-oauth-url-encode (cdr pair))))
		    `(("x_auth_mode" . "client_auth")
		      ("x_auth_password" . ,password)
		      ("x_auth_username" . ,username))
		    "&")))
    (twittering-oauth-get-token-alist access-token-url auth-str post-body)))

(provide 'twittering-oauth)

;;; twittering-oauth.el ends here
