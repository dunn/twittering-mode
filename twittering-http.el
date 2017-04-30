;;; twittering-http.el --- Functions for making HTTP(S) requests in twittering-mode

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

;; Functions for making HTTP(S) requests in twittering-mode

;;; Code:

(require 'twittering-utilities)

(defcustom twittering-use-ssl t
  "*Use SSL connection if this variable is non-nil.

SSL connections use an external command as a backend."
  :type 'boolean
  :group 'twittering-mode)

(defcustom twittering-allow-insecure-server-cert nil
  "*If non-nil, `twittering-mode' allows insecure server certificates."
  :type 'boolean
  :group 'twittering-mode)

(defvar twittering-curl-program nil
  "Cache a result of `twittering-find-curl-program'.
DO NOT SET VALUE MANUALLY.")
(defvar twittering-curl-program-https-capability nil
  "Cache a result of `twittering-start-http-session-curl-https-p'.
DO NOT SET VALUE MANUALLY.")
(defvar twittering-curl-program-http2-capability nil
  "Cache a result of `twittering-start-http-session-curl-http2-p'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-wget-program nil
  "Cache a result of `twittering-find-wget-program'.
DO NOT SET VALUE MANUALLY.")

(defcustom twittering-tls-program nil
  "*List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
Also see `tls-program'.
If nil, this is initialized with a list of valied entries extracted from
`tls-program'."
  :type '(repeat string)
  :group 'twittering-mode)

(defcustom twittering-connection-type-order
  '(curl wget urllib-http native urllib-https)
  "*A list of connection methods in the preferred order."
  :type 'list
  :group 'twittering-mode)

(defvar twittering-notify-successful-http-get t)

(defun twittering-connection-build-customize-option ()
  "Generate a valid `defcustom' entry to build `twittering-connection-type-table' variable."
  (list 'repeat
	(list
	 'cons :tag "Connection"
	 '(symbol :tag "Name" :value "")
	 '(repeat
	   :tag "Connection method definition"
	   (choice
	    (cons :tag "Check test method"
		  (const :format "" check)
		  (choice :value t (const :tag "Do not check" t)
			  (function :tag "Check function")))
	    (cons :tag "Display name"
		  (const :format "" display-name)
		  string)
	    (cons :tag "HTTPS connection method"
		  (const :format "" https)
		  (choice :value nil (const :tag "None" nil)
			  (const :tag "True" t)
			  (function :tag "HTTPS test function")))
	    (cons :tag "Send HTTP request function"
		  (const :format "" send-http-request)
		  function)
	    (cons :tag "Pre process buffer"
		  (const :format "" pre-process-buffer)
		  function))))))

(defcustom twittering-connection-type-table
  '((native
     (check . t)
     (send-http-request . twittering-send-http-request-native)
     (pre-process-buffer . twittering-pre-process-buffer-native))
    (curl
     (check . twittering-start-http-session-curl-p)
     (https . twittering-start-http-session-curl-https-p)
     (send-http-request . twittering-send-http-request-curl)
     (pre-process-buffer . twittering-pre-process-buffer-curl))
    (wget
     (check . twittering-start-http-session-wget-p)
     (https . ignore)
     (send-http-request . twittering-send-http-request-wget)
     (pre-process-buffer . twittering-pre-process-buffer-wget))
    (urllib-http
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . nil)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib))
    (urllib-https
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . twittering-start-http-session-urllib-https-p)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib)))
  "A list of alist of connection methods."
  :group 'twittering-mode
  :type (twittering-connection-build-customize-option))

;;;

(defun twittering-percent-encode (str &optional coding-system)
  "Encode STR according to Percent-Encoding defined in RFC 3986."
  (twittering-oauth-url-encode str coding-system))

(defun twittering-lookup-connection-type (use-ssl &optional order table)
  "Return available entry extracted fron connection type table.
TABLE is connection type table, which is an alist of type symbol and its
item alist, such as
 '((native (check . t)
           (https . twittering-start-http-session-native-tls-p)
           (start . twittering-start-http-session-native))
   (curl (check . twittering-start-http-session-curl-p)
         (https . twittering-start-http-session-curl-https-p)
         (start . twittering-start-http-session-curl))) .
ORDER means the priority order of type symbols.
If USE-SSL is nil, the item `https' is ignored.
When the type `curl' has priority and is available for the above table,
the function returns
 '((check . twittering-start-http-session-curl-p)
   (https . twittering-start-http-session-curl-https-p)
   (start . twittering-start-http-session-curl)) ."
  (let ((rest (or order twittering-connection-type-order))
	(table (or table twittering-connection-type-table))
	(result nil))
    (while (and rest (null result))
      (let* ((candidate (car rest))
	     (entry (cons `(symbol . ,candidate)
			  (cdr (assq candidate table))))
	     (entry (if (assq 'display-name entry)
			entry
		      (cons `(display-name . ,(symbol-name candidate))
			    entry)))
	     (validate (lambda (item)
			 (let ((v (cdr (assq item entry))))
			   (or (null v) (eq t v) (functionp v)))))
	     (confirm (lambda (item)
			(let ((v (cdr (assq item entry))))
			  (cond
			   ((null v) nil)
			   ((eq t v) t)
			   ((functionp v) (funcall v)))))))
	(if (and (funcall validate 'check)
		 (or (not use-ssl) (funcall validate 'https)))
	    (cond
	     ((and (funcall confirm 'check)
		   (or (not use-ssl) (funcall confirm 'https)))
	      (setq rest nil)
	      (setq result entry))
	     (t
	      (setq rest (cdr rest))))
	  (message "The configuration for conncetion type `%s' is invalid."
		   candidate)
	  (setq rest nil))))
    result))

(defun twittering-get-connection-method-name (use-ssl)
  "Return a name of the preferred connection method.
If USE-SSL is non-nil, return a connection method for HTTPS.
If USE-SSL is nil, return a connection method for HTTP."
  (cdr (assq 'display-name (twittering-lookup-connection-type use-ssl))))

(defun twittering-lookup-http-start-function (&optional order table)
  "Decide a connection method from currently available methods."
  (let ((entry
	 (twittering-lookup-connection-type twittering-use-ssl order table)))
    (cdr (assq 'send-http-request entry))))

(defun twittering-ensure-connection-method (&optional order table)
  "Ensure a connection method with a compromise.
Return nil if no connection methods are available with a compromise."
  (let* ((use-ssl (or twittering-use-ssl twittering-oauth-use-ssl))
	 (entry (twittering-lookup-connection-type use-ssl order table)))
    (cond
     (entry
      t)
     ((and (null entry) use-ssl
	   (yes-or-no-p "HTTPS(SSL) is unavailable. Use HTTP instead? "))
      ;; Fall back on connection without SSL.
      (setq twittering-use-ssl nil)
      (setq twittering-oauth-use-ssl nil)
      (twittering-update-mode-line)
      (twittering-ensure-connection-method order table))
     (t
      (message "No connection methods are available.")
      nil))))

(defun twittering-make-http-request (method header-list host port path query-parameters post-body use-ssl)
  "Returns an alist specifying a HTTP request.
METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\", \"User-Agent\" and \"Content-Length\" are
automatically filled if necessary.
HOST specifies the host.
PORT specifies the port. This must be an integer.
PATH specifies the absolute path in URI (without query string).
QUERY-PARAMTERS is a string or an alist.
If QUERY-PARAMTERS is a string, it is treated as an encoded query string.
If QUERY-PARAMTERS is an alist, it represents a list of cons pairs of
string, (query-key . query-value).
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted.
If USE-SSL is non-nil, the request is performed with SSL.

The result alist includes the following keys, where a key is a symbol.
  method: HTTP method such as \"GET\" or \"POST\".
  scheme: the scheme name. \"http\" or \"https\".
  host: the host to which the request is sent.
  port: the port to which the request is sent (integer).
  path: the absolute path string. Note that it does not include query string.
  query-string: the query string.
  encoded-query-alist: the alist consisting of pairs of encoded query-name and
    encoded query-value.
  uri: the URI. It includes the query string.
  uri-without-query: the URI without the query string.
  header-list: an alist specifying pairs of a parameter and its value in HTTP
    header field.
  post-body: the entity that will be posted."
  (let* ((scheme (if use-ssl "https" "http"))
	 (default-port (if use-ssl 443 80))
	 (port (if port port default-port))
	 (query-string
	  (cond
	   ((stringp query-parameters)
	    query-parameters)
	   ((consp query-parameters)
	    (mapconcat (lambda (pair)
			 (cond
			  ((stringp pair)
			   (twittering-percent-encode pair))
			  ((consp pair)
			   (format
			    "%s=%s"
			    (twittering-percent-encode (car pair))
			    (twittering-percent-encode (cdr pair))))
			  (t
			   nil)))
		       query-parameters
		       "&"))
	   (t
	    nil)))
	 (encoded-query-alist
	  (cond
	   ((stringp query-parameters)
	    ;; Query name and its value must be already encoded.
	    (mapcar (lambda (str)
		      (if (string-match "=" str)
			  (let ((key (substring str 0 (match-beginning 0)))
				(value (substring str (match-end 0))))
			    `(,key . ,value))
			`(,str . nil)))
		    (split-string query-parameters "&")))
	   ((consp query-parameters)
	    (mapcar (lambda (pair)
		      (cond
		       ((stringp pair)
			(cons (twittering-percent-encode pair) nil))
		       ((consp pair)
			(cons (twittering-percent-encode (car pair))
			      (twittering-percent-encode (cdr pair))))
		       (t
			nil)))
		    query-parameters))
	   (t
	    nil)))
	 (uri-without-query
	  (concat scheme "://"
		  host
		  (when (and port (not (= port default-port)))
		    (format ":%d" port))
		  path))
	 (uri
	  (if query-string
	      (concat uri-without-query "?" query-string)
	    uri-without-query))
	 (header-list
	  `(,@(when (and (string= method "POST")
			 (not (assoc "Content-Length" header-list)))
		`(("Content-Length" . ,(format "%d" (length post-body)))))
	    ,@(unless (assoc "Host" header-list)
		`(("Host" . ,host)))
	    ,@(unless (assoc "User-Agent" header-list)
		`(("User-Agent" . ,(twittering-user-agent))))
	    ,@header-list)))
    (cond
     ((not (member method '("POST" "GET")))
      (error "Unknown HTTP method: %s" method)
      nil)
     ((not (string-match "^/" path))
      (error "Invalid HTTP path: %s" path)
      nil)
     (t
      `((method . ,method)
	(scheme . ,scheme)
	(host . ,host)
	(port . ,port)
	(path . ,path)
	(query-string . ,query-string)
	(encoded-query-alist . ,encoded-query-alist)
	(uri . ,uri)
	(uri-without-query . ,uri-without-query)
	(header-list . ,header-list)
	(post-body . ,post-body))))))

(defun twittering-make-http-request-from-uri (method header-list uri &optional post-body)
  "Returns an alist specifying a HTTP request.
The result alist has the same form as an alist generated by
`twittering-make-http-request'.

METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\" and \"User-Agent\" are automatically filled
if necessary.
URI specifies the URI including query string.
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted."
  (let* ((parts-alist
	  (let ((parsed-url (url-generic-parse-url uri)))
	    ;; This is required for the difference of url library
	    ;; distributed with Emacs 22 and 23.
	    (cond
	     ((and (fboundp 'url-p) (url-p parsed-url))
	      ;; Emacs 23 and later.
	      `((scheme . ,(url-type parsed-url))
		(host . ,(url-host parsed-url))
		(port . ,(url-portspec parsed-url))
		(path . ,(url-filename parsed-url))))
	     ((vectorp parsed-url)
	      ;; Emacs 22.
	      `((scheme . ,(aref parsed-url 0))
		(host . ,(aref parsed-url 3))
		(port . ,(aref parsed-url 4))
		(path . ,(aref parsed-url 5))))
	     (t
	      nil))))
	 (path (let ((path (cdr (assq 'path parts-alist))))
		 (if (string-match "\\`\\(.*\\)\\?" path)
		     (match-string 1 path)
		   path)))
	 (query-string (let ((path (cdr (assq 'path parts-alist))))
			 (if (string-match "\\?\\(.*\\)\\'" path)
			     (match-string 1 path)
			   nil))))
    (twittering-make-http-request method header-list
				  (cdr (assq 'host parts-alist))
				  (cdr (assq 'port parts-alist))
				  path
				  query-string
				  post-body
				  (string= "https"
					   (cdr (assq 'scheme parts-alist))))))

(defun twittering-make-connection-info (request &optional additional order table)
  "Make an alist specifying the information of connection for REQUEST.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

ADDITIONAL is appended to the tail of the result alist.
Following ADDITIONAL, an entry in TABLE is also appended to the result alist,
where `twittering-lookup-connection-type' determines the entry according to
the priority order ORDER.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.

The parameter symbols are following:
  use-ssl: whether SSL is enabled or not.
  allow-insecure-server-cert: non-nil if an insecure server certificate is
    allowed on SSL.
  cacert-file-fullpath: the full-path of a file including the certificates
    authorizing a server certificate on SSL. The file must be in PEM format.
  use-proxy: non-nil if using a proxy.
  proxy-server: a proxy server or nil.
  proxy-port: a port for connecting the proxy (integer) or nil.
  proxy-user: a username for connecting the proxy or nil.
  proxy-password: a password for connecting the proxy or nil.
  request: an alist specifying a HTTP request."
  (let* ((order (or order twittering-connection-type-order))
	 (table (or table twittering-connection-type-table))
	 (scheme (cdr (assq 'scheme request)))
	 (use-ssl (string= "https" scheme))
	 (entry (twittering-lookup-connection-type use-ssl order table)))
    `((use-ssl . ,use-ssl)
      (allow-insecure-server-cert
       . ,twittering-allow-insecure-server-cert)
      (cacert-file-fullpath
       . ,(when use-ssl (twittering-ensure-ca-cert)))
      (use-proxy . ,twittering-proxy-use)
      ,@(when twittering-proxy-use
	  `((proxy-server . ,(twittering-proxy-info scheme 'server))
	    (proxy-port . ,(twittering-proxy-info scheme 'port))
	    (proxy-user . ,(twittering-proxy-info scheme 'user))
	    (proxy-password . ,(twittering-proxy-info scheme 'password))))
      (request . ,request)
      ,@additional
      ,@entry)))

(defun twittering-get-response-header (buffer)
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (prog1
	      (buffer-substring (point-min) (match-end 0))
	    (when twittering-debug-mode
	      (debug-printf "connection-info=%s\n" connection-info)
	      (debug-print "HTTP response header:\n--BEGIN\n")
	      (debug-print (buffer-substring (point-min) (match-end 0)))
	      (debug-print "--END\n")))
	nil))))

(defun twittering-make-header-info-alist (header-str)
  "Make HTTP header alist from HEADER-STR.
The alist consists of pairs of field-name and field-value, such as
'((\"Content-Type\" . \"application/xml\; charset=utf-8\")
  (\"Content-Length\" . \"2075\"))."
  (let* ((lines (split-string header-str "\r?\n"))
	 (status-line (car lines))
	 (header-lines (cdr lines)))
    (when (string-match
	   "^\\(HTTP/1\\.[01]\\|HTTP/2\\(?:\\.0\\)?\\) \\([0-9][0-9][0-9]\\)\\(.*\\)$"
	   status-line)
      (append `((status-line . ,status-line)
		(http-version . ,(match-string 1 status-line))
		(status-code . ,(match-string 2 status-line))
		(reason-phrase . ,(match-string 3 status-line)))
	      (remove nil
		      (mapcar
		       (lambda (line)
			 (when (string-match "^\\([^: ]*\\): *\\(.*\\)$" line)
			   (cons (match-string 1 line) (match-string 2 line))))
		       header-lines))))))

(defun twittering-get-content-subtype-symbol-from-header-info (header-info)
  "Return a symbol corresponding to the subtype of content-type."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (subtype (when (and (stringp content-type)
			     (string-match "\\` *[^/]*/\\([^ ;]*\\)"
					   content-type))
		    (downcase (match-string 1 content-type))))
	 (symbol-alist
	  '(("json" . json)
	    ("atom+xml" . atom)
	    ("xml" . xml))))
    (cdr (assoc subtype symbol-alist))))

(defun twittering-decode-response-body (header-info)
  "Decode the current buffer according to the content-type in HEADER-INFO."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (parameters (when (stringp content-type)
		       (cdr (split-string content-type ";"))))
	 (regexp "^[[:space:]]*charset=utf-8[[:space:]]*$")
	 (encoded-with-utf-8
	  (let ((case-fold-search t))
	    (remove nil
		    (mapcar (lambda (entry)
			      (string-match regexp entry))
			    parameters)))))
    (when encoded-with-utf-8
      (decode-coding-region (point-min) (point-max) 'utf-8))))

(defun twittering-send-http-request-internal (request additional-info sentinel &optional order table)
  "Open a connection and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.
SENTINEL is called as a function when the process changes state.
It gets three arguments: the process, a string describing the change, and
the connection-info, which is generated by `twittering-make-connection-info'
and also includes an alist ADDITIONAL-INFO.

How to perform the request is selected from TABLE according to the priority
order ORDER. ORDER and TABLE are directly sent to
`twittering-make-connection-info'.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.
"
  (let* ((order (or order twittering-connection-type-order))
	 (table (or table twittering-connection-type-table))
	 (connection-info
	  (twittering-make-connection-info request additional-info
					   order table))
	 (func (cdr (assq 'send-http-request connection-info)))
	 (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory))
    (cond
     ((and func (functionp func))
      (funcall func "*twmode-generic*" temp-buffer
	       connection-info
	       (when (and sentinel (functionp sentinel))
		 (lexical-let ((sentinel sentinel)
			       (connection-info connection-info))
		   (lambda (proc status)
		     (apply sentinel proc status connection-info nil))))))
     (t
      (error "No valid connection method is found")
      nil))))

(defun twittering-send-http-request (request additional-info func &optional clean-up-func)
  "Send a HTTP request and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

FUNC is called when a HTTP response has been received without errors.
It is called with the current buffer containing the HTTP response (without
HTTP headers). FUNC is called with four arguments: the process, a symbol
describing the status of the process, a connection-info generated by
`twittering-make-connection-info', and a header-info generated by
`twittering-get-response-header' and `twittering-make-header-info-alist'.
The connection-info also includes an alist ADDITIONAL-INFO.
If FUNC returns non-nil and `twittering-buffer-related-p' is non-nil, the
returned value is displayed as a message.
And also, if FUNC returns a string and it matches the regular expression
\"^\\\\(Failuare\\\\|Response\\\\): \", the returned value is displayed
as a message.

CLEAN-UP-FUNC is called whenever the sentinel of the subprocess for the
connection is called (as `set-process-sentinel').
It is called with three arguments: the process, a symbol describing the status
of the proess, and a connection-info generated by
`twittering-make-connection-info'.
They are the same as arguments for FUNC.
When a HTTP response has been received, FUNC is called in advance of
CLEAN-UP-FUNC. CLEAN-UP-FUNC can overwrite the message displayed by FUNC.

If the subprocess has exited, the buffer bound to it is automatically killed
after calling CLEAN-UP-FUNC.

The method to perform the request is determined from
`twittering-connection-type-table' according to the priority order
`twittering-connection-type-order'."
  (lexical-let ((func func)
		(clean-up-func clean-up-func))
    (twittering-send-http-request-internal
     request additional-info
     (lambda (proc status-str connection-info)
       (let ((status (cond
		      ((string= status-str "urllib-finished") 'exit)
		      ((processp proc) (process-status proc))
		      (t nil)))
	     (buffer (process-buffer proc))
	     (exit-status (cond
			   ((string= status-str "urllib-finished") 0)
			   ((processp proc) (process-exit-status proc))
			   (t 1)))
	     (command (process-command proc))
	     (pre-process-func
	      (cdr (assq 'pre-process-buffer connection-info)))
	     (mes nil))
	 (unwind-protect
	     (setq mes
		   (cond
		    ((null status)
		     (format "Failure: process %s does not exist" proc))
		    ((or (memq status '(run stop open listen connect))
			 (not (memq status '(exit signal closed failed))))
		     ;; If the process is running, FUNC is not called.
		     nil)
		    ((and command
			  (not (= 0 exit-status)))
		     ;; If the process abnormally exited,
		     (format "Failure: %s exited abnormally (exit-status=%s)"
			     (car command) exit-status))
		    ((not (buffer-live-p buffer))
		     (format "Failure: the buffer for %s is already killed"
			     proc))
		    (t
		     (when (functionp pre-process-func)
		       ;; Pre-process buffer.
		       (funcall pre-process-func proc buffer connection-info))
		     (let* ((header (twittering-get-response-header buffer))
			    (header-info
			     (and header
				  (twittering-make-header-info-alist header))))
		       (with-current-buffer buffer
			 (goto-char (point-min))
			 (when (search-forward-regexp "\r?\n\r?\n" nil t)
			   ;; delete HTTP headers.
			   (delete-region (point-min) (match-end 0)))
			 ;; It may be necessary to decode the contents of
			 ;; the buffer by UTF-8 because
			 ;; `twittering-http-application-headers' specifies
			 ;; utf-8 as one of acceptable charset.
			 ;; For the present, only UTF-8 is taken into account.
			 (twittering-decode-response-body header-info)
			 (apply func proc status connection-info
				header-info nil))))))
	   ;; unwind-forms
	   (setq mes
		 (cond
		  ((null mes)
		   nil)
		  ((string-match "^\\(Failure\\|Response\\): " mes)
		   (let* ((request (cdr (assq 'request connection-info)))
			  (method (cdr (assq 'method request)))
			  (uri (cdr (assq 'uri request))))
		     (concat mes " for " method " " uri)))
		  ((twittering-buffer-related-p)
		   mes)))
	   (when mes
	     ;; CLEAN-UP-FUNC can overwrite a message from the return value
	     ;; of FUNC.
	     (message "%s" mes))
	   (when (functionp clean-up-func)
	     (funcall clean-up-func proc status connection-info))
	   (when (and (memq status '(exit signal closed failed))
		      (buffer-live-p buffer)
		      (not twittering-debug-mode))
	     (kill-buffer buffer))))))))

(eval-when-compile (require 'tls nil t))
(defun twittering-start-http-session-native-tls-p ()
  (when (and (not twittering-proxy-use)
	     (require 'tls nil t))
    (unless twittering-tls-program
      (let ((programs
	     (remove nil
		     (mapcar (lambda (cmd)
			       (when (string-match "\\`\\([^ ]+\\) " cmd)
				 (when (executable-find (match-string 1 cmd))
				   cmd)))
			     tls-program))))
	(setq twittering-tls-program
	      (if twittering-allow-insecure-server-cert
		  (mapcar
		   (lambda (str)
		     (cond
		      ((string-match "^\\([^ ]*/\\)?openssl s_client " str)
		       (concat (match-string 0 str) "-verify 0 "
			       (substring str (match-end 0))))
		      ((string-match "^\\([^ ]*/\\)?gnutls-cli " str)
		       (concat (match-string 0 str) "--insecure "
			       (substring str (match-end 0))))
		      (t
		       str)))
		   programs)
		programs))))
    (not (null twittering-tls-program))))

;; TODO: proxy
(defun twittering-send-http-request-native (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (uri (cdr (assq 'uri connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (host (cdr (assq 'host request)))
	 (port (cdr (assq 'port request)))
	 (path (cdr (assq 'path request)))
	 (query-string (cdr (assq 'query-string request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (proxy-credentials
	  (when (and proxy-user proxy-password)
	    (concat "Basic "
		    (base64-encode-string
		     (concat proxy-user ":" proxy-password)))))
	 (header-list
	  (let ((original-header-list (cdr (assq 'header-list request))))
	    (if proxy-credentials
		(cons
		 `("Proxy-Authorization" ,proxy-credentials)
		 original-header-list)
	      original-header-list)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (connect-host (or proxy-server host))
	 (connect-port (or proxy-port port))
	 (request-str
	  (format "%s %s HTTP/1.1\r\n%s\r\n\r\n%s\r\n"
		  method
		  (if use-proxy
		      ;; As described in 5.1.2 of RFC2616, the
		      ;; absolute URI is required here if the connection
		      ;; uses a proxy.
		      uri
		    (concat path
			    (when query-string
			      (concat "?" query-string))))
		  (mapconcat (lambda (pair)
			       (format "%s: %s" (car pair) (cdr pair)))
			     header-list "\r\n")
		  (or post-body "")))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (tls-program twittering-tls-program)
	 (proc
	  (funcall (if use-ssl
		       'open-tls-stream
		     'open-network-stream)
		   "network-connection-process"
		   nil connect-host connect-port)))
    (when proc
      (set-process-buffer proc buffer)
      (when (functionp sentinel)
	(if (twittering-process-alive-p proc)
	    (set-process-sentinel proc sentinel)
	  (funcall sentinel proc "finished")))
      (process-send-string proc request-str)
      proc)))

(defun twittering-pre-process-buffer-native (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(args (process-command proc)))
    (cond
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar (lambda (cmd)
			      (string-match "^\\(.*/\\)?gnutls-cli\\b" cmd))
			    args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp
		 "- Peer has closed the GNUTLS connection\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar
		     (lambda (cmd)
		       (string-match "^\\(.*/\\)?openssl s_client\\b" cmd))
		     args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp "closed\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     (t
      nil))))

;;;;
;;;; HTTP functions for twitter-like service
;;;;

(defun twittering-http-application-headers (&optional method headers)
  "Return an assoc list of HTTP headers for twittering-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twittering-user-agent)) headers)
    (when (string= "GET" method)
      (push (cons "Accept"
		  (concat
		   "text/xml"
		   ",application/xml"
		   ",application/xhtml+xml"
		   ",application/html;q=0.9"
		   ",text/plain;q=0.8"
		   ",image/png,*/*;q=0.5"))
	    headers)
      (push (cons "Accept-Charset" "utf-8;q=0.7,*;q=0.7")
	    headers))
    (when (string= "POST" method)
      (push (cons "Content-Type" "text/plain") headers))
    headers
    ))

(defun twittering-add-application-header-to-http-request (request account-info)
  "Make a new HTTP request based on REQUEST with the authorization header.
The authorization header is generated from ACCOUNT-INFO.
ACCOUNT-INFO must be an alist that includes the following keys;
  \"screen_name\" and \"password\" if `twittering-auth-method' is 'basic,
  \"screen_name\", \"oauth_token\" and \"oauth_token_secret\" if
  `twittering-auth-method' is 'oauth or 'xauth."
  (let* ((method (cdr (assq 'method request)))
	 (auth-str
	  (cond
	   ((eq twittering-auth-method 'basic)
	    (twittering-make-basic-authentication-string account-info))
	   ((memq twittering-auth-method '(oauth xauth))
	    (twittering-make-oauth-authentication-string account-info request))
	   (t
	    nil)))
	 (cookie-str (twittering-make-cookie-string request account-info))
	 (application-headers
	  `(,@(twittering-http-application-headers method)
	    ("Authorization" . ,auth-str)
	    ,@(when cookie-str
		`(("Cookie" . ,cookie-str))))))
    (mapcar (lambda (entry)
	      (if (eq (car entry) 'header-list)
		  `(header-list
		    . ,(append (cdr entry) application-headers))
		entry))
	    request)))

(defun twittering-get-error-message (header-info connection-info &optional buffer)
  "Return an error message generated from the arguments.
HEADER-INFO must be an alist generated by `twittering-get-response-header'.
CONNECTION-INFO must be an alist generated by
`twittering-make-connection-info'. It may include some additional information
which is added by `twittering-send-http-request'.
BUFFER must be nil or a HTTP response body, which includes error messages from
the server when the HTTP status code equals to 400 or 403.
If BUFFER is nil, the current buffer is used instead."
  (let ((buffer (or buffer (current-buffer)))
	(status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (cond
     ((and (buffer-live-p buffer)
	   (member status-code '("400" "401" "403" "404")))
      ;; Twitter returns an error message as a HTTP response body if
      ;; HTTP status is "400 Bad Request" or "403 Forbidden".
      ;; See "HTTP Response Codes and Errors | dev.twitter.com"
      ;; http://dev.twitter.com/pages/responses_errors .
      ;;
      ;; However, Twitter seems to return an error message even when
      ;; the HTTP status is "401 Unauthorized" or "404 Not Found".
      (let* ((error-mes
	      (cond
	       ((eq format 'xml)
		(let ((xmltree
		       (with-current-buffer buffer
			 (twittering-xml-parse-region (point-min)
						      (point-max)))))
		  (car (cddr (assq 'error (or (assq 'errors xmltree)
					      (assq 'hash xmltree)))))))
	       ((eq format 'json)
		(let ((json-object (with-current-buffer buffer
				     (twittering-json-read))))
		  (cdr (assq 'error json-object))))
	       (t
		;; ATOM is not supported.
		nil))))
	(if error-mes
	    (format "%s (%s)" status-line error-mes)
	  status-line)))
     (t
      status-line))))

(defun twittering-http-get (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send a HTTP GET request with application headers.
ACCOUNT-INFO-ALIST is an alist used by
`twittering-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twittering-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twittering-http-get-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twittering-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twittering-add-application-header-to-http-request
	   (twittering-make-http-request "GET" headers host port path
					 parameters post-body
					 twittering-use-ssl)
	   account-info-alist))
	 (additional-info
	  `((account-info . ,account-info-alist)
	    ,@additional-info)))
    (twittering-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twittering-http-get-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (twittering-case-string
     status-code
     (("200")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((spec (cdr (assq 'timeline-spec connection-info)))
	     (spec-string (cdr (assq 'timeline-spec-string connection-info)))
	     (service-method (cdr (assq 'service-method connection-info)))
	     (statuses
	      (cond
	       ((eq format 'json)
		(let ((json-array (twittering-json-read)))
		  (cond
		   ((null json-array)
		    nil)
		   ((eq (car spec) 'search)
		    (cond
		     ((memq service-method '(twitter statusnet))
		      (mapcar 'twittering-json-object-to-a-status-on-search
			      (cdr (assq 'results json-array))))
		     ((eq service-method 'twitter-api-v1.1)
		      (mapcar 'twittering-json-object-to-a-status
			      (cdr (assq 'statuses json-array))))))
		   ((twittering-timeline-spec-is-direct-messages-p spec)
		    (mapcar
		     'twittering-json-object-to-a-status-on-direct-messages
		     json-array))
		   (t
		    (mapcar 'twittering-json-object-to-a-status
			    json-array)))))
	       ((eq format 'xml)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twittering-xmltree-to-status xmltree))))
	       ((eq format 'atom)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twittering-atom-xmltree-to-status xmltree))))
	       (t
		nil)))
	     (rendered-tweets nil))
	(let ((updated-timeline-info
	       (twittering-add-statuses-to-timeline-data statuses spec))
	      (buffer (twittering-get-buffer-from-spec spec)))
	  ;; FIXME: We should retrieve un-retrieved statuses until
	  ;; statuses is nil. twitter server returns nil as
	  ;; xmltree with HTTP status-code is "200" when we
	  ;; retrieved all un-retrieved statuses.
	  (if twittering-notify-successful-http-get
	      (if updated-timeline-info
		  (concat
		   (format "Fetching %s. Success. " spec-string)
		   (mapconcat
		    (lambda (info)
		      (let ((spec-string (nth 0 info))
			    (num (nth 1 info)))
			(format "%s: +%d" spec-string num)))
		    updated-timeline-info
		    ", "))
		(format "Fetching %s. Success. (No new tweets)"
			spec-string))
	    nil))))
     (("404")
      ;; The requested resource does not exist.
      (let ((spec (cdr (assq 'timeline-spec connection-info)))
	    (spec-string (cdr (assq 'timeline-spec-string connection-info))))
	;; Remove specs related to the invalid spec from history.
	(mapc
	 (lambda (buffer)
	   (let ((other-spec (twittering-get-timeline-spec-for-buffer buffer))
		 (other-spec-string
		  (twittering-get-timeline-spec-string-for-buffer buffer)))
	     (when (twittering-timeline-spec-depending-on-p other-spec spec)
	       (twittering-remove-timeline-spec-string-from-history
		other-spec-string))))
	 (twittering-get-buffer-list)))
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info)))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-retrieve-single-tweet-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (twittering-case-string
     status-code
     (("200" "403" "404")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((id (cdr (assq 'id connection-info)))
	     (user-screen-name (cdr (assq 'user-screen-name connection-info)))
	     (status
	      (cond
	       ((string= status-code "403")
		;; Forbidden. Maybe a protected tweet?
		(twittering-make-alist-of-forbidden-tweet id
							  user-screen-name))
	       ((string= status-code "404")
		;; The requested resource does not exist.
		(twittering-make-alist-of-non-existent-tweet id
							     user-screen-name))
	       ((eq format 'json)
		(let ((json-object (twittering-json-read)))
		  (twittering-json-object-to-a-status json-object)))
	       ((eq format 'xml)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (car
		     (twittering-xmltree-to-status
		      `((statuses nil ,@xmltree)))))))
	       (t
		nil))))
	(when status
	  (twittering-add-statuses-to-timeline-data `(,status) '(:single))
	  (let ((buffer (cdr (assq 'buffer connection-info)))
		(spec (cdr (assq 'timeline-spec connection-info)))
		(prop
		 (cdr (assq 'property-to-be-redisplayed connection-info))))
	    (cond
	     ((null prop)
	      ;; The process has been invoked via `twittering-call-api' with
	      ;; the command `retrieve-timeline', not the command
	      ;; `retrieve-single-tweet' for rendering a replied tweet.
	      ;; No special property that specifies regions being re-rendered
	      ;; is given.
	      (let ((new-statuses `(,status))
		    (buffer (twittering-get-buffer-from-spec spec)))
		(when (and new-statuses buffer)
		  (twittering-render-timeline buffer new-statuses t))))
	     ((and buffer prop (buffer-live-p buffer))
	      (twittering-redisplay-status-on-each-buffer buffer prop)
	      (with-current-buffer buffer
		(save-excursion
		  (let ((buffer-read-only nil))
		    (lexical-let ((prop prop))
		      (twittering-for-each-property-region
		       prop
		       (lambda (beg end value)
			 ;; Remove the property required no longer.
			 (remove-text-properties beg end `(,prop nil))
			 (goto-char beg)
			 (twittering-render-replied-statuses)))))))))))
	(cond
	 ((string= status-code "403")
	  (format "You are not authorized to see this tweet (ID %s)." id))
	 ((string= status-code "404")
	  (format "The tweet with ID %s does not exist." id))
	 (twittering-notify-successful-http-get
	  (format "Fetching %s.  Success." id))
	 (t
	  nil))))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defmacro twittering-http-get-list-sentinel-base (what)
  `(let ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (format
	  (twittering-get-content-subtype-symbol-from-header-info header-info))
	 (indexes nil)
	 (mes nil))
     (twittering-case-string
      status-code
      (("200")
       (cond
	((eq format 'xml)
	 (let ((xmltree (twittering-xml-parse-region (point-min) (point-max))))
	   (when xmltree
	     (setq indexes
		   (mapcar
		    (lambda (c-node)
		      (caddr (assq ,what c-node)))
		    (remove nil
			    (mapcar
			     (lambda (node)
			       (and (consp node) (eq 'list (car node))
				    node))
			     (cdr-safe
			      (assq 'lists (assq 'lists_list xmltree))))
			    ))
		   ))))
	((eq format 'json)
	 (let* ((json-object (twittering-json-read))
		(json-list
		 (cond
		  ((arrayp json-object)
		   ;; GET lists/list in the Twitter REST API v1.1 returns
		   ;; an array.
		   json-object)
		  (t
		   ;; GET lists/subscriptions in the Twitter REST API v1.1
		   ;; returns an alist.
		   (cdr (assq 'lists json-object))))))
	   (when json-object
	     (setq indexes
		   (mapcar (lambda (entry)
			     (cdr (assq ,what entry)))
			   json-list)))))
	(t
	 (error "Format \"%s\" is not supported" format)
	 nil)))
      (t
       (setq mes (format "Response: %s"
			 (twittering-get-error-message header-info
						       connection-info)))))
     (setq twittering-list-index-retrieved
	   (or indexes
	       mes
	       "")) ;; set "" explicitly if user does not have a list.
     mes))

(defun twittering-http-get-list-index-sentinel (proc status connection-info header-info)
  (twittering-http-get-list-sentinel-base 'slug))

(defun twittering-http-get-list-subscriptions-sentinel (proc status connection-info header-info)
  (let ((result (twittering-http-get-list-sentinel-base 'full_name)))
    (when (listp twittering-list-index-retrieved)
      (setq twittering-list-index-retrieved
	    (mapcar (lambda (str)
		      (and (string-match "\\`@\\(.*\\)\\'" str)
			   (match-string 1 str)))
		    twittering-list-index-retrieved)))
    result))

(defun twittering-http-post (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send HTTP POST request to api.twitter.com (or search.twitter.com)
ACCOUNT-INFO-ALIST is an alist used by
`twittering-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twittering-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twittering-http-post-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twittering-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twittering-add-application-header-to-http-request
	   (twittering-make-http-request "POST" headers host port path
					 parameters post-body
					 twittering-use-ssl)
	   account-info-alist))
	 (additional-info `((account-info . ,account-info-alist)
			    ,@additional-info)))
    (twittering-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twittering-http-post-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (twittering-case-string
     status-code
     (("200")
      "Success: Post.")
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-http-post-destroy-status-sentinel (proc status connection-info header-info)
  "A sentinel for deleting a status invoked via `twittering-call-api'."
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (twittering-case-string
     status-code
     (("200")
      (let* ((params
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twittering-xml-parse-region (point-min) (point-max))))
		  `((id . ,(elt (assq 'id (assq 'status xml)) 2))
		    (text . ,(elt (assq 'text (assq 'status xml)) 2)))))
	       ((eq format 'json)
		(let ((json-object (twittering-json-read)))
		  `((id . ,(cdr (assq 'id_str json-object)))
		    (text . ,(cdr (assq 'text json-object))))))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (id (cdr (assq 'id params)))
	     (text (cdr (assq 'text params))))
	(cond
	 (id
	  (twittering-delete-status-from-data-table id)
	  (format "Deleting \"%s\". Success." text))
	 (t
	  "Failure: the response for deletion could not be parsed."))))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

;;;;
;;;; CA certificate
;;;;

(defvar twittering-cert-file nil
  "The full-path of the file including the certificates \
authorizing servers on SSL.")

(defconst twittering-ca-cert-list
  '(
;; #BEGIN-CERTIFICATE
;; Verisign Class 3 Public Primary Certification Authority - G3
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G3
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G3
;; serial=9B7E0649A33E62B9D5EE90487129EF57
;; SHA1 Fingerprint=13:2D:0D:45:53:4B:69:97:CD:B2:D5:C3:39:E2:55:76:60:9B:5C:C6
;; notBefore=Oct  1 00:00:00 1999 GMT
;; notAfter=Jul 16 23:59:59 2036 GMT
"-----BEGIN CERTIFICATE-----
MIIEGjCCAwICEQCbfgZJoz5iudXukEhxKe9XMA0GCSqGSIb3DQEBBQUAMIHKMQsw
CQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZl
cmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWdu
LCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlT
aWduIENsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3Jp
dHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3MTYyMzU5NTlaMIHKMQswCQYD
VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlT
aWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWduLCBJ
bmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWdu
IENsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkg
LSBHMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMu6nFL8eB8aHm8b
N3O9+MlrlBIwT/A2R/XQkQr1F8ilYcEWQE37imGQ5XYgwREGfassbqb1EUGO+i2t
KmFZpGcmTNDovFJbcCAEWNF6yaRpvIMXZK0Fi7zQWM6NjPXr8EJJC52XJ2cybuGu
kxUccLwgTS8Y3pKI6GyFVxEa6X7jJhFUokWWVYPKMIno3Nij7SqAP395ZVc+FSBm
CC+Vk7+qRy+oRpfwEuL+wgorUeZ25rdGt+INpsyow0xZVYnm6FNcHOqd8GIWC6fJ
Xwzw3sJ2zq/3avL6QaaiMxTJ5Xpj055iN9WFZZ4O5lMkdBteHRJTW8cs54NJOxWu
imi5V5cCAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAERSWwauSCPc/L8my/uRan2Te
2yFPhpk0djZX3dAVL8WtfxUfN2JzPtTnX84XA9s1+ivbrmAJXx5fj267Cz3qWhMe
DGBvtcC1IyIuBwvLqXTLR7sdwdela8wv0kL9Sd2nic9TutoAWii/gt/4uhMdUIaC
/Y4wjylGsB49Ndo4YhYYSq3mtlFs3q9i6wHQHiT+eo8SGhJouPtmmRQURVyu565p
F4ErWjfJXir0xuKhXFSbplQAz/DxwceYMBo7Nhbbo27q/a2ywtrvAkcTisDxszGt
TxzhT5yvDwyd93gN2PQ1VoDat20Xj50egWTh/sVFuq1ruQp6Tk9LhO5L8X3dEQ==
-----END CERTIFICATE-----
"
;; GeoTrust Global CA
;; issuer= /C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
;; subject= /C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
;; serial=023456
;; SHA1 Fingerprint=DE:28:F4:A4:FF:E5:B9:2F:A3:C5:03:D1:A3:49:A7:F9:96:2A:82:12
;; notBefore=May 21 04:00:00 2002 GMT
;; notAfter=May 21 04:00:00 2022 GMT
"-----BEGIN CERTIFICATE-----
MIIDVDCCAjygAwIBAgIDAjRWMA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYTAlVT
MRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMRswGQYDVQQDExJHZW9UcnVzdCBHbG9i
YWwgQ0EwHhcNMDIwNTIxMDQwMDAwWhcNMjIwNTIxMDQwMDAwWjBCMQswCQYDVQQG
EwJVUzEWMBQGA1UEChMNR2VvVHJ1c3QgSW5jLjEbMBkGA1UEAxMSR2VvVHJ1c3Qg
R2xvYmFsIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2swYYzD9
9BcjGlZ+W988bDjkcbd4kdS8odhM+KhDtgPpTSEHCIjaWC9mOSm9BXiLnTjoBbdq
fnGk5sRgprDvgOSJKA+eJdbtg/OtppHHmMlCGDUUna2YRpIuT8rxh0PBFpVXLVDv
iS2Aelet8u5fa9IAjbkU+BQVNdnARqN7csiRv8lVK83Qlz6cJmTM386DGXHKTubU
1XupGc1V3sjs0l44U+VcT4wt/lAjNvxm5suOpDkZALeVAjmRCw7+OC7RHQWa9k0+
bw8HHa8sHo9gOeL6NlMTOdReJivbPagUvTLrGAMoUgRx5aszPeE4uwc2hGKceeoW
MPRfwCvocWvk+QIDAQABo1MwUTAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBTA
ephojYn7qwVkDBF9qn1luMrMTjAfBgNVHSMEGDAWgBTAephojYn7qwVkDBF9qn1l
uMrMTjANBgkqhkiG9w0BAQUFAAOCAQEANeMpauUvXVSOKVCUn5kaFOSPeCpilKIn
Z57QzxpeR+nBsqTP3UEaBU6bS+5Kb1VSsyShNwrrZHYqLizz/Tt1kL/6cdjHPTfS
tQWVYrmm3ok9Nns4d0iXrKYgjy6myQzCsplFAMfOEVEiIuCl6rYVSAlk6l5PdPcF
PseKUgzbFbS9bZvlxrFUaKnjaZC2mqUPuLk/IH2uSrW4nOQdtqvmlKXBx4Ot2/Un
hw4EbNX/3aBd7YdStysVAq45pmp06drE57xNNB6pXE0zX5IJL4hmXXeXxx12E6nV
5fEWCRE11azbJHFwLJhWC9kXtNHjUStedejV0NxPNO3CBWaAocvmMw==
-----END CERTIFICATE-----
"
;; DigiCert High Assurance EV Root CA
;; issuer= /C=US/O=DigiCert Inc/OU=www.digicert.com/CN=DigiCert High Assurance EV Root CA
;; subject= /C=US/O=DigiCert Inc/OU=www.digicert.com/CN=DigiCert High Assurance EV Root CA
;; serial=02AC5C266A0B409B8F0B79F2AE462577
;; SHA1 Fingerprint=5F:B7:EE:06:33:E2:59:DB:AD:0C:4C:9A:E6:D3:8F:1A:61:C7:DC:25
;; notBefore=Nov 10 00:00:00 2006 GMT
;; notAfter=Nov 10 00:00:00 2031 GMT
"-----BEGIN CERTIFICATE-----
MIIDxTCCAq2gAwIBAgIQAqxcJmoLQJuPC3nyrkYldzANBgkqhkiG9w0BAQUFADBs
MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3
d3cuZGlnaWNlcnQuY29tMSswKQYDVQQDEyJEaWdpQ2VydCBIaWdoIEFzc3VyYW5j
ZSBFViBSb290IENBMB4XDTA2MTExMDAwMDAwMFoXDTMxMTExMDAwMDAwMFowbDEL
MAkGA1UEBhMCVVMxFTATBgNVBAoTDERpZ2lDZXJ0IEluYzEZMBcGA1UECxMQd3d3
LmRpZ2ljZXJ0LmNvbTErMCkGA1UEAxMiRGlnaUNlcnQgSGlnaCBBc3N1cmFuY2Ug
RVYgUm9vdCBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMbM5XPm
+9S75S0tMqbf5YE/yc0lSbZxKsPVlDRnogocsF9ppkCxxLeyj9CYpKlBWTrT3JTW
PNt0OKRKzE0lgvdKpVMSOO7zSW1xkX5jtqumX8OkhPhPYlG++MXs2ziS4wblCJEM
xChBVfvLWokVfnHoNb9Ncgk9vjo4UFt3MRuNs8ckRZqnrG0AFFoEt7oT61EKmEFB
Ik5lYYeBQVCmeVyJ3hlKV9Uu5l0cUyx+mM0aBhakaHPQNAQTXKFx01p8VdteZOE3
hzBWBOURtCmAEvF5OYiiAhF8J2a3iLd48soKqDirCmTCv2ZdlYTBoSUeh10aUAsg
EsxBu24LUTi4S8sCAwEAAaNjMGEwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQF
MAMBAf8wHQYDVR0OBBYEFLE+w2kD+L9HAdSYJhoIAu9jZCvDMB8GA1UdIwQYMBaA
FLE+w2kD+L9HAdSYJhoIAu9jZCvDMA0GCSqGSIb3DQEBBQUAA4IBAQAcGgaX3Nec
nzyIZgYIVyHbIUf4KmeqvxgydkAQV8GK83rZEWWONfqe/EW1ntlMMUu4kehDLI6z
eM7b41N5cdblIZQB2lWHmiRk9opmzN6cN82oNLFpmyPInngiK3BD41VHMWEZ71jF
hS9OMPagMRYjyOfiZRYzy78aG6A9+MpeizGLYAiJLQwGXFK3xPkKmNEVX58Svnw2
Yzi9RKR/5CYrCsSXaQ3pjOLAEFe4yHYSkVXySGnYvCoCWw9E1CAx2/S6cCZdkGCe
vEsXCS+0yx5DaMkHJ8HSXPfqIbloEpw8nL+e/IBcm2PN7EeqJSdnoDfzAIJ9VNep
+OkuE6N36B9K
-----END CERTIFICATE-----
"
;; VeriSign Class 3 Public Primary Certification Authority - G5
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2006 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G5
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2006 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G5
;; serial=18DAD19E267DE8BB4A2158CDCC6B3B4A
;; SHA1 Fingerprint=4E:B6:D5:78:49:9B:1C:CF:5F:58:1E:AD:56:BE:3D:9B:67:44:A5:E5
;; notBefore=Nov  8 00:00:00 2006 GMT
;; notAfter=Jul 16 23:59:59 2036 GMT
"-----BEGIN CERTIFICATE-----
MIIE0zCCA7ugAwIBAgIQGNrRniZ96LtKIVjNzGs7SjANBgkqhkiG9w0BAQUFADCB
yjELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQL
ExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNiBWZXJp
U2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxW
ZXJpU2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0
aG9yaXR5IC0gRzUwHhcNMDYxMTA4MDAwMDAwWhcNMzYwNzE2MjM1OTU5WjCByjEL
MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZW
ZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNiBWZXJpU2ln
biwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJp
U2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y
aXR5IC0gRzUwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCvJAgIKXo1
nmAMqudLO07cfLw8RRy7K+D+KQL5VwijZIUVJ/XxrcgxiV0i6CqqpkKzj/i5Vbex
t0uz/o9+B1fs70PbZmIVYc9gDaTY3vjgw2IIPVQT60nKWVSFJuUrjxuf6/WhkcIz
SdhDY2pSS9KP6HBRTdGJaXvHcPaz3BJ023tdS1bTlr8Vd6Gw9KIl8q8ckmcY5fQG
BO+QueQA5N06tRn/Arr0PO7gi+s3i+z016zy9vA9r911kTMZHRxAy3QkGSGT2RT+
rCpSx4/VBEnkjWNHiDxpg8v+R70rfk/Fla4OndTRQ8Bnc+MUCH7lP59zuDMKz10/
NIeWiu5T6CUVAgMBAAGjgbIwga8wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8E
BAMCAQYwbQYIKwYBBQUHAQwEYTBfoV2gWzBZMFcwVRYJaW1hZ2UvZ2lmMCEwHzAH
BgUrDgMCGgQUj+XTGoasjY5rw8+AatRIGCx7GS4wJRYjaHR0cDovL2xvZ28udmVy
aXNpZ24uY29tL3ZzbG9nby5naWYwHQYDVR0OBBYEFH/TZafC3ey78DAJ80M5+gKv
MzEzMA0GCSqGSIb3DQEBBQUAA4IBAQCTJEowX2LP2BqYLz3q3JktvXf2pXkiOOzE
p6B4Eq1iDkVwZMXnl2YtmAl+X6/WzChl8gGqCBpH3vn5fJJaCGkgDdk+bW48DW7Y
5gaRQBi5+MHt39tBquCWIMnNZBU4gcmU7qKEKQsTb47bDN0lAtukixlE0kF6BWlK
WE9gyn6CagsCqiUXObXbf+eEZSqVir2G3l6BFoMtEMze/aiCKm0oHw0LxOXnGiYZ
4fQRbxC1lfznQgUy286dUV4otp6F01vvpX1FQHKOtw5rDgb7MzVIcbidJ4vEZV8N
hnacRHr2lVz2XTIIM6RUthg/aFzyQkqFOFSDX9HoLPKsEdao7WNq
-----END CERTIFICATE-----
"
;; VeriSign Universal Root Certification Authority
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2008 VeriSign, Inc. - For authorized use only/CN=VeriSign Universal Root Certification Authority
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2008 VeriSign, Inc. - For authorized use only/CN=VeriSign Universal Root Certification Authority
;; serial=401AC46421B31321030EBBE4121AC51D
;; SHA1 Fingerprint=36:79:CA:35:66:87:72:30:4D:30:A5:FB:87:3B:0F:A7:7B:B7:0D:54
;; notBefore=Apr  2 00:00:00 2008 GMT
;; notAfter=Dec  1 23:59:59 2037 GMT
"-----BEGIN CERTIFICATE-----
MIIEuTCCA6GgAwIBAgIQQBrEZCGzEyEDDrvkEhrFHTANBgkqhkiG9w0BAQsFADCB
vTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQL
ExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwOCBWZXJp
U2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MTgwNgYDVQQDEy9W
ZXJpU2lnbiBVbml2ZXJzYWwgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTAe
Fw0wODA0MDIwMDAwMDBaFw0zNzEyMDEyMzU5NTlaMIG9MQswCQYDVQQGEwJVUzEX
MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0
IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAyMDA4IFZlcmlTaWduLCBJbmMuIC0gRm9y
IGF1dGhvcml6ZWQgdXNlIG9ubHkxODA2BgNVBAMTL1ZlcmlTaWduIFVuaXZlcnNh
bCBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEAx2E3XrEBNNti1xWb/1hajCMj1mCOkdeQmIN65lgZOIzF
9uVkhbSicfvtvbnazU0AtMgtc6XHaXGVHzk8skQHnOgO+k1KxCHfKWGPMiJhgsWH
H26MfF8WIFFE0XBPV+rjHOPMee5Y2A7Cs0WTwCznmhcrewA3ekEzeOEz4vMQGn+H
LL729fdC4uW/h2KJXwBL38Xd5HVEMkE6HnFuacsLdUYI0crSK5XQz/u5QGtkjFdN
/BMReYTtXlT2NJ8IAfMQJQYXStrxHXpma5hgZqTZ79IugvHw7wnqRMkVauIDbjPT
rJ9VAMf2CGqUuV/c4DPxhGD5WycRtPwW8rtWaoAljQIDAQABo4GyMIGvMA8GA1Ud
EwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgEGMG0GCCsGAQUFBwEMBGEwX6FdoFsw
WTBXMFUWCWltYWdlL2dpZjAhMB8wBwYFKw4DAhoEFI/l0xqGrI2Oa8PPgGrUSBgs
exkuMCUWI2h0dHA6Ly9sb2dvLnZlcmlzaWduLmNvbS92c2xvZ28uZ2lmMB0GA1Ud
DgQWBBS2d/ppSEefUxLVwuoHMnYH0ZcHGTANBgkqhkiG9w0BAQsFAAOCAQEASvj4
sAPmLGd75JR3Y8xuTPl9Dg3cyLk1uXBPY/ok+myDjEedO2Pzmvl2MpWRsXe8rJq+
seQxIcaBlVZaDrHC1LGmWazxY8u4TB1ZkErvkBYoH1quEPuBUDgMbMzxPcP1Y+Oz
4yHJJDnp/RVmRvQbEdBNc6N9Rvk97ahfYtTxP/jgdFcrGJ2BtMQo2pSXpXDrrB2+
BxHw1dvd5Yzw1TKwg+ZX4o+/vqGqvz0dtdQ46tewXDpPaj+PwGZsY6rp2aQW9IHR
lRQOfc2VNNnSj3BzgXucfr2YYdhFh5iQxeuGMMY1v/D/w1WIg0vvBZIGcfK4mJO3
7M2CYfE45k+XmCpajQ==
-----END CERTIFICATE-----
"
;; VeriSign Class 3 Public Primary Certification Authority - G4
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2007 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G4
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2007 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G4
;; serial=2F80FE238C0E220F486712289187ACB3
;; SHA1 Fingerprint=22:D5:D8:DF:8F:02:31:D1:8D:F7:9D:B7:CF:8A:2D:64:C9:3F:6C:3A
;; notBefore=Nov  5 00:00:00 2007 GMT
;; notAfter=Jan 18 23:59:59 2038 GMT
"-----BEGIN CERTIFICATE-----
MIIDhDCCAwqgAwIBAgIQL4D+I4wOIg9IZxIokYesszAKBggqhkjOPQQDAzCByjEL
MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZW
ZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNyBWZXJpU2ln
biwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJp
U2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y
aXR5IC0gRzQwHhcNMDcxMTA1MDAwMDAwWhcNMzgwMTE4MjM1OTU5WjCByjELMAkG
A1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZWZXJp
U2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNyBWZXJpU2lnbiwg
SW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJpU2ln
biBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9yaXR5
IC0gRzQwdjAQBgcqhkjOPQIBBgUrgQQAIgNiAASnVnp8Utpkmw4tXNherJI9/gHm
GUo9FANL+mAnINmDiWn6VMaaGF5VKmTeBvaNSjutEDxlPZCIBIngMGGzrl0Bp3ve
fLK+ymVhAIau2o970ImtTR1ZmkGxvEeA3J5iw/mjgbIwga8wDwYDVR0TAQH/BAUw
AwEB/zAOBgNVHQ8BAf8EBAMCAQYwbQYIKwYBBQUHAQwEYTBfoV2gWzBZMFcwVRYJ
aW1hZ2UvZ2lmMCEwHzAHBgUrDgMCGgQUj+XTGoasjY5rw8+AatRIGCx7GS4wJRYj
aHR0cDovL2xvZ28udmVyaXNpZ24uY29tL3ZzbG9nby5naWYwHQYDVR0OBBYEFLMW
kf3upm7ktS5Jj4d4gYDs5bG1MAoGCCqGSM49BAMDA2gAMGUCMGYhDBgmYFo4e1ZC
4Kf8NoRRkSAsdk1DPcQdhCPQrNZ8NQbOzWm9kA3bbEhCHQ6qQgIxAJw9SDkjOVga
FRJZap7v1VmyHVIsmXHNxynfGyphe3HR3vPA5Q06Sqotp9iGKt0uEA==
-----END CERTIFICATE-----
"
;; #END-CERTIFICATE
))

(defun twittering-delete-ca-cert ()
  (when (and twittering-cert-file
	     (file-exists-p twittering-cert-file))
    (delete-file twittering-cert-file))
  (setq twittering-cert-file nil))

(defun twittering-ensure-ca-cert ()
  "Return a full-path of the file including CA certificates.

If it does not exist, create it.
The directory includes root certificates in \"hash format\". In detail, see verify(1SSL)."
  (unless twittering-cert-file
    (let ((coding-system-for-write 'iso-safe)
	  (file (make-temp-file "twmode-cacert")))
      (with-temp-file file
	(apply 'insert twittering-ca-cert-list))
      (setq twittering-cert-file file)
      (add-hook 'kill-emacs-hook 'twittering-delete-ca-cert)))
  twittering-cert-file)

;;;;
;;;; User agent
;;;;

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (format "Emacs/%d.%d Twittering-mode/%s"
	  emacs-major-version emacs-minor-version
	  twittering-mode-version))

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

;;;;
;;;; Async HTTP
;;;;

(defvar twittering-url-data-hash (make-hash-table :test 'equal))
(defvar twittering-url-request-list nil)
(defvar twittering-url-request-sentinel-hash (make-hash-table :test 'equal))
(defvar twittering-internal-url-queue nil)
(defvar twittering-url-request-resolving-p nil)
(defvar twittering-url-request-retry-limit 3)
(defvar twittering-url-request-sentinel-delay 1.0
  "*Delay from completing retrieval to invoking associated sentinels.
Sentinels registered by `twittering-url-retrieve-async' will be invoked
after retrieval is completed and Emacs remains idle a certain time, which
this variable specifies. The unit is second.")

(defun twittering-http-get-verify-credentials-sentinel (proc status connection-info header-info)
  (let* ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (username
	  (twittering-get-from-account-info "screen_name" account-info))
	 (password
	  (twittering-get-from-account-info "password" account-info)))
    (twittering-case-string
     status-code
     (("200")
      (twittering-register-account-info account-info)
      (setq twittering-account-authorization 'authorized)
      (message "Authorization for the account \"%s\" succeeded." username)
      nil)
     (("401")
      (setq twittering-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed. Type M-x twit to retry with correct information."
		     username)))
	;; Invalidate the account info.
	(twittering-register-account-info nil)
	(message "%s" error-mes)
	nil))
     (t
      (setq twittering-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed due to \"%s\"."
		     username status-line)))
	(message "%s" error-mes)
	nil)))))

(defun twittering-http-get-verify-credentials-clean-up-sentinel (proc status connection-info)
  (when (and (memq status '(exit signal closed failed))
	     (eq twittering-account-authorization 'queried))
    (setq twittering-account-authorization nil)
    (let ((exit-status (cond
			((processp proc) (process-exit-status proc))
			(t 0)))
	  (command (process-command proc)))
      (if (= 0 exit-status)
	  (message "Authorization failed. Type M-x twit to retry.")
	(message "Authorization failed: %s exited abnormally (exit-status=%s)."
		 (car command) exit-status)))
    (setq twittering-username nil)
    (setq twittering-password nil)))

(defun twittering-remove-redundant-queries (queue)
  (remove nil
	  (mapcar
	   (lambda (url)
	     (let ((current (gethash url twittering-url-data-hash)))
	       (when (or (null current)
			 (and (integerp current)
			      (< current twittering-url-request-retry-limit)))
		 url)))
	   (delete-dups queue))))

(defun twittering-url-retrieve-async-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(uri (cdr (assq 'uri (assq 'request connection-info)))))
    (when (string= status-code "200")
      (let ((body (string-as-unibyte (buffer-string))))
	(puthash uri body twittering-url-data-hash)
	(setq twittering-internal-url-queue
	      (remove uri twittering-internal-url-queue))
	(let ((sentinels (gethash uri twittering-url-request-sentinel-hash)))
	  (when sentinels
	    (remhash uri twittering-url-request-sentinel-hash))
	  (twittering-run-on-idle twittering-url-request-sentinel-delay
				  (lambda (sentinels uri body)
				    (mapc (lambda (func)
					    (funcall func uri body))
					  sentinels)
				    ;; Resolve the rest of requests.
				    (setq twittering-url-request-resolving-p
					  nil)
				    (twittering-resolve-url-request))
				  sentinels uri body)
	  ;;  Without the following nil, it seems that the value of
	  ;; `sentinels' is displayed.
	  nil)))))

(defun twittering-url-retrieve-async-clean-up-sentinel (proc status connection-info)
  (when (memq status '(exit signal closed failed))
    (let* ((uri (cdr (assq 'uri connection-info)))
	   (current (gethash uri twittering-url-data-hash)))
      (when (or (null current) (integerp current))
	;; Increment the counter on failure and then retry retrieval.
	(puthash uri (1+ (or current 0)) twittering-url-data-hash)
	(setq twittering-url-request-resolving-p nil)
	(twittering-resolve-url-request)))))

(defun twittering-resolve-url-request ()
  "Resolve requests of asynchronous URL retrieval."
  (when (null twittering-url-request-resolving-p)
    (setq twittering-url-request-resolving-p t)
    ;; It is assumed that the following part is not processed
    ;; in parallel.
    (setq twittering-internal-url-queue
	  (append twittering-internal-url-queue twittering-url-request-list))
    (setq twittering-url-request-list nil)
    (setq twittering-internal-url-queue
	  (twittering-remove-redundant-queries twittering-internal-url-queue))
    (if (null twittering-internal-url-queue)
	(setq twittering-url-request-resolving-p nil)
      (let* ((url (car twittering-internal-url-queue))
	     (request (twittering-make-http-request-from-uri "GET" nil url))
	     (additional-info `((uri . ,url))))
	(twittering-send-http-request
	 request additional-info
	 'twittering-url-retrieve-async-sentinel
	 'twittering-url-retrieve-async-clean-up-sentinel)))))

(defun twittering-url-retrieve-async (url &optional sentinel)
  "Retrieve URL asynchronously and call SENTINEL with the retrieved data.
The request is placed at the last of queries queue. When the data has been
retrieved and Emacs remains idle a certain time specified by
`twittering-url-request-sentinel-delay', SENTINEL will be called as
 (funcall SENTINEL URL url-data).
The retrieved data can be referred as (gethash URL twittering-url-data-hash)."
  (let ((data (gethash url twittering-url-data-hash)))
    (cond
     ((or (null data) (integerp data))
      (add-to-list 'twittering-url-request-list url t)
      (when sentinel
	(let ((current (gethash url twittering-url-request-sentinel-hash)))
	  (unless (member sentinel current)
	    (puthash url (cons sentinel current)
		     twittering-url-request-sentinel-hash))))
      (twittering-resolve-url-request)
      nil)
     (t
      ;; URL has been already retrieved.
      (twittering-run-on-idle twittering-url-request-sentinel-delay
			      sentinel url data)
      data))))

(provide 'twittering-http)

;;; twittering-http.el ends here
