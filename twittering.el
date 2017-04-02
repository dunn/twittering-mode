;;; twittering.el --- Twitter API client and posting-station backend

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard
;;               2017 Alex Dunn

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;	Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
;;	Xavier Maillard <xavier@maillard.im>
;; Created: Sep 4, 2007
;; Version: 0.1.0
;; Keywords: twitter api
;; Prefix: twittering

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar twittering-api-host "api.twitter.com")
(defvar twittering-api-search-host "search.twitter.com")
(defvar twittering-web-host "twitter.com")
(defvar twittering-oauth-request-token-url-without-scheme
  "://api.twitter.com/oauth/request_token")
(defvar twittering-oauth-authorization-url-base-without-scheme
  "://api.twitter.com/oauth/authorize?oauth_token=")
(defvar twittering-oauth-access-token-url-without-scheme
  "://api.twitter.com/oauth/access_token")

(defcustom twittering-username nil
  "*A username of your Twitter account."
  :type '(choice (const nil)
		 string)
  :group 'posting-station)

(defcustom twittering-password nil
  "*The password for your Twitter account.
Leaving it blank is recommended because writing a password in .emacs file is so
dangerous."
  :type '(choice (const nil)
		 string)
  :group 'posting-station)

(defcustom twittering-auth-method 'oauth
  "*Authentication method to use with `twittering-mode'.

Choose between symbols `oauth' (default), `basic' or `xauth'.

OAuth Authentication requires `twittering-oauth-consumer-key' and
`twittering-oauth-consumer-secret'.

Additionally, it requires an external command `curl' or another
command included in `tls-program', which may be `openssl' or
`gnutls-cli', for SSL."
  :group 'twittering-mode
  :type '(choice :tag "Twitter authentication method"
		 (const :tag "Basic authentication" :value basic)
		 (const :tag "OAuth authentication" :value oauth)
		 (const :tag "xAuth authentication" :value xauth)))

(defvar twittering-account-authorization nil
  "State of account authorization for `twittering-username' and \
`twittering-password'.  The value is one of the following symbols:
nil -- The account have not been authorized yet.
queried -- The authorization has been queried, but not finished yet.
authorized -- The account has been authorized.")

(defcustom twittering-oauth-use-ssl t
  "*If non-nil, use SSL authentication for OAuth.

Twitter requires SSL on authorization via OAuth."
  :group 'twittering-mode
  :type 'boolean)

(defcustom twittering-oauth-invoke-browser nil
  "*If non-nil, invoke a browser on authorization of access key automatically."
  :type 'boolean
  :group 'twittering-mode)

(defvar twittering-oauth-consumer-key nil)
(defvar twittering-oauth-consumer-secret nil)
(defvar twittering-oauth-access-token-alist nil)

(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defcustom twittering-number-of-tweets-on-retrieval 20
  "*Number of tweets which will be retrieved in one request.

The upper limit is `twittering-max-number-of-tweets-on-retrieval'."
  :type 'integer
  :group 'twittering-mode)

(defvar twittering-server-info-alist nil
  "Alist of server information.")

(defvar twittering-api-limit-info-alist '()
  "Alist of an API identifier and an alist representing rate limit for the API.")

(defvar twittering-cookie-alist nil
  "Alist for stroing cookies for each account.
This variable stores an alist.
A key of the alist is a string that is a screen name of an account.
A value of the alist is a cookie alist which corresponds to a list of
a pair of a cookie name and value.")

(defcustom twittering-use-ssl t
  "*Use SSL connection if this variable is non-nil.

SSL connections use an external command as a backend."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-allow-insecure-server-cert nil
  "*If non-nil, `posting-station' allows insecure server certificates."
  :type 'boolean
  :group 'posting-station)

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
  :group 'posting-station)

(defcustom twittering-connection-type-order
  '(curl wget urllib-http native urllib-https)
  "*A list of connection methods in the preferred order."
  :type 'list
  :group 'posting-station)

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
  :group 'posting-station
  :type (twittering-connection-build-customize-option))

(defvar twittering-api-prefix "1/")
(defvar twittering-search-api-method "search")
(defvar twittering-web-path-prefix "")

;;;;
;;;; Functions for URL library
;;;;

(defcustom twittering-url-show-status nil
  "*If non-nil, show a running total of bytes transferred by urllib.

This has effect only if either \"urllib-httpp\" or \"urllib-https\" is used
as the connection method."
  :group 'posting-station
  :type 'boolean)

;;;;
;;;; Proxy setting / functions
;;;;

(defgroup twittering-proxy nil
  "Subgroup handling `posting-station' proxy setup."
  :group 'posting-station)

(defcustom twittering-proxy-use nil
  "*If non-nil, use PROXY.

See also `twittering-proxy-server' for documentation."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-proxy-server nil
  "*Proxy server for `posting-station'.

If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil."
  :group 'twittering-proxy
  :type '(choice (const nil) string))

(defcustom twittering-proxy-port nil
  "*Port number for `posting-station'.

If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 integer))

(defvar twittering-proxy-keep-alive nil)
(defcustom twittering-proxy-user nil
  "*Username for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defcustom twittering-proxy-password nil
  "*Password for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-http-proxy-server nil
  "*HTTP proxy server for `posting-station'.
If nil, it is initialized on entering `posting-station'.
The port number is specified by `twittering-http-proxy-port'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-http-proxy-port nil
  "*Port number of a HTTP proxy server for `posting-station'.
If nil, it is initialized on entering `posting-station'.
The server is specified by `twittering-http-proxy-server'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twittering-http-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twittering-proxy
  :type 'boolean)

(defcustom twittering-http-proxy-user nil
  "*Username for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-http-proxy-password nil
  "*Password for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-https-proxy-server nil
  "*HTTPS proxy server for `posting-station'.
If nil, it is initialized on entering `posting-station'.
The port number is specified by `twittering-https-proxy-port'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-https-proxy-port nil
  "*Port number of a HTTPS proxy server for `posting-station'.
If nil, it is initialized on entering `posting-station'.
The server is specified by `twittering-https-proxy-server'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twittering-https-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twittering-proxy
  :type 'boolean)

(defcustom twittering-https-proxy-user nil
  "*Username for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defcustom twittering-https-proxy-password nil
  "*Password for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twittering-proxy
  :type '(choice (const nil)
		 string))

(defun twittering-normalize-proxy-vars ()
  "Normalize the type of `twittering-http-proxy-port' and
`twittering-https-proxy-port'."
  (mapc (lambda (sym)
	  (let ((value (symbol-value sym)))
	    (cond
	     ((null value)
	      nil)
	     ((integerp value)
	      nil)
	     ((stringp value)
	      (set sym (string-to-number value)))
	     (t
	      (set sym nil)))))
	'(twittering-proxy-port
	  twittering-http-proxy-port
	  twittering-https-proxy-port)))

(defun twittering-proxy-info (scheme &optional item)
  "Return an alist for proxy configuration registered for SCHEME.
SCHEME must be a string \"http\", \"https\" or a symbol 'http or 'https.
The server name is a string and the port number is an integer."
  (twittering-normalize-proxy-vars)
  (let ((scheme (if (symbolp scheme)
		    (symbol-name scheme)
		  scheme))
	(info-list
	 `((("http" "https")
	    . ((server . ,twittering-proxy-server)
	       (port . ,twittering-proxy-port)
	       (keep-alive . ,twittering-proxy-keep-alive)
	       (user . ,twittering-proxy-user)
	       (password . ,twittering-proxy-password)))
	   (("http")
	    . ((server . ,twittering-http-proxy-server)
	       (port . ,twittering-http-proxy-port)
	       (keep-alive . ,twittering-http-proxy-keep-alive)
	       (user . ,twittering-http-proxy-user)
	       (password . ,twittering-http-proxy-password)))
	   (("https")
	    . ((server . ,twittering-https-proxy-server)
	       (port . ,twittering-https-proxy-port)
	       (keep-alive . ,twittering-https-proxy-keep-alive)
	       (user . ,twittering-https-proxy-user)
	       (password . ,twittering-https-proxy-password))))))
    (let ((info
	   (car (remove nil
			(mapcar
			 (lambda (entry)
			   (when (member scheme (car entry))
			     (let ((info (cdr entry)))
			       (when (and (cdr (assq 'server info))
					  (cdr (assq 'port info)))
				 info))))
			 info-list)))))
      (if item
	  (cdr (assq item info))
	info))))

(defun twittering-url-proxy-services ()
  "Return the current proxy configuration for `posting-station' in the format
of `url-proxy-services'."
  (remove nil (mapcar
	       (lambda (scheme)
		 (let ((server (twittering-proxy-info scheme 'server))
		       (port (twittering-proxy-info scheme 'port)))
		   (when (and server port)
		     `(,scheme . ,(format "%s:%s" server port)))))
	       '("http" "https"))))

(defun twittering-find-proxy (scheme)
  "Find proxy server and its port from the environmental variables and return
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
	   (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
	       (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
	  (let ((host (match-string 1 proxy))
		(port (string-to-number (match-string 2 proxy))))
	    (cons host port))
	nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
	   (env-proxy (or (getenv (upcase env-var))
			  (getenv (downcase env-var))))
	   (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
	       (string-match
		"^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
		env-proxy))
	  (let* ((host (match-string 2 env-proxy))
		 (port-str (or (match-string 4 env-proxy) default-port))
		 (port (string-to-number port-str)))
	    (cons host port))
	nil)))))

(defun twittering-setup-proxy ()
  (when (require 'url-methods nil t)
    ;; If `url-scheme-registry' is not initialized,
    ;; `url-proxy-services' will be reset by calling
    ;; `url-insert-file-contents' or `url-retrieve-synchronously', etc.
    ;; To avoid it, initialize `url-scheme-registry' by calling
    ;; `url-scheme-get-property' before calling such functions.
    (url-scheme-get-property "http" 'name)
    (url-scheme-get-property "https" 'name))
  (unless (and twittering-http-proxy-server
	       twittering-http-proxy-port)
    (let ((info (twittering-find-proxy "http")))
      (setq twittering-http-proxy-server (car-safe info))
      (setq twittering-http-proxy-port (cdr-safe info))))
  (unless (and twittering-https-proxy-server
	       twittering-https-proxy-port)
    (let ((info (twittering-find-proxy "https")))
      (setq twittering-https-proxy-server (car-safe info))
      (setq twittering-https-proxy-port (cdr-safe info))))
  (if (and twittering-proxy-use
	   (null (twittering-proxy-info "http"))
	   (null (twittering-proxy-info "https")))
      (progn
	(message "Disabling proxy due to lack of configuration.")
	(setq twittering-proxy-use nil))
    t))

(defun twittering-toggle-proxy ()
  (interactive)
  (setq twittering-proxy-use
	(not twittering-proxy-use))
  (if (twittering-setup-proxy)
      (message (if twittering-proxy-use "Use Proxy:on" "Use Proxy:off")))
  (twittering-update-mode-line))

;;;;
;;;; CA certificate
;;;;

(defvar twittering-cert-file nil
  "The full-path of the file including the certificates authorizing
servers on SSL.")

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

If it does not exist, create it. The directory includes root certificates
in \"hash format\". In detail, see verify(1SSL)."
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
  (format "Emacs/%d.%d posting-station/%s"
	  emacs-major-version emacs-minor-version
	  posting-station-version))

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

;;;;
;;;; Basic HTTP functions (general)
;;;;

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

;;;;
;;;; Basic HTTP functions with tls and Emacs builtins.
;;;;

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
;;;; Basic HTTP functions with curl
;;;;

(defun twittering-find-curl-program ()
  "Returns an appropriate `curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (memq system-type '(windows-nt cygwin)))
	    (curl.exe
	     (expand-file-name
	      "curl.exe"
	      (expand-file-name
	       "win-curl"
	       (file-name-directory (symbol-file 'twit))))))
	(and windows-p
	     (file-exists-p curl.exe) curl.exe))))

(defun twittering-start-http-session-curl-p ()
  "Return t if curl was installed, otherwise nil."
  (unless twittering-curl-program
    (setq twittering-curl-program (twittering-find-curl-program)))
  (not (null twittering-curl-program)))

(defun twittering-start-http-session-curl-https-p ()
  "Return t if curl was installed and the curl support HTTPS, otherwise nil."
  (when (twittering-start-http-session-curl-p)
    (unless twittering-curl-program-https-capability
      (with-temp-buffer
	(let ((coding-system-for-read 'iso-safe)
	      (coding-system-for-write 'iso-safe)
	      ;; Bind `default-directory' to the temporary directory
	      ;; because it is possible that the directory pointed by
	      ;; `default-directory' has been already removed.
	      (default-directory temporary-file-directory))
	  (call-process twittering-curl-program
			nil (current-buffer) nil
			"--version")
	  (goto-char (point-min))
	  (setq twittering-curl-program-https-capability
		(if (search-forward-regexp "^Protocols: .*https" nil t)
		    'capable
		  'incapable)))))
    (eq twittering-curl-program-https-capability 'capable)))

(defun twittering-start-http-session-curl-http2-p ()
  "Return t if the curl support HTTP2, otherwise nil."
  (when (twittering-start-http-session-curl-p)
    (unless twittering-curl-program-http2-capability
      (with-temp-buffer
	(let ((coding-system-for-read 'iso-safe)
	      (coding-system-for-write 'iso-safe)
	      ;; Bind `default-directory' to the temporary directory
	      ;; because it is possible that the directory pointed by
	      ;; `default-directory' has been already removed.
	      (default-directory temporary-file-directory))
	  (call-process twittering-curl-program
			nil (current-buffer) nil
			"--version")
	  (goto-char (point-min))
	  (setq twittering-curl-program-http2-capability
		(if (search-forward-regexp "^Features:.* HTTP2" nil t)
		    'capable
		  'incapable)))))
    (eq twittering-curl-program-http2-capability 'capable)))

(defun twittering-send-http-request-curl (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (use-http2 (twittering-start-http-session-curl-http2-p))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (header-list
	  `(,@header-list
	    ;; Make `curl' remove the HTTP header field "Expect" for
	    ;; avoiding '417 Expectation Failed' HTTP response error.
	    ;; The header field is automatically added for a HTTP request
	    ;; exceeding 1024 byte. See
	    ;; http://d.hatena.ne.jp/imait/20091228/1262004813 and
	    ;; http://www.escafrace.co.jp/blog/09/10/16/1008
	    ("Expect" . "")))
	 (curl-args
	  `("--include" "--silent" "--compressed"
	    ,@(when use-http2 `("--http2"))
	    ,@(apply 'append
		     (mapcar
		      (lambda (pair)
			;; Do not overwrite internal headers `curl' would use.
			;; Thanks to William Xu.
			;; "cURL - How To Use"
			;; http://curl.haxx.se/docs/manpage.html
			(unless (string= (car pair) "Host")
			  `("-H" ,(format "%s: %s" (car pair) (cdr pair)))))
		      header-list))
	    ,@(when use-ssl `("--cacert" ,cacert-file-body))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--insecure"))
	    ,@(when (and use-proxy proxy-server proxy-port)
		(append
		 `("-x" ,(format "%s:%s" proxy-server proxy-port))
		 (when (and proxy-user proxy-password)
		   `("-U" ,(format "%s:%s" proxy-user proxy-password)))))
	    ,@(when (string= "POST" method)
		`("-d" ,(or post-body "")))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `curl' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `curl' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `curl' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory)))
    (twittering-start-process-with-sentinel name buffer
					    twittering-curl-program
					    curl-args sentinel)))

(defun twittering-pre-process-buffer-curl (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(use-proxy (cdr (assq 'use-proxy connection-info))))
    (when (and use-ssl use-proxy)
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-min))
	  (let ((first-regexp
		 ;; successful HTTP response
		 "\\`HTTP/\\(1\\.[01]\\|2\\(\\.0\\)?\\) 2[0-9][0-9].*?\r?\n")
		(next-regexp
		 ;; following HTTP response
		 "^\\(\r?\n\\)HTTP/\\(1\\.[01]\\|2\\(\\.0\\)?\\) [0-9][0-9][0-9].*?\r?\n"))
	    (when (and (search-forward-regexp first-regexp nil t)
		       (search-forward-regexp next-regexp nil t))
	      (let ((beg (point-min))
		    (end (match-end 1)))
		(delete-region beg end)))))))))

;;;;
;;;; Basic HTTP functions with wget
;;;;

(defun twittering-find-wget-program ()
  "Returns an appropriate `wget' program pathname or nil if not found."
  (executable-find "wget"))

(defun twittering-start-http-session-wget-p ()
  "Return t if `wget' was installed, otherwise nil."
  (unless twittering-wget-program
    (setq twittering-wget-program (twittering-find-wget-program)))
  (not (null twittering-wget-program)))

(defun twittering-send-http-request-wget (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (args
	  `("--save-headers"
	    "--quiet"
	    "--output-document=-"
	    ,@(remove nil
		      (mapcar
		       (lambda (pair)
			 (unless (string= (car pair) "Host")
			   (format "--header=%s: %s" (car pair) (cdr pair))))
		       header-list))
	    ,@(when use-ssl
		`(,(format "--ca-certificate=%s" cacert-file-body)))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--no-check-certificate"))
	    ,@(cond
	       ((not use-proxy)
		'("--no-proxy"))
	       ((and use-proxy proxy-server proxy-port
		     proxy-user proxy-password)
		`(,(format "--proxy-user=%s" proxy-user)
		  ,(format "--proxy-password=%s" proxy-password)))
	       (t
		nil))
	    ,@(when (string= "POST" method)
		`(,(concat "--post-data=" (or post-body ""))))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `wget' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `wget' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `wget' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory))
	 (process-environment
	  `(,@(when (and use-proxy proxy-server proxy-port)
		`(,(format "%s_proxy=%s://%s:%s/" scheme
			   scheme proxy-server proxy-port)))
	    ,@process-environment)))
    (twittering-start-process-with-sentinel name buffer
					    twittering-wget-program args
					    sentinel)))

(defun twittering-pre-process-buffer-wget (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "\\`[^\n]*?\r\r\n" (point-max) t)
	;; When `wget.exe' writes HTTP response in text mode,
	;; CRLF may be converted into CRCRLF.
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" nil t)))
      (goto-char (point-max))
      (when (search-backward-regexp "\nProcess [^\n]* finished\n\\'"
				    (point-min) t)
	(replace-match "" nil t))
      )))

;;;;
;;;; Basic HTTP functions with url library
;;;;

(defun twittering-start-http-session-urllib-p ()
  "Return t if url library is available, otherwise nil."
  (require 'url nil t))

(defun twittering-start-http-session-urllib-https-p ()
  "Return t if url library can be used for HTTPS, otherwise nil."
  (and (not twittering-proxy-use)
       (require 'url nil t)
       (cond
	((<= 22 emacs-major-version)
	 ;; On Emacs22 and later, `url' requires `tls'.
	 (twittering-start-http-session-native-tls-p))
	((require 'ssl nil t)
	 ;; On Emacs21, `url' requires `ssl'.
	 t)
	((or (and (fboundp 'open-ssl-stream)
		  ;; Since `url-gw' (required by `url') defines autoload of
		  ;; `open-ssl-stream' from "ssl",
		  ;; (fboundp 'open-ssl-stream) will be non-nil even if
		  ;; "ssl" cannot be loaded and `open-ssl-stream' is
		  ;; unavailable.
		  ;; Here, the availability is confirmed by `documentation'.
		  (documentation 'open-ssl-stream))
	     ;; On Emacs21, `url' requires `ssl' in order to use
	     ;; `open-ssl-stream', which is included in `ssl.el'.
	     ;; Even if `ssl' cannot be loaded, `open-tls-stream' can be
	     ;; used as an alternative of the function.
	     (and (twittering-start-http-session-native-tls-p)
		  (defalias 'open-ssl-stream 'open-tls-stream)))
	 (provide 'ssl)
	 t)
	(t
	 nil))))

(defun twittering-send-http-request-urllib (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
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
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (url-proxy-services
	  (when use-proxy
	    `((,scheme . ,(format "%s:%s" proxy-server proxy-port)))))
	 (url-request-method method)
	 (url-request-extra-headers
	  ;; Remove some headers that should be configured by url library.
	  ;; They may break redirections by url library because
	  ;; `url-request-extra-headers' overwrites the new headers
	  ;; that are adapted to redirected connection.
	  (apply 'append
		 (mapcar (lambda (pair)
			   (if (member (car pair)
				       '("Host" "Content-Length"))
			       nil
			     `(,pair)))
			 (if proxy-credentials
			     (cons
			      `("Proxy-Authorization" ,proxy-credentials)
			      header-list)
			   header-list))))
	 (url-request-data post-body)
	 (url-show-status twittering-url-show-status)
	 (url-http-attempt-keepalives nil)
	 (tls-program twittering-tls-program)
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary))
    (lexical-let ((sentinel sentinel)
		  (buffer buffer))
      (let ((result-buffer
	     (url-retrieve
	      uri
	      (lambda (&rest args)
		(let ((proc url-http-process)
		      (url-buffer (current-buffer))
		      (status-str
		       (if (and (< emacs-major-version 22)
				(boundp 'url-http-end-of-headers)
				url-http-end-of-headers)
			   "urllib-finished"
			 "finished")))
		  ;; Callback may be called multiple times.
		  ;; (as filter and sentinel?)
		  (unless (local-variable-if-set-p 'twittering-retrieved)
		    (set (make-local-variable 'twittering-retrieved)
			 'not-completed)
		    (with-current-buffer buffer
		      (set-buffer-multibyte nil)
		      (insert-buffer-substring url-buffer))
		    (set-process-buffer proc buffer)
		    (unwind-protect
			(apply sentinel proc status-str nil)
		      (set-process-buffer proc url-buffer)
		      (if (eq twittering-retrieved 'exited)
			  (url-mark-buffer-as-dead url-buffer)
			(setq twittering-retrieved 'completed))))
		  (when (memq (process-status proc)
			      '(nil closed exit failed signal))
		    ;; Mark `url-buffer' as dead when the process exited
		    ;; and `sentinel' is completed.
		    ;; If this `lambda' is evaluated via a filter, the
		    ;; process may exit before it is finished to evaluate
		    ;; `(apply sentinel ...)'. In the case, `buffer' should
		    ;; not be killed. It should be killed after the
		    ;; evaluation of `sentinel'.
		    (if (eq twittering-retrieved 'completed)
			(url-mark-buffer-as-dead url-buffer)
		      (setq twittering-retrieved 'exited))))))))
	(when (buffer-live-p result-buffer)
	  (with-current-buffer result-buffer
	    (set (make-local-variable 'url-show-status)
		 twittering-url-show-status)
	    ;; Make `url-http-attempt-keepalives' buffer-local
	    ;; in order to send the current value of the variable
	    ;; to the sentinel invoked for HTTP redirection,
	    (make-local-variable 'url-http-attempt-keepalives))
	  (get-buffer-process result-buffer))))))

(defun twittering-pre-process-buffer-urllib (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (cond
       ((search-backward-regexp
	 "- Peer has closed the GNUTLS connection\r?\n\\'"
	 nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       ((search-backward-regexp "closed\r?\n\\'" nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       (t nil)))))

;;;;
;;;; HTTP functions for twitter-like serivce
;;;;

(defun twittering-http-application-headers (&optional method headers)
  "Return an assoc list of HTTP headers for posting-station."
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
;;;; OAuth
;;;;

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

;;;;
;;;; Asynchronous retrieval
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

(defun twittering-remove-redundant-queries (queue)
  (remove nil
	  (mapcar
	   (lambda (url)
	     (let ((current (gethash url twittering-url-data-hash)))
	       (when (or (null current)
			 (and (integerp current)
			      (< current twittering-url-request-retry-limit)))
		 url)))
	   (twittering-remove-duplicates queue))))

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

(defun twittering-extract-cookie (connection-info header-info)
  (remove
   nil
   (mapcar
    (lambda (entry)
      (let ((header-item (car entry))
	    (header-value (cdr entry)))
	(when (and (string= header-item "Set-Cookie")
		   (string-match "\\([^= ]*\\) *= *\\([^; ]*\\) *;? *"
				 header-value))
	  ;; For ease of implementation, the followings are assumed.
	  ;; 1. Each response header includes only one cookie.
	  ;; 2. `value' of cookie is a token, not a quoted string.
	  ;; 3. Attributes except `domain', `expires' and `path' are ignored.
	  (let* ((name (downcase (match-string 1 header-value)))
		 (value (match-string 2 header-value))
		 (attributes
		  (mapcar
		   (lambda (str)
		     (when (string-match "\\` *\\([^ ]*\\) *= *\\(.*\\)\\'"
					 str)
		       (let ((attr (downcase (match-string 1 str)))
			     (value (match-string 2 str)))
			 (cond
			  ((string= attr "domain")
			   `(domain . ,value))
			  ((string= attr "expires")
			   `(expires
			     . ,(apply 'encode-time
				       (parse-time-string
					(replace-regexp-in-string
					 "-" " " value)))))
			  ((string= attr "path")
			   `(path . ,value))
			  (t
			   nil)))))
		   (split-string (substring header-value (match-end 0))
				 " *; *")))
		 (additional-attributes
		  `(,@(let* ((domain (cdr (assq 'domain attributes)))
			     (request (cdr (assq 'request connection-info)))
			     (host (cdr (assq 'host request)))
			     (prefix
			      (if domain
				  (regexp-quote domain)
				(concat "\\`" (regexp-quote host)))))
			`((domain-regexp . ,(concat prefix "\\'")))))))
	    `(,name
	      (value . ,value)
	      ,@attributes
	      ,@additional-attributes)))))
    header-info)))

(defun twittering-make-cookie-string (request account-info)
  (let ((account
	 (twittering-get-from-account-info "screen_name" account-info))
	(current-time (current-time))
	(host (cdr (assq 'host request))))
    (when account
      (mapconcat
       'identity
       (remove nil
	       (mapcar
		(lambda (entry)
		  (let* ((expires (cdr (assq 'expires entry)))
			 (not-expired (or (null expires)
					  (time-less-p current-time expires)))
			 (domain-regexp (cdr (assq 'domain-regexp entry))))
		    (when (and not-expired
			       (string-match domain-regexp host))
		      (format "%s=%s" (car entry) (cdr (assq 'value entry))))))
		(cdr (assoc account twittering-cookie-alist))))
       ";"))))

;;;;
;;;; Abstract layer for Twitter API
;;;;

(defun twittering-api-path (&rest params)
  (mapconcat 'identity `(,twittering-api-prefix ,@params) ""))

(defun twittering-call-api (command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
Invoke `twittering-call-api-with-account' with the main account specified
by `twittering-get-main-account-info'.
For details of arguments, see `twittering-call-api-with-account'."
  (let ((account-info-alist (twittering-get-main-account-info)))
    (twittering-call-api-with-account account-info-alist command args-alist
				      additional-info)))

(defun twittering-call-api-with-account (account-info-alist command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
COMMAND is a symbol specifying API. ARGS-ALIST is an alist specifying
arguments for the API corresponding to COMMAND. Each key of ARGS-ALIST is a
symbol.
ACCOUNT-INFO-ALIST is an alist storing account information, which has
the following key;
\"screen_name\", \"oauth_token\" and \"oauth_token_secret\" for OAuth/xAuth,
\"screen_name\" and \"password\" for basic authentication.
ADDITIONAL-INFO is used as an argument ADDITIONAL-INFO of
`twittering-send-http-request'. Sentinels associated to the returned process
receives it as the fourth argument. See also the function
`twittering-send-http-request'.

The valid symbols as COMMAND follows:
retrieve-timeline -- Retrieve a timeline.
  Valid key symbols in ARGS-ALIST:
    timeline-spec -- the timeline spec to be retrieved.
    timeline-spec-string -- the string representation of the timeline spec.
    format -- (optional) the symbol specifying the format.
    number -- (optional) how many tweets are retrieved. It must be an integer.
      If nil, `twittering-number-of-tweets-on-retrieval' is used instead.
      The maximum for search timeline is 100, and that for other timelines is
      `twittering-max-number-of-tweets-on-retrieval'.
      If the given number exceeds the maximum, the maximum is used instead.
    max_id -- (optional) the maximum ID of retrieved tweets.
    since_id -- (optional) the minimum ID of retrieved tweets.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twittering-send-http-request' via `twittering-http-get'.
      If nil, `twittering-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
    page -- (optional and valid only for favorites timeline) which page will
      be retrieved.
retrieve-single-tweet -- Retrieve a single tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the tweet to be retrieved.
    username -- (optional) the screen name of the author of the tweet.
    format -- (optional) the symbol specifying the format.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twittering-send-http-request' via `twittering-http-get'.
      If nil, `twittering-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
get-list-index -- Retrieve list names owned by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
get-list-subscriptions -- Retrieve list names followed by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
create-friendships -- Follow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be followed.
destroy-friendships -- Unfollow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be unfollowed.
create-favorites -- Mark a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
destroy-favorites -- Remove a mark of a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
update-status -- Post a tweet.
  Valid key symbols in ARGS-ALIST:
    status -- the string to be posted.
    in-reply-to-status-id -- (optional) the ID of a status that this post is
      in reply to.
destroy-status -- Destroy a tweet posted by the authenticated user itself.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
retweet -- Retweet a tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
verify-credentials -- Verify the current credentials.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes returned information. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- the clean-up sentinel that post-processes the buffer
      associated to the process. This is used as an argument CLEAN-UP-SENTINEL
      of `twittering-send-http-request' via `twittering-http-get'.
send-direct-message -- Send a direct message.
  Valid key symbols in ARGS-ALIST:
    username -- the username who the message is sent to.
    status -- the sent message.
mute -- Mute a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be muted.
    username -- the username who will be muted.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
unmute -- Un-mute a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be un-muted.
    username -- the username who will be un-muted.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block -- Block a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block-and-report-as-spammer -- Block a user and report him or her as a spammer.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
get-service-configuration -- Get the configuration of the server.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request'."
  (let* ((additional-info
	  `(,@additional-info
	    (service-method . ,twittering-service-method))))
    (cond
     ((memq twittering-service-method '(twitter statusnet))
      (twittering-call-api-with-account-in-api1.0
       account-info-alist command args-alist additional-info))
     ((eq twittering-service-method 'twitter-api-v1.1)
      (cond
       ((not (require 'json nil t))
	(error "`json.el' is required to use the Twitter REST API v1.1")
	nil)
       ((not twittering-use-ssl)
	(error "SSL is required to use the Twitter REST API v1.1")
	nil)
       (t
	(twittering-call-api-with-account-in-api1.1
	 account-info-alist command args-alist additional-info))))
     (t
      (error "`twittering-service-method' is an invalid service method")
      ))))

(defun twittering-call-api-with-account-in-api1.0 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.0 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twittering-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twittering-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (page (cdr (assq 'page args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'favorites)
	      `(("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when page `(("page" . ,page)))))
	     ((eq spec-type 'retweeted_by_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("include_entities" . "true")
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     ((eq spec-type 'retweeted_to_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     (t
	      `(,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		,@(cond
		   ((eq spec-type 'search)
		    `(("include_entities" . "true")
		      ("q" . ,word)
		      ("rpp" . ,number-str)
		      ("with_twitter_user_id". "true")))
		   ((eq spec-type 'list)
		    (let ((username (elt spec 1))
			  (list-name (elt spec 2)))
		      `(("include_entities" . "true")
			("include_rts" . "true")
			("owner_screen_name" . ,username)
			("per_page" . ,number-str)
			("slug" . ,list-name))))
		   ((eq spec-type 'user)
		    (let ((username (elt spec 1)))
		      `(("count" . ,number-str)
			("include_entities" . "true")
			("include_rts" . "true")
			("screen_name" . ,username))))
		   ((memq spec-type '(friends mentions public))
		    `(("include_entities" . "true")
		      ("count" . ,number-str)
		      ("include_rts" . "true")))
		   (t
		    ;; direct_messages
		    ;; direct_messages_sent
		    ;; home
		    ;; replies
		    ;; retweeted_by_me
		    ;; retweeted_to_me
		    ;; retweets_of_me
		    `(("include_entities" . "true")
		      ("count" . ,number-str))))))))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       ((eq spec-type 'search)
		'atom)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (simple-spec-list
	    '((direct_messages . "direct_messages")
	      (direct_messages_sent . "direct_messages/sent")
	      (friends . "statuses/friends_timeline")
	      (home . "statuses/home_timeline")
	      (mentions . "statuses/mentions")
	      (public . "statuses/public_timeline")
	      (replies . "statuses/replies")
	      (retweeted_by_me . "statuses/retweeted_by_me")
	      (retweeted_to_me . "statuses/retweeted_to_me")
	      (retweets_of_me . "statuses/retweets_of_me")
	      (user . "statuses/user_timeline")))
	   (host (cond ((eq spec-type 'search) twittering-api-search-host)
		       (t twittering-api-host)))
	   (method
	    (cond
	     ((eq spec-type 'list)
	      (twittering-api-path "lists/statuses"))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		(if user
		    (twittering-api-path "favorites/" user)
		  (twittering-api-path "favorites"))))
	     ((eq spec-type 'retweeted_by_user)
	      (twittering-api-path "statuses/retweeted_by_user"))
	     ((eq spec-type 'retweeted_to_user)
	      (twittering-api-path "statuses/retweeted_to_user"))
	     ((eq spec-type 'search)
	      twittering-search-api-method)
	     ((assq spec-type simple-spec-list)
	      (twittering-api-path (cdr (assq spec-type simple-spec-list))))
	     (t nil)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format))))
      (cond
       ((eq spec-type 'single)
	(let ((id (cadr spec))
	      (sentinel (or sentinel
			    'twittering-retrieve-single-tweet-sentinel)))
	  (if (twittering-find-status id)
	      ;; If the status has already retrieved, do nothing.
	      nil
	    (twittering-call-api 'retrieve-single-tweet
				 `((id . ,id)
				   (format . ,format)
				   (sentinel . ,sentinel)
				   (clean-up-sentinel . ,clean-up-sentinel))
				 additional-info))))
       ((and host method)
	(twittering-http-get account-info-alist host method parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (parameters '(("include_entities" . "true")))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path "statuses/show/" id)
			   parameters format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path username "/lists")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path username "/lists/subscriptions")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "friendships/create")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "friendships/destroy")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "favorites/create/" id)
			    nil nil additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "favorites/destroy/" id)
			    nil nil additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (parameters
	    `(("status" . ,status)
	      ,@(when (eq twittering-auth-method 'basic)
		  '(("source" . "twmode")))
	      ,@(when id `(("in_reply_to_status_id" . ,id))))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/update")
			    parameters nil additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format)))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/destroy/" id)
			    nil format-str additional-info
			    'twittering-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/retweet/" id)
			    nil nil additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let ((sentinel (cdr (assq 'sentinel args-alist)))
	  (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path "account/verify_credentials")
			   nil nil additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let ((parameters
	   `(("screen_name" . ,(cdr (assq 'username args-alist)))
	     ("text" . ,(cdr (assq 'status args-alist))))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "direct_messages/new")
			    parameters nil additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "blocks/create")
			    parameters nil additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "report_spam")
			    parameters nil additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (request
	    (twittering-make-http-request-from-uri
	     "GET" nil
	     (concat (if twittering-use-ssl
			 "https"
		       "http")
		     "://" twittering-api-host
		     "/"
		     (twittering-api-path "help/configuration." format-str))))
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-send-http-request request additional-info
				    sentinel clean-up-sentinel)))
   (t
    nil)))

(defun twittering-call-api-with-account-in-api1.1 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.1 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((args-alist
	    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
		   (spec-type (car-safe spec))
		   (table '((friends . (home))
			    (replies . (mentions)))))
	      (cond
	       ((memq spec-type '(friends replies))
		(let* ((alternative (cdr (assq spec-type table)))
		       (alternative-str
			(twittering-timeline-spec-to-string alternative)))
		  (message
		   "Timeline spec %s is not supported in the Twitter REST API v1.1"
		   spec)
		  `((timeline-spec . ,alternative)
		    (timeline-spec-string . ,alternative-str)
		    ,@args-alist)))
	       (t
		args-alist))))
	   (spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twittering-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twittering-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'user)
	      (let ((username (elt spec 1)))
		`(,twittering-api-host
		  "1.1/statuses/user_timeline"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("tweet_mode" . "extended")
		  )))
	     ((eq spec-type 'list)
	      (let ((username (elt spec 1))
		    (list-name (elt spec 2)))
		`(,twittering-api-host
		  "1.1/lists/statuses"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("owner_screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("slug" . ,list-name)
		  ("tweet_mode" . "extended")
		  )))
	     ((eq spec-type 'direct_messages)
	      `(,twittering-api-host
		"1.1/direct_messages"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		("tweet_mode" . "extended")))
	     ((eq spec-type 'direct_messages_sent)
	      `(,twittering-api-host
		"1.1/direct_messages/sent"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		("tweet_mode" . "extended")
		))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		`(,twittering-api-host
		  "1.1/favorites/list"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ,@(when user `(("screen_name" . ,user)))
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("tweet_mode" . "extended")
		  )))
	     ((eq spec-type 'home)
	      `(,twittering-api-host
		"1.1/statuses/home_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		("tweet_mode" . "extended")
		))
	     ((eq spec-type 'mentions)
	      `(,twittering-api-host
		"1.1/statuses/mentions_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		("tweet_mode" . "extended")
		))
	     ((eq spec-type 'public)
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((memq spec-type '(retweeted_by_me
				retweeted_by_user
				retweeted_to_me
				retweeted_to_user))
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((eq spec-type 'retweets_of_me)
	      `(,twittering-api-host
		"1.1/statuses/retweets_of_me"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		("tweet_mode" . "extended")
		))
	     ((eq spec-type 'single)
	      (let ((id (elt spec 1)))
		`(,twittering-api-host
		  "1.1/statuses/show"
		  ("id" . ,id)
		  ("include_entities" . "true")
		  ("tweet_mode" . "extended")
		  )))
	     ((eq spec-type 'search)
	      (let ((word (elt spec 1)))
		`(,twittering-api-host
		  "1.1/search/tweets"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("q" . ,word)
		  ("result_type" . "recent")
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("tweet_mode" . "extended")
		  )))
	     (t
	      (error
	       "Timeline spec %s is unknown"
	       spec-string)
	      nil)))
	   (format 'json)
	   (format-str (symbol-name format))
	   (host (elt parameters 0))
	   (method (elt parameters 1))
	   (http-parameters (nthcdr 2 parameters))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format)))
	   ;; special treatment for single timeline.
	   (id (cdr (assoc "id" http-parameters)))
	   (sentinel (or sentinel
			 (when (eq spec-type 'single)
			   'twittering-retrieve-single-tweet-sentinel))))
      (cond
       ((null parameters)
	nil)
       ((not (and (string= format-str "json")
		  (require 'json nil t)))
	(error "The Twitter REST API v1.1 supports only JSON")
	nil)
       ((and (eq spec-type 'single)
	     (twittering-find-status id))
	;; If the status has already retrieved, do nothing.
	nil)
       ((and host method)
	(twittering-http-get account-info-alist host method http-parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")
	nil))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'json))))
	   (format-str (symbol-name format))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twittering-call-api-with-account-in-api1.1
       account-info-alist 'retrieve-timeline
       `((timeline-spec . (single ,id))
	 (format . ,format)
	 (sentinel . ,sentinel)
	 (clean-up-sentinel . ,clean-up-sentinel))
       additional-info)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host twittering-api-host)
	   (method "1.1/lists/list")
	   (http-parameters `(("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host twittering-api-host)
	   (method "1.1/lists/subscriptions")
	   (http-parameters
	    `(("count" . "20")
	      ("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let* ((username (cdr (assq 'username args-alist)))
	   (host twittering-api-host)
	   (method "1.1/friendships/create")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let* ((username (cdr (assq 'username args-alist)))
	   (host twittering-api-host)
	   (method "1.1/friendships/destroy")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host twittering-api-host)
	   (method "1.1/favorites/create")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host twittering-api-host)
	   (method "1.1/favorites/destroy")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (host twittering-api-host)
	   (method "1.1/statuses/update")
	   (http-parameters
	    `(("status" . ,status)
	      ,@(when id `(("in_reply_to_status_id" . ,id)))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host twittering-api-host)
	   (method (format "1.1/statuses/destroy/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info
			    'twittering-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host twittering-api-host)
	   (method (format "1.1/statuses/retweet/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let* ((host twittering-api-host)
	   (method "1.1/account/verify_credentials")
	   (http-parameters nil)
	   (format-str "json")
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let* ((host twittering-api-host)
	   (method "1.1/direct_messages/new")
	   (http-parameters
	    `(("screen_name" . ,(cdr (assq 'username args-alist)))
	      ("text" . ,(cdr (assq 'status args-alist)))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((memq command '(mute unmute))
    ;; Mute a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host twittering-api-host)
	   (method
	    (cdr (assq command '((mute . "1.1/mutes/users/create")
				 (unmute . "1.1/mutes/users/destroy")))))
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host twittering-api-host)
	   (method "1.1/blocks/create")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host twittering-api-host)
	   (method "1.1/users/report_spam")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((host twittering-api-host)
	   (method "1.1/help/configuration")
	   (http-parameters nil)
	   (format-str "json")
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   (t
    nil)))

(defun twittering-make-basic-authentication-string (account-info)
  (concat "Basic "
	  (base64-encode-string
	   (concat (cdr (assoc "screen_name" account-info))
		   ":" (cdr (assoc "password" account-info))))))

(defun twittering-make-oauth-authentication-string (account-info request)
  (let ((method (cdr (assq 'method request)))
	(access-token (cdr (assoc "oauth_token" account-info)))
	(access-token-secret (cdr (assoc "oauth_token_secret" account-info))))
    (unless (and (stringp access-token)
		 (stringp access-token-secret))
      (error "`account-info' has no valid OAuth token"))
    (twittering-oauth-auth-str-access
     method
     (cdr (assq 'uri-without-query request))
     (cdr (assq 'encoded-query-alist request))
     twittering-oauth-consumer-key twittering-oauth-consumer-secret
     access-token access-token-secret)))

(defun twittering-account-authorized-p ()
  (eq twittering-account-authorization 'authorized))
(defun twittering-account-authorization-queried-p ()
  (eq twittering-account-authorization 'queried))

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

(provide 'twittering)
;;; twittering.el ends here
