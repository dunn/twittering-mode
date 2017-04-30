;;; twittering-proxy.el --- Proxying in twittering-mode

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

;; Proxying in twittering-mode

;;; Code:

(defgroup twittering-proxy nil
  "Subgroup handling `twittering-mode' proxy setup."
  :group 'twittering-mode)

(defcustom twittering-proxy-use nil
  "*If non-nil, use PROXY.

See also `twittering-proxy-server' for documentation."
  :type 'boolean
  :group 'twittering-mode)

(defcustom twittering-proxy-server nil
  "*Proxy server for `twittering-mode'.

If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil."
  :group 'twittering-proxy
  :type '(choice (const nil) string))

(defcustom twittering-proxy-port nil
  "*Port number for `twittering-mode'.

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
  "*HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
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
  "*Port number of a HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
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
  "*HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
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
  "*Port number of a HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
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
  "Return the current proxy configuration for `twittering-mode' in the format
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

(provide 'twittering-proxy)

;;; twittering-proxy.el ends here
