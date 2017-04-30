;;; twittering-curl.el --- Functions for using curl with twittering-mode

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

;; Functions for using curl with twittering-mode

;;; Code:

(require 'twittering-http)

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

(provide 'twittering-curl)

;;; twittering-curl.el ends here
