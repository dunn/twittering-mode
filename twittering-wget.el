;;; twittering-wget.el --- Functions for using wget with twittering-mode

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

;; Functions for using wget with twittering-mode

;;; Code:

(require 'twittering-http)

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

(provide 'twittering-wget)

;;; twittering-wget.el ends here
