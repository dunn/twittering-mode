;;; twittering-urllib.el --- Functions for using urllib with twittering-mode

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

;; Functions for using urllib with twittering-mode

;;; Code:

(require 'twittering-http)

(defcustom twittering-url-show-status nil
  "*If non-nil, show a running total of bytes transferred by urllib.

This has effect only if either \"urllib-httpp\" or \"urllib-https\" is used
as the connection method."
  :group 'twittering-mode
  :type 'boolean)

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

(provide 'twittering-urllib)

;;; twittering-urllib.el ends here
