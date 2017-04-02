;;;;
;;;; Account authorization
;;;;

(defun twittering-register-account-info (account-info)
  (setq twittering-oauth-access-token-alist account-info))

(defun twittering-get-main-account-info ()
  (cond
   ((eq twittering-auth-method 'basic)
    `(("screen_name" . ,twittering-username)
      ("password" . ,twittering-password)))
   ((memq twittering-auth-method '(oauth xauth))
    twittering-oauth-access-token-alist)))

(defun twittering-get-from-account-info (param account-info)
  (cdr (assoc param account-info)))

(defun twittering-get-username ()
  (let ((account-info (twittering-get-main-account-info)))
    (twittering-get-from-account-info "screen_name" account-info)))

(defun twittering-get-password ()
  (let ((account-info (twittering-get-main-account-info)))
    (twittering-get-from-account-info "password" account-info)))

(defun twittering-prepare-account-info ()
  "Return a pair of username and password.
If `twittering-username' is nil, read it from the minibuffer.
If `twittering-password' is nil, read it from the minibuffer."
  (let* ((username (or twittering-username
		       (read-string "your twitter username: ")))
	 (password (or twittering-password
		       (read-passwd (format "%s's twitter password: "
					    username)))))
    `(,username . ,password)))

(defun twittering-verify-credentials ()
  "Verify the account.

This function is an internal function, which should be called from
`twittering-ensure-account-verification'.

If the account has been authorized already, return t.
Otherwise, this function tries to authorize the account.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twittering-account-authorized-p)
    ;; The account has been authorized already.
    t)
   ((not (twittering-account-authorization-queried-p))
    ;; This function must be invoked from
    ;; `twittering-ensure-account-verification', which updates the variable
    ;; `twittering-account-authorization' into the symbol `queried'.
    (error "`twittering-verify-credentials' is invoked multiple times.")
    nil)
   ((and (memq twittering-auth-method '(oauth xauth))
	 (or (null twittering-oauth-consumer-key)
	     (null twittering-oauth-consumer-secret)))
    (message "Consumer for OAuth is not specified.")
    nil)
   ((twittering-has-oauth-access-token-p)
    (let* ((username (cdr (assoc "screen_name"
				 (twittering-get-main-account-info))))
	   (proc
	    (twittering-call-api-with-account
	     (twittering-get-main-account-info)
	     'verify-credentials
	     `((sentinel
		. twittering-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twittering-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed." username)
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twittering-wait-while nil 0.1
			       (and
				(twittering-account-authorization-queried-p)
				(twittering-process-alive-p proc)))
	(if (not (twittering-account-authorization-queried-p))
	    ;; The query is completed.
	    (twittering-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twittering-wait-while
	   10 0.1
	   (twittering-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   ((eq twittering-auth-method 'oauth)
    (let* ((scheme (if twittering-oauth-use-ssl
		       "https"
		     "http"))
	   (request-token-url
	    (concat scheme twittering-oauth-request-token-url-without-scheme))
	   (access-token-url
	    (concat scheme twittering-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twittering-oauth-get-access-token
	     request-token-url
	     (lambda (token)
	       (concat scheme
		       twittering-oauth-authorization-url-base-without-scheme
		       token))
	     access-token-url
	     twittering-oauth-consumer-key twittering-oauth-consumer-secret
	     "posting-station")))
      (cond
       ((and (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist)
	     (assoc "screen_name" token-alist))
	(let ((username (cdr (assoc "screen_name" token-alist))))
	  (twittering-register-account-info token-alist)
	  (message "Authorization for the account \"%s\" succeeded."
		   username)
	  (when (and twittering-use-master-password
		     (twittering-capable-of-encryption-p)
		     (not (file-exists-p twittering-private-info-file)))
	    (twittering-save-private-info-with-guide))
	  ;; Succeeded in authorizing the account.
	  t))
       (t
	;; There is no global account info that should be invalidated.
	;; Failed to authorize the account.
	(message "Authorization via OAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twittering-auth-method 'xauth)
    (let* ((account-info (twittering-prepare-account-info))
	   (scheme (if twittering-oauth-use-ssl
		       "https"
		     "http"))
	   (access-token-url
	    (concat scheme twittering-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twittering-xauth-get-access-token
	     access-token-url
	     twittering-oauth-consumer-key twittering-oauth-consumer-secret
	     (car account-info)
	     (cdr account-info))))
      ;; Dispose of password as recommended by Twitter.
      ;; http://dev.twitter.com/pages/xauth
      (setcdr account-info nil)
      (cond
       ((and token-alist
	     (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist))
	(twittering-register-account-info token-alist)
	(message "Authorization for the account \"%s\" succeeded."
		 (twittering-get-username))
	(when (and twittering-use-master-password
		   (twittering-capable-of-encryption-p)
		   (not (file-exists-p twittering-private-info-file)))
	  (twittering-save-private-info-with-guide))
	;; Succeeded in authorizing the account.
	t)
       (t
	;; Failed to authorize the account.
	(message "Authorization via xAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twittering-auth-method 'basic)
    (let* ((account-info
	    (let ((pair (twittering-prepare-account-info)))
	      `(("screen_name" . ,(car pair))
		("password" . ,(cdr pair)))))
	   ;; Bind account information locally to ensure that
	   ;; the variables are reset when the verification fails.
	   (twittering-username (car account-info))
	   (twittering-password (cdr account-info))
	   (proc
	    (twittering-call-api-with-account
	     account-info
	     'verify-credentials
	     `((sentinel . twittering-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twittering-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed."
		 (twittering-get-from-account-info "screen_name" account-info))
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twittering-wait-while nil 0.1
			       (and
				(twittering-account-authorization-queried-p)
				(twittering-process-alive-p proc)))
	(if (not (twittering-account-authorization-queried-p))
	    ;; The query is finished.
	    (twittering-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twittering-wait-while
	   10 0.1
	   (twittering-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   (t
    (message "%s is invalid as an authorization method."
	     twittering-auth-method)
    nil)))

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

(defun twittering-ensure-account-verification ()
  "Ensure verification of an account.

If an account has been already authorized, return t.
If a query of authorization is being processed, return nil.

Otherwise, this function tries to authorize an account by calling
`twittering-verify-credentials'.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twittering-account-authorized-p)
    ;; The account has been already authorized.
    t)
   ((twittering-account-authorization-queried-p)
    ;; The account has not been authorized yet.
    nil)
   (t
    (setq twittering-account-authorization 'queried)
    (let ((result nil))
      (unwind-protect
	  (setq result (twittering-verify-credentials))
	(if result
	    (setq twittering-account-authorization 'authorized)
	  (setq twittering-account-authorization nil)))
      result))))

;;;;
;;;; Preparation for invoking APIs
;;;;

(defun twittering-api-invocation-is-ready-p ()
  "Return non-nil if the preparation for invoking APIs has been completed."
  (and
   ;; The global variables are initialized.
   twittering-initialized
   ;; A connection method is prepared.
   (let ((use-ssl (or twittering-use-ssl twittering-oauth-use-ssl)))
     (twittering-lookup-connection-type use-ssl))
   ;; The account has been already authorized.
   (twittering-account-authorized-p)))

(defun twittering-ensure-preparation-for-api-invocation ()
  "Ensure prerequisites for invoking APIs. Return non-nil in success.
If prerequisites has been already satisifed, just return non-nil.
If prerequisites are not satisfied, this function try to satisfy them.
Then, return non-nil if they has been satisfied and return nil otherwise."
  (twittering-initialize-global-variables-if-necessary)
  (and (twittering-ensure-connection-method)
       (twittering-ensure-private-info)
       (twittering-ensure-account-verification)))
