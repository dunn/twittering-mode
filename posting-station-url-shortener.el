;;;;
;;;; URI shortening
;;;;

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
