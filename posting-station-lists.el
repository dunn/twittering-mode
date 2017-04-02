;;;;
;;;; List info retrieval
;;;;

(defun twittering-get-list-index (username)
  (twittering-call-api
   'get-list-index
   `((username . ,username)
     (sentinel . twittering-http-get-list-index-sentinel))))

(defun twittering-get-list-subscriptions (username)
  (twittering-call-api
   'get-list-subscriptions
   `((username . ,username)
     (sentinel . twittering-http-get-list-subscriptions-sentinel))))

(defun twittering-get-list-sync (username function)
  (setq twittering-list-index-retrieved nil)
  (let ((proc (funcall function username)))
    (when proc
      (twittering-wait-while nil 0.1
			     (and (not twittering-list-index-retrieved)
				  (twittering-process-alive-p proc)))
      (when (and (not twittering-list-index-retrieved)
		 (not (twittering-process-alive-p proc)))
	;; If the process has been dead, wait a moment because
	;; Emacs may be in the middle of evaluating the sentinel.
	(twittering-wait-while 10 0.1
			       (not twittering-list-index-retrieved)))))
  (cond
   ((null twittering-list-index-retrieved)
    (message "Failed to retrieve %s's lists." username)
    nil)
   ((stringp twittering-list-index-retrieved)
    (if (string= "" twittering-list-index-retrieved)
	(message "%s does not have a list." username)
      (message "%s" twittering-list-index-retrieved))
    nil)
   ((listp twittering-list-index-retrieved)
    twittering-list-index-retrieved)))

(defun twittering-get-list-index-sync (username)
  (twittering-get-list-sync username 'twittering-get-list-index))

(defun twittering-get-list-subscriptions-sync (username)
  (twittering-get-list-sync username 'twittering-get-list-subscriptions))
