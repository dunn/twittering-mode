;;;;
;;;; Edit mode skeleton
;;;;

(defcustom twittering-edit-skeleton-footer ""
  "*String to be used as the footer in the edit skeleton."
  :group 'posting-station
  :type 'string)

(defvar twittering-edit-skeleton-footer-history nil)

(defcustom twittering-edit-skeleton-alist
  '((none . nil)
    (footer . ((nil _ twittering-edit-skeleton-footer)))
    (footer-only-normal
     . ((nil _ twittering-edit-skeleton-footer) . normal))
    (inherit-hashtags
     . [(twittering-edit-skeleton-inherit-hashtags . normal)
	(twittering-edit-skeleton-inherit-hashtags . reply)])
    (inherit-mentions
     . (twittering-edit-skeleton-inherit-mentions . reply))
    (inherit-any
     . [(twittering-edit-skeleton-inherit-mentions . reply)
	(twittering-edit-skeleton-inherit-hashtags . normal)
	(twittering-edit-skeleton-inherit-hashtags . reply)]))
  "*Alist of skeletons performed on `twittering-update-status-interactive'.
A key of the alist is a symbol and each value is nil, (SKELETON . PRED),
 (FUNC . PRED) or a vector of them.

When invoking `twittering-update-status-interactive', the value corresponding
to the key specified `twittering-edit-skeleton' are performed.

The value like (SKELETON . PRED) or (FUNC . PRED) is performed when the
current context matches with PRED.
PRED is nil, a symbol or a function.
If PRED is nil, the value is unconditionally performed.
If PRED is a symbol, the value is performed only when it equals to the
type of the tweet being edited. The type is one of 'direct-message, 'normal,
'organic-retweet and 'reply.
If PRED is a function, the value is performed only when the predicate
function PRED returns non-nil. PRED is invoked with three arguments
TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC.
TWEET-TYPE is a symbol, which is one of 'direct-message, 'normal,
'organic-retweet and 'reply, specifying which type of tweet will be edited.
If the tweet will be edited as a reply or an organic retweet, IN-REPLY-TO-ID
is a string specifying the replied tweet. Otherwise, IN-REPLY-TO-ID is nil.
CURRENT-SPEC specifies where the action of posting a tweet is performed.
If the action is performed on a posting-station buffer, CURRENT-SPEC is
a timeline spec string of the buffer.
If the action is performed on other buffers, CURRENT-SPEC is nil.
If the option IGNORE-CURRENT-SPEC for `twittering-update-status' is non-nil,
CURRENT-SPEC is also nil.

If PRED matches the current context, the value is performed as follows.
The value like (SKELETON . PRED) is performed by directly using SKELETON as an
argument of `skeleton-insert'.
The value like (FUNC . PRED) is performed by invoking FUNC with three
arguments, TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC as same as PRED.

If the value is a vector, each element is performed in order of elements
in the vector.

Note that the effective skeleton is invoked after inserting a
recipient."
  :group 'posting-station
  :type 'alist)

(defcustom twittering-edit-skeleton 'inherit-mentions
  "*A symbol specifying an effective skeleton.

The list of valid value is defined in `twittering-edit-skeleton-alist'.
To be valid, an entry should be added to `twittering-edit-skeleton-alist'
first.

When entering `twittering-edit-mode', the skeletons in the specified
entry in `twittering-edit-skeleton-alist' are performed."
  :group 'posting-station
  :type (if (> (length (mapcar #'car twittering-edit-skeleton-alist)) 0)
	    `(choice ,@(mapcar (lambda (entry) `(const ,(car entry)))
			       twittering-edit-skeleton-alist))
	  'symbol))

(defun twittering-switch-edit-skeleton ()
  (interactive)
  (let ((skeleton-keys
	 (mapcar (lambda (entry) (symbol-name (car entry)))
		 twittering-edit-skeleton-alist))
	(current (symbol-name (or twittering-edit-skeleton 'none))))
    (let ((selected
	   (twittering-completing-read
	    (format "Skeleton (%s): " current)
	    skeleton-keys nil t nil nil current)))
      (when selected
	(setq twittering-edit-skeleton (intern selected)))))
  (when (null twittering-edit-skeleton)
    (setq twittering-edit-skeleton 'none))
  (message "Current skeleton: %s" twittering-edit-skeleton))

(defun twittering-edit-skeleton-change-footer (&optional footer-str)
  (interactive)
  (let ((footer-str
	 (or footer-str
	     (read-from-minibuffer "Footer: " twittering-edit-skeleton-footer
				   nil nil
				   'twittering-edit-skeleton-footer-history))))
    (when footer-str
      (setq twittering-edit-skeleton-footer footer-str)))
  (message "Current footer: [%s]" twittering-edit-skeleton-footer))

(defun twittering-edit-skeleton-insert-base (&optional tweet-type in-reply-to-id current-spec)
  (let ((entry
	 (cdr (assq twittering-edit-skeleton twittering-edit-skeleton-alist))))
    (when entry
      (require 'skeleton)
      (let ((skeletons (if (vectorp entry)
			   entry
			 (list entry))))
	(mapcar (lambda (def)
		  (let ((skeleton-or-func (car def))
			(pred (cdr def)))
		    (when (or (null pred)
			      (and (functionp pred)
				   (funcall pred tweet-type in-reply-to-id))
			      (and (symbolp pred)
				   (eq pred tweet-type)))
		      (cond
		       ((functionp skeleton-or-func)
			(funcall skeleton-or-func tweet-type in-reply-to-id
				 current-spec))
		       (t
			(skeleton-insert skeleton-or-func))))))
		skeletons)))))

(defun twittering-edit-skeleton-insert (&optional tweet-type in-reply-to-id current-spec)
  (if (> 22 emacs-major-version)
      ;; This prevents Emacs21 from inserting skeletons before the cursor.
      (let ((current (point))
	    (pair (with-temp-buffer
		    (twittering-edit-skeleton-insert-base tweet-type
							  in-reply-to-id
							  current-spec)
		    `(,(buffer-string) . ,(point)))))
	(insert (car pair))
	(goto-char (+ -1 current (cdr pair))))
    (twittering-edit-skeleton-insert-base tweet-type in-reply-to-id
					  current-spec)))

(defun twittering-edit-skeleton-inherit-hashtags (tweet-type in-reply-to-id current-spec)
  (cond
   (in-reply-to-id
    (let* ((status (twittering-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (hashtags
	    (twittering-extract-matched-substring-all
	     (concat twittering-regexp-hash
		     "\\([[:alpha:]0-9_-]+\\)")
	     text))
	   (footer
	    (mapconcat (lambda (tag) (concat "#" tag))
		       hashtags " ")))
      (when hashtags
	(skeleton-insert `(nil _ " " ,footer)))))
   ((twittering-timeline-spec-is-search-p current-spec)
    (let* ((query-string
	    (twittering-extract-query-string-from-search-timeline-spec
	     current-spec))
	   (hashtag-list
	    (twittering-extract-matched-substring-all
	     (concat "\\(" twittering-regexp-hash "[[:alpha:]0-9_-]+\\)")
	     query-string)))
      (when hashtag-list
	(let ((footer (mapconcat 'identity hashtag-list " ")))
	  (skeleton-insert `(nil _ " " ,footer))))))))

(defun twittering-edit-skeleton-inherit-mentions (tweet-type in-reply-to-id current-spec)
  (when in-reply-to-id
    (let* ((status (twittering-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (recipient (cdr (assq 'user-screen-name status)))
	   (mentions
	    (twittering-extract-matched-substring-all
	     (concat twittering-regexp-atmark
		     "\\([a-zA-Z0-9_-]+\\)")
	     text))
	   (reduced-mentions
	    (remove nil
		    (mapcar
		     (lambda (mention)
		       (unless (or (string= mention recipient)
				   (string= mention (twittering-get-username)))
			 mention))
		     mentions))))
      (when reduced-mentions
	(let ((header (mapconcat (lambda (user) (concat "@" user))
				 reduced-mentions " ")))
	  (skeleton-insert `(nil ,header " " _)))))))

;;;;
;;;; Edit mode
;;;;

(defvar twittering-edit-buffer "*twittering-edit*")
(defvar twittering-pre-edit-window-configuration nil)
(defvar twittering-edit-history nil)
(defvar twittering-edit-local-history nil)
(defvar twittering-edit-local-history-idx nil)
(defvar twittering-warning-overlay nil)

(define-derived-mode twittering-edit-mode nil "twmode-status-edit"
  (use-local-map twittering-edit-mode-map)

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twittering-edit-mode))

  (make-local-variable 'twittering-warning-overlay)
  (setq twittering-warning-overlay (make-overlay 1 1 nil nil nil))
  (overlay-put twittering-warning-overlay 'face 'font-lock-warning-face)

  (make-local-variable 'twittering-edit-local-history)
  (setq twittering-edit-local-history (cons (buffer-string)
					    twittering-edit-history))
  (make-local-variable 'twittering-edit-local-history-idx)
  (setq twittering-edit-local-history-idx 0)

  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'twittering-edit-length-check)
  )

(when twittering-edit-mode-map
  (let ((km twittering-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'twittering-edit-post-status)
    (define-key km (kbd "C-c C-k") 'twittering-edit-cancel-status)
    (define-key km (kbd "C-c C-r") 'twittering-edit-toggle-reply)
    (define-key km (kbd "M-n") 'twittering-edit-next-history)
    (define-key km (kbd "M-p") 'twittering-edit-previous-history)
    (define-key km (kbd "<f4>") 'twittering-edit-replace-at-point)))

(defun twittering-effective-length (str &optional short-length-http short-length-https)
  "Return the effective length of STR with taking account of shortening URIs.

The returned length is calculated with taking account of shortening URIs
if `twittering-service-method' is the symbol `twitter' or `twitter-api-v1.1'.
It is assumed that a URI via HTTP will be converted into a URI consisting of
SHORT-LENGTH-HTTP characters.
It is assumed that a URI via HTTPS will be converted into a URI consisting of
SHORT-LENGTH-HTTPS characters.

If SHORT-LENGTH-HTTP is nil, the value of
 (twittering-get-service-configuration 'short_url_length) is used instead.
If SHORT-LENGTH-HTTPS is nil, the value of
 (twittering-get-service-configuration 'short_url_length_https) is used
instead."
  (cond
   ((memq twittering-service-method '(twitter twitter-api-v1.1))
    (let ((regexp "\\(?:^\\|[[:space:]]\\)\\(http\\(s\\)?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")
	  (short-length-http
	   (or short-length-http
	       (twittering-get-service-configuration 'short_url_length)))
	  (short-length-https
	   (or short-length-https
	       (twittering-get-service-configuration 'short_url_length_https)))
	  (rest str)
	  (pos 0)
	  (len 0))
      (save-match-data
	(while (string-match regexp str pos)
	  (let ((beg (match-beginning 1))
		(end (match-end 1))
		(short-len (if (match-beginning 2)
			       short-length-https
			     short-length-http)))
	    (let ((additional-length
		   ;; Ignore the original length to follow the change
		   ;; of t.co URL wrapper.
		   ;;
		   ;; https://dev.twitter.com/docs/tco-url-wrapper
		   ;; As of October 10, 2011 the t.co URL wrapper
		   ;; automatically wraps all links submitted to
		   ;; Twitter, regardless of length. This includes
		   ;; so-called URLs without protocols.
		   (+ (- beg pos) short-len)))
	      (setq len (+ len additional-length))
	      (setq pos end)))))
      (+ len (- (length str) pos))))
   (t
    (length str))))

(defun twittering-edit-length-check (&optional beg end len)
  (let* ((status (twittering-edit-extract-status))
	 (tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	 (maxlen (twittering-get-maximum-message-length tweet-type))
	 (length (twittering-effective-length status)))
    (setq mode-name
	  (format "twmode-status-edit[%d/%d]" length maxlen))
    (force-mode-line-update)
    (unless twittering-disable-overlay-on-too-long-string
      (if (< maxlen length)
	  (move-overlay twittering-warning-overlay
			(- (point-max) (- length maxlen)) (point-max))
	(move-overlay twittering-warning-overlay 1 1)))))

(defun twittering-edit-get-help-end ()
  "Return the end position of the help on `twittering-edit-mode'."
  (when (eq major-mode 'twittering-edit-mode)
    (next-single-property-change (point-min) 'read-only nil (point-max))))

(defun twittering-edit-extract-status ()
  "Return the text of the status being edited on `twittering-edit-mode'."
  (if (eq major-mode 'twittering-edit-mode)
      (buffer-substring-no-properties (twittering-edit-get-help-end)
				      (point-max))
    ""))

(defun twittering-edit-reset-status (str)
  "Reset the contents of the current `twittering-edit-mode' buffer with STR."
  (when (eq major-mode 'twittering-edit-mode)
    (let ((help-end (twittering-edit-get-help-end)))
      (delete-region help-end (point-max))
      (goto-char help-end)
      (insert str)
      (goto-char help-end))))

(defun twittering-edit-set-help-string (str)
  "Render STR as a help for `twittering-edit-mode' to the current buffer."
  (let* ((help-str (propertize str 'read-only t))
	 (len (length help-str)))
    (add-text-properties 0 1 '(front-sticky (read-only)) help-str)
    (add-text-properties (1- len) len '(rear-nonsticky t) help-str)
    (save-excursion
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t)
	    (help-end (twittering-edit-get-help-end)))
	(goto-char help-end)
	(if (= (point-min) help-end)
	    ;; When no helps are rendered, the all markers should be
	    ;; placed after the new help.
	    (insert help-str)
	  ;; Use `insert-before-markers' because the marker of the current
	  ;; position should follow the new help.
	  ;; Delete the old help after inserting the new help to make
	  ;; the new help visible if possible.
	  (insert-before-markers help-str)
	  (delete-region (point-min) help-end))))))

(defun twittering-edit-setup-help ()
  (let* ((direct-message-recipient
	  (cdr (assq 'direct-message-recipient twittering-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	 (cited-id (cdr (assq 'cited-id twittering-edit-mode-info)))
	 (item (cond
		((eq tweet-type 'direct-message)
		 (format "a direct message to %s" direct-message-recipient))
		((eq tweet-type 'reply)
		 "a reply")
		(t
		 "a tweet")))
	 (status-format
	  (cond
	   ((eq tweet-type 'direct-message)
	    (format "%%FILL{DIRECT MESSAGE to %s}\n" direct-message-recipient))
	   ((eq tweet-type 'reply)
	    "%FILL{REPLY to the tweet by %s at %C{%y/%m/%d %H:%M:%S};}\n%FILL{%FACE[font-lock-doc-face]{\"%T\"}}\n")
	   (t
	    nil)))
	 (func (when status-format
		 (twittering-generate-format-status-function status-format)))
	 (help-str
	  (apply 'concat
		 `(,@(when func
		       `(,(funcall func
				   (twittering-find-status cited-id)
				   nil)))
		   ,(propertize (format (substitute-command-keys "Keymap:
  \\[twittering-edit-post-status]: send %s
  \\[twittering-edit-cancel-status]: cancel %s
  \\[twittering-edit-toggle-reply]: toggle a normal tweet and a reply.
  \\[twittering-edit-next-history]: next history element
  \\[twittering-edit-previous-history]: previous history element
  \\[twittering-edit-replace-at-point]: shorten URL at point

---- text above this line is ignored ----
") item item)
				'face 'font-lock-comment-face)))))
    (twittering-edit-set-help-string help-str)))

(defun twittering-edit-close ()
  (kill-buffer (current-buffer))
  (when twittering-pre-edit-window-configuration
    (set-window-configuration twittering-pre-edit-window-configuration)
    (setq twittering-pre-edit-window-configuration nil)))

(defvar twittering-edit-mode-info nil
  "Alist of a tweet being edited.
Pairs of a key symbol and an associated value are following:
  direct-message-recipient -- the recipient when the edited message is
    sent as a direct message.
  cited-id -- the id of the tweet that the edited message refers to.
  tweet-type -- the type of the edited message, which is one of the
    following symbol; normal, reply or direct-message.")

(defun twittering-ensure-whole-of-status-is-visible (&optional window)
  "Ensure that the whole of the tweet on the current point is visible."
  (interactive)
  (let* ((window (or window (selected-window)))
	 (buffer (window-buffer window)))
    (when (twittering-buffer-p buffer)
      (with-current-buffer buffer
	(save-excursion
	  (let* ((next-head (or (twittering-get-next-status-head) (point-max)))
		 (current-tail (max (1- next-head) (point-min))))
	    (when (< (window-end window t) current-tail)
	      (twittering-set-window-end window current-tail))))))))

(defun twittering-update-status-from-pop-up-buffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (interactive)
  (let ((buf (generate-new-buffer twittering-edit-buffer)))
    (setq twittering-pre-edit-window-configuration
	  (current-window-configuration))
    (twittering-pop-to-buffer buf)
    (twittering-edit-mode)
    (make-local-variable 'twittering-edit-mode-info)
    (setq twittering-edit-mode-info
	  `((cited-id . ,reply-to-id)
	    (tweet-type . ,(cdr (assq tweet-type
				      '((direct-message . direct-message)
					(normal . normal)
					(organic-retweet . normal)
					(reply . reply)))))
	    (direct-message-recipient . ,username)))
    (twittering-edit-setup-help)
    (twittering-edit-length-check) ;; Update mode-line
    (setq buffer-undo-list nil)
    (goto-char (twittering-edit-get-help-end))
    (if (eq tweet-type 'direct-message)
	(message "C-c C-c to send, C-c C-k to cancel")
      (and (null init-string-or-skeleton)
	   twittering-current-hashtag
	   (setq init-string-or-skeleton
		 (format " #%s " twittering-current-hashtag)))
      (message "C-c C-c to post, C-c C-k to cancel"))
    (when init-string-or-skeleton
      (require 'skeleton)
      (cond
       ((stringp init-string-or-skeleton)
	(insert init-string-or-skeleton))
       ((listp init-string-or-skeleton)
	(skeleton-insert init-string-or-skeleton))))
    (twittering-edit-skeleton-insert tweet-type reply-to-id
				     current-spec)
    (set-buffer-modified-p nil)))

(defun twittering-edit-post-status ()
  (interactive)
  (let* ((status (twittering-edit-extract-status))
	 (cited-id (cdr (assq 'cited-id twittering-edit-mode-info)))
	 (cited-tweet (twittering-find-status cited-id))
	 (cited-username (cdr (assq 'user-screen-name cited-tweet)))
	 (direct-message-recipient
	  (cdr (assq 'direct-message-recipient twittering-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	 (max-length (twittering-get-maximum-message-length tweet-type)))
    (cond
     ((string-match "\\` *\\'" status)
      (message "Empty tweet!"))
     ((< max-length (twittering-effective-length status))
      (message "Tweet is too long!"))
     ((cond
       (twittering-request-confirmation-on-posting
	(y-or-n-p "Send this tweet? "))
       (t
	t))
      (setq twittering-edit-history
	    (cons status twittering-edit-history))
      (cond
       ((eq tweet-type 'direct-message)
	(if direct-message-recipient
	    (twittering-call-api 'send-direct-message
				 `((username . ,direct-message-recipient)
				   (status . ,status)))
	  (message "No username specified")))
       ((eq tweet-type 'reply)
	(twittering-call-api 'update-status
			     `((status . ,status)
			       (in-reply-to-status-id . ,cited-id))))
       (t
	(twittering-call-api 'update-status `((status . ,status)))))
      (twittering-edit-close))
     (t
      nil))))

(defun twittering-edit-cancel-status ()
  (interactive)
  (when (or (not (buffer-modified-p))
	    (prog1 (if (y-or-n-p "Cancel this tweet? ")
		       (message "Request canceled")
		     (message nil))))
    (twittering-edit-close)))

(defun twittering-edit-next-history ()
  (interactive)
  (if (>= 0 twittering-edit-local-history-idx)
      (message "End of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
				   twittering-edit-local-history)))
      (setcar current-history (twittering-edit-extract-status))
      (decf twittering-edit-local-history-idx)
      (twittering-edit-reset-status (nth twittering-edit-local-history-idx
					 twittering-edit-local-history)))))

(defun twittering-edit-previous-history ()
  (interactive)
  (if (>= twittering-edit-local-history-idx
	  (- (length twittering-edit-local-history) 1))
      (message "Beginning of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
				   twittering-edit-local-history)))
      (setcar current-history (twittering-edit-extract-status))
      (incf twittering-edit-local-history-idx)
      (twittering-edit-reset-status (nth twittering-edit-local-history-idx
					 twittering-edit-local-history)))))

(defun twittering-edit-replace-at-point ()
  (interactive)
  (when (eq major-mode 'twittering-edit-mode)
    (twittering-tinyurl-replace-at-point)
    (twittering-edit-length-check)))

(defun twittering-edit-toggle-reply ()
  "Toggle whether the tweet being edited will be sent as a reply or not."
  (interactive)
  (let ((tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	(cited-id (cdr (assq 'cited-id twittering-edit-mode-info))))
    (cond
     ((eq tweet-type 'direct-message)
      (message "The current message is a direct message."))
     ((null cited-id)
      (message "The current message does not have a reply target."))
     (t
      (setq twittering-edit-mode-info
	    (mapcar (lambda (entry)
		      (if (eq (car entry) 'tweet-type)
			  `(tweet-type
			    . ,(cdr (assq (cdr entry)
					  '((normal . reply)
					    (reply . normal)))))
			entry))
		    twittering-edit-mode-info))
      (twittering-edit-setup-help)))))

;;;;
;;;; Edit a status on minibuffer
;;;;

(defun twittering-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
	(deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
	   (status-len (- (twittering-effective-length (buffer-string))
			  (minibuffer-prompt-width)))
	   (mes (format "%d" status-len)))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ;; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twittering-setup-minibuffer ()
  (add-hook 'post-command-hook 'twittering-show-minibuffer-length t t))

(defun twittering-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twittering-show-minibuffer-length t))

(defun twittering-status-not-blank-p (status)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (with-temp-buffer
      (insert status)
      (goto-char (point-min))
      ;; skip user name
      (re-search-forward "\\`[[:space:]]*@[a-zA-Z0-9_-]+\\([[:space:]]+@[a-zA-Z0-9_-]+\\)*" nil t)
      (re-search-forward "[^[:space:]]" nil t))))

(defun twittering-update-status-from-minibuffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (and (not (eq tweet-type 'direct-message))
       (null init-string-or-skeleton)
       twittering-current-hashtag
       (setq init-string-or-skeleton
	     (format " #%s " twittering-current-hashtag)))
  (let ((status
	 (with-temp-buffer
	   (when init-string-or-skeleton
	     (require 'skeleton)
	     (cond
	      ((stringp init-string-or-skeleton)
	       (insert init-string-or-skeleton))
	      ((listp init-string-or-skeleton)
	       (skeleton-insert init-string-or-skeleton))))
	   (twittering-edit-skeleton-insert tweet-type reply-to-id
					    current-spec)
	   `(,(buffer-string) . ,(point))))
	(not-posted-p t)
	(prompt "status: ")
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'twittering-tinyurl-replace-at-point)
    (when twittering-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer t))
    (unwind-protect
	(while not-posted-p
	  (setq status (read-from-minibuffer prompt status map nil 'twittering-tweet-history nil t))
	  (let ((status status)
		(max-length
		 (twittering-get-maximum-message-length tweet-type)))
	    (if (< max-length (twittering-effective-length status))
		(setq prompt "status (too long): ")
	      (setq prompt "status: ")
	      (when (twittering-status-not-blank-p status)
		(cond
		 ((eq tweet-type 'direct-message)
		  (if username
		      (twittering-call-api 'send-direct-message
					   `((username . ,username)
					     (status . ,status)))
		    (message "No username specified")))
		 (t
		  (let ((parameters `(("status" . ,status)))
			(as-reply
			 (and reply-to-id
			      username
			      (eq tweet-type 'reply)
			      (string-match
			       (concat "@" username "\\(?:[\n\r \t]+\\)*")
			       status))))
		    ;; Add in_reply_to_status_id only when a posting
		    ;; status begins with @username.
		    (twittering-call-api
		     'update-status
		     `((status . ,status)
		       ,@(when as-reply
			   `((in-reply-to-status-id
			      . ,(format "%s" reply-to-id))))))
		    )))
		(setq not-posted-p nil))
	      )))
      ;; unwindforms
      (when (memq 'twittering-setup-minibuffer minibuffer-setup-hook)
	(remove-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer))
      (when (memq 'twittering-finish-minibuffer minibuffer-exit-hook)
	(remove-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer))
      )))
