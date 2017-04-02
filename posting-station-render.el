;;;;
;;;; Rendering
;;;;

(defun twittering-field-id< (field1 field2)
  (string< field1 field2))

(defun twittering-field-id= (field1 field2)
  (string= field1 field2))

(defun twittering-make-field-id-from-id (id &optional base-id)
  "Generate a field property for the tweet corresponding to ID.
Tweets are rendered in order of the field on `posting-station'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twittering-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((format-func (lambda (id) (format "%02d-%s" (length id) id))))
    (cond
     (base-id
      (format "O:%s:5:ancestor:%s"
	      (funcall format-func base-id)
	      (funcall format-func id)))
     (t
      (format "O:%s:8" (funcall format-func id))))))

(defun twittering-make-field-id (status &optional base-id)
  "Generate a field property for STATUS.
Tweets are rendered in order of the field on `posting-station'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twittering-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((id (cdr (assq 'id status))))
    (twittering-make-field-id-from-id id base-id)))

(defun twittering-make-properties-of-popped-ancestors (base-id)
  `(rendered-as ((ancestor-of . ,base-id))))

(defun twittering-make-field-id-of-timeline-oldest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be less than IDs made by `twittering-make-field-id' for any other
normal fields in the meaning of `twittering-field-id<'."
  (format "H:%s" spec-string))

(defun twittering-make-field-id-of-timeline-latest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be greater than IDs made by `twittering-make-field-id' for any other
normal fields in the meaning of `twittering-field-id<'."
  (format "U:%s" spec-string))

(defun twittering-field-id-is-timeline-oldest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the oldest end field.
Return non-nil if FIELD-ID is made by
`twittering-make-field-id-of-timeline-oldest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "H:")))

(defun twittering-field-id-is-timeline-latest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the latest end field.
Return non-nil if FIELD-ID is made by
`twittering-make-field-id-of-timeline-latest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "U:")))

(defun twittering-rendered-as-ancestor-status-p (&optional pos)
  "Return non-nil if the status at POS is rendered as an ancestor.
Ancestor statuses are rendered by `twittering-show-replied-statuses'."
  (let ((pos (or pos (point))))
    (assq 'ancestor-of (get-text-property pos 'rendered-as))))

(defun twittering-get-base-id-of-ancestor-at (&optional pos)
  "Return the base ID of a popped ancestor status rendered at POS.
If the status at POS is not a popped ancestor status or no status is
rendered at POS, return nil."
  (let ((pos (or pos (point))))
    (cdr (assq 'ancestor-of (get-text-property pos 'rendered-as)))))

(eval-and-compile
  (defsubst twittering-fill-string (str &optional adjustment prefix keep-newline)
    (when (and (not (boundp 'kinsoku-limit))
	       enable-kinsoku)
      ;; `kinsoku-limit' is defined on loading "international/kinsoku.el".
      ;; Without preloading, "kinsoku.el" will be loaded by auto-loading
      ;; triggered by `fill-region-as-paragraph'.
      ;; In that case, the local binding of `kinsoku-limit' conflicts the
      ;; definition by `defvar' in "kinsoku.el".
      ;; The below warning is displayed;
      ;; "Warning: defvar ignored because kinsoku-limit is let-bound".
      ;; So, we load "kinsoku.el" in advance if necessary.
      (load "international/kinsoku"))
    (let* ((kinsoku-limit 1)
	   (adjustment (+ (or adjustment 0)
			  (if enable-kinsoku
			      kinsoku-limit
			    0)))
	   (min-width
	    (apply 'min
		   (or
		    (mapcar 'window-width
			    (get-buffer-window-list (current-buffer) nil t))
		    ;; Use `(frame-width)' if no windows display
		    ;; the current buffer.
		    `(,(frame-width)))))
	   (temporary-fill-column (- (or twittering-fill-column (1- min-width))
				     adjustment)))
      (with-temp-buffer
	(let ((fill-column temporary-fill-column)
	      (fill-prefix (or prefix fill-prefix))
	      (adaptive-fill-regexp ""))
	  (if keep-newline
	      (let* ((hard-newline (propertize "\n" 'hard t))
		     (str (mapconcat 'identity (split-string str "\n")
				     (concat hard-newline fill-prefix))))
		(use-hard-newlines)
		(insert (concat prefix str))
		(fill-region (point-min) (point-max) nil t)
		(remove-text-properties (point-min) (point-max) '(hard nil)))
	    (insert (concat prefix str))
	    (fill-region-as-paragraph (point-min) (point-max)))
	  (buffer-substring (point-min) (point-max))))))

  (defsubst twittering-update-filled-string (beg end formater status prefix local-prefix &optional keep-newline)
    (let* ((str (twittering-fill-string (funcall formater status prefix)
					(length prefix) local-prefix
					keep-newline))
	   (next (next-single-property-change 0 'need-to-be-updated str)))
      (if (or (get-text-property 0 'need-to-be-updated str)
	      (and next (< next (length str))))
	  (put-text-property 0 (length str) 'need-to-be-updated
			     `(twittering-update-filled-string
			       ,formater ,status ,prefix ,local-prefix
			       ,keep-newline)
			     str)
	;; Remove the property required no longer.
	(remove-text-properties 0 (length str) '(need-to-be-updated nil) str))
      str))

  (defsubst twittering-make-passed-time-string
    (beg end encoded-created-at time-format &optional additional-properties)
    (let* ((now (current-time))
	   (secs (+ (* (- (car now) (car encoded-created-at)) 65536)
		    (- (cadr now) (cadr encoded-created-at))))
	   (time-string
	    (cond
	     ((< secs 5) "less than 5 seconds ago")
	     ((< secs 10) "less than 10 seconds ago")
	     ((< secs 20) "less than 20 seconds ago")
	     ((< secs 30) "half a minute ago")
	     ((< secs 60) "less than a minute ago")
	     ((< secs 150) "1 minute ago")
	     ((< secs 2400) (format "%d minutes ago"
				    (/ (+ secs 30) 60)))
	     ((< secs 5400) "about 1 hour ago")
	     ((< secs 84600) (format "about %d hours ago"
				     (/ (+ secs 1800) 3600)))
	     (t (format-time-string time-format encoded-created-at))))
	   (properties (append additional-properties
			       (and beg (text-properties-at beg))))
	   (time-string
	    ;; Copy a string and restore properties.
	    (apply 'propertize time-string properties)))
      (if (< secs 84600)
	  (put-text-property 0 (length time-string)
			     'need-to-be-updated
			     `(twittering-make-passed-time-string
			       ,encoded-created-at ,time-format)
			     time-string)
	;; Remove the property required no longer.
	(remove-text-properties 0 (length time-string)
				'(need-to-be-updated nil)
				time-string))
      time-string)))

(defmacro twittering-render-a-field (pos field-id generator &optional without-separator)
  "Render a field on the current buffer managed by `posting-station'.
Insert a field to the position pointed by FIELD-ID. The position is searched
after POS. The string for the field is generated by the GENERATOR expression.
This function does not render the status if a status with the same field ID
as FIELD-ID is already rendered.
Return non-nil if the status is rendered. Otherwise, return nil."
  `(lexical-let ((pos ,pos)
		 (field-id ,field-id))
     (while
	 (let ((buf-field-id (get-text-property pos 'field)))
	   (if (and buf-field-id
		    (if twittering-reverse-mode
			(twittering-field-id< buf-field-id field-id)
		      (twittering-field-id< field-id buf-field-id)))
	       (let ((next-pos
		      (twittering-get-next-status-head pos)))
		 (setq pos (or next-pos (point-max)))
		 next-pos)
	     nil)))
     (goto-char pos)
     (unless (twittering-field-id= field-id (get-text-property pos 'field))
       (let ((formatted-status (propertize ,generator 'field ,field-id))
	     (separator (if ,without-separator
			    ""
			  "\n")))
	 (if (eq pos (point-max))
	     ;; Use `insert' only if no statuses are rendered on the below.
	     (insert formatted-status separator)
	   ;; Use `insert-before-markers' in order to keep
	   ;; which status is pointed by each marker.
	   (insert-before-markers formatted-status separator))
	 t))))

(defun twittering-render-timeline (buffer timeline-data &optional invoke-hook keep-point)
  "Render statuses for BUFFER and return the list of the rendered statuses.
TIMELINE-DATA is a list of statuses being rendered.
If INVOKE-HOOK is non-nil and one or more tweets are rendered, run hooks
specified by `twittering-new-tweets-rendered-hook'.
If KEEP-POINT is nil and BUFFER is empty, this function moves cursor positions
to the latest status.

This function returns a list of the statuses newly rendered by the invocation."
  (with-current-buffer buffer
    (let* ((spec (twittering-get-timeline-spec-for-buffer buffer))
	   (referring-id-table
	    (twittering-current-timeline-referring-id-table spec))
	   (current-user (twittering-get-username))
	   (timeline-data
	    ;; Collect visible statuses.
	    (let ((prev-id nil))
	      (remove
	       nil
	       (mapcar
		(lambda (status)
		  (let ((id (cdr (assq 'id status)))
			(retweeted-id (cdr (assq 'retweeted-id status))))
		    (if (twittering-status-id= prev-id id)
			;; `status' is equivalent the previous one.
			nil
		      (setq prev-id id)
		      (cond
		       ((null retweeted-id)
			;; `status' is not a retweet.
			status)
		       ((and retweeted-id
			     (twittering-status-id=
			      id (gethash retweeted-id referring-id-table)))
			;; `status' is the first retweet.
			status)
		       ((and retweeted-id
			     (string= (cdr (assq 'retweeting-user-screen-name
						 status))
				      current-user))
			;; `status' is retweeted by the current account.
			status)
		       ((null (gethash retweeted-id referring-id-table))
			;; If the first ID referring the retweet is unknown,
			;; render it.
			;; This is necessary because a referring ID table
			;; of a composite timeline may lack information of
			;; some component timelines.
			status)
		       (t
			;; Otherwise, do not render it.
			nil)))))
		timeline-data))))
	   (timeline-data (if twittering-reverse-mode
			      (reverse timeline-data)
			    timeline-data))
	   (rendering-entire (null (twittering-get-first-status-head)))
	   (result-tweets nil)
	   (buffer-read-only nil))
      (twittering-update-status-format)
      (twittering-update-mode-line)
      (save-excursion
	(let ((pos (point-min))
	      (spec-string
	       (twittering-get-timeline-spec-string-for-buffer buffer)))
	  (cond
	   (rendering-entire
	    (let* ((latest-id
		    (twittering-make-field-id-of-timeline-latest-end
		     spec-string))
		   (oldest-id
		    (twittering-make-field-id-of-timeline-oldest-end
		     spec-string))
		   (footer-id
		    (if twittering-reverse-mode
			latest-id
		      oldest-id))
		   (header-id
		    (if twittering-reverse-mode
			oldest-id
		      latest-id)))
	      (setq
	       pos
	       (let ((footer
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twittering-timeline-footer-face))
		       (split-string (or twittering-timeline-footer "") "\n")
		       "\n"))
		     (header
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twittering-timeline-header-face))
		       (split-string (or twittering-timeline-header "") "\n")
		       "\n")))
		 (twittering-render-a-field (point-min) footer-id footer t)
		 (twittering-render-a-field (point-min) header-id header t)
		 (point)))))
	   (t
	    (setq pos (twittering-get-first-status-head))))
	  (goto-char pos)
	  (let* ((rendered-tweets
		  (remove nil
			  (mapcar
			   (lambda (status)
			     (when (twittering-render-a-field
				    (point)
				    (twittering-make-field-id status)
				    (twittering-format-status status))
			       (when twittering-default-show-replied-tweets
				 (twittering-show-replied-statuses
				  twittering-default-show-replied-tweets))
			       status))
			   timeline-data)))
		 (twittering-rendered-new-tweets
		  (if twittering-reverse-mode
		      (reverse rendered-tweets)
		    rendered-tweets))
		 (twittering-rendered-new-tweets-spec spec)
		 (twittering-rendered-new-tweets-spec-string spec-string))
	    (setq result-tweets rendered-tweets)
	    (when (and invoke-hook twittering-rendered-new-tweets)
	      (run-hooks 'twittering-new-tweets-rendered-hook)))))
      (debug-print (current-buffer))
      (cond
       ((and (not keep-point) rendering-entire)
	;; Go to the latest status of buffer after full insertion.
	(let ((dest (if twittering-reverse-mode
			(or (twittering-get-last-normal-field-head)
			    (twittering-get-last-status-head)
			    (point-max))
		      (or (twittering-get-first-normal-field-head)
			  (twittering-get-first-status-head)
			  (point-min))))
	      (window-list (get-buffer-window-list (current-buffer) nil t)))
	  (if window-list
	      (mapc
	       (lambda (window)
		 (set-window-point window dest)
		 (if twittering-reverse-mode
		     (twittering-set-window-end window (point-max))
		   (set-window-start window (point-min))))
	       window-list)
	    ;; Move the buffer position if the buffer is invisible.
	    (goto-char dest))))
       )
      result-tweets)
    ))

(defun twittering-rerender-timeline-all (buffer &optional restore-point)
  "Re-render statuses on BUFFER after clearing BUFFER.
If RESTORE-POINT is non-nil, positions on buffers bound to the same timeline
will be restored after rendering statuses."
  (with-current-buffer buffer
    (let* ((window-list (get-buffer-window-list (current-buffer) nil t))
	   (point-window-list
	    (mapcar (lambda (window)
		      (cons (window-point window) window))
		    window-list))
	   (original-pos (point)))
      (let ((buffer-read-only nil))
	(erase-buffer))
      (twittering-render-timeline
       (current-buffer) (twittering-current-timeline-data) nil restore-point)
      (when restore-point
	;; Restore points.
	(mapc (lambda (pair)
		(let* ((point (car pair))
		       (window (cdr pair))
		       (dest (max (point-max) point)))
		  (set-window-point window dest)))
	      point-window-list)
	(goto-char original-pos)))))

(defun twittering-retrieve-timeline (spec-string noninteractive api-arguments additional-info)
  "Retrieve and render a timeline specified by SPEC-STRING.
Retrieve a timeline specified by SPEC-STRING, which must be a timeline spec
string. Any timeline spec string including that for composite timeline can be
used as SPEC-STRING, though the primitive function `twittering-call-api'
accepts only a spec of a primary timeline.

NONINTERACTIVE is sent to the sentinel as a parameter `noninteractive' via
an argument `additional-info' of `twittering-call-api'.
API-ARGUMENTS is also sent to `twittering-call-api' as its argument
`args-alist'."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (cond
     ((not (twittering-account-authorized-p))
      ;; ignore any requests if the account has not been authorized.
      (message "No account for Twitter has been authorized.")
      t)
     ((and noninteractive (twittering-process-active-p spec))
      ;; ignore non-interactive request if a process is waiting for responses.
      t)
     ((twittering-timeline-spec-primary-p spec)
      (let* ((args
	      `(,@api-arguments
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)
		(format . ,(when (require 'json nil t)
			     'json))
		(clean-up-sentinel
		 . ,(lambda (proc status connection-info)
		      (when (memq status '(exit signal closed failed))
			(twittering-release-process proc))))))
	     (additional-info
	      `(,@additional-info
		(noninteractive . ,noninteractive)
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)))
	     (proc
	      (twittering-call-api 'retrieve-timeline args additional-info)))
	(when proc
	  (twittering-register-process proc spec spec-string)
	  (twittering-initialize-retrieval-count spec))))
     ((twittering-timeline-spec-composite-p spec)
      (mapc
       (lambda (spec)
	 (let* ((buffer (twittering-get-buffer-from-spec spec))
		(spec-string
		 (if buffer
		     (twittering-get-timeline-spec-string-for-buffer buffer)
		   (twittering-timeline-spec-to-string spec))))
	   (twittering-retrieve-timeline spec-string noninteractive
					 api-arguments additional-info)))
       (twittering-get-base-timeline-specs spec)))
     (t
      (let ((type (car spec)))
	(error "%s has not been supported yet" type))))))

(defun twittering-get-and-render-timeline (&optional noninteractive id spec spec-string)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (spec-string
	  (or spec-string (twittering-current-timeline-spec-string)))
	 (latest-status
	  ;; Assume that a list which was returned by
	  ;; `twittering-current-timeline-data' is sorted.
	  (car (twittering-current-timeline-data spec)))
	 (since_id (cdr-safe (assq 'id latest-status)))
	 (args `(,@(cond
		    (id `((max_id . ,id)))
		    (since_id `((since_id . ,since_id)))
		    (t nil)))))
    (twittering-retrieve-timeline spec-string noninteractive args nil)))

(defun twittering-render-a-status-with-delay (beg end id prefix)
  "Render a status with a delay.
It is assumed that this function is used as a property value that is
processed by the function `twittering-redisplay-status-on-each-buffer'."
  (let ((status (twittering-find-status id)))
    (when status
      (let ((properties (and beg (text-properties-at beg))))
	(apply 'propertize (twittering-format-status status prefix)
	       properties)))))

(defun twittering-toggle-or-retrieve-replied-statuses ()
  "Show/Hide all of replied statuses or retrieve a replied status.
If the cursor points to a reply or one of expanded replied statuses and
some of ancestor replied statuses have been already retrieved but they have
not been rendered, render them.
If the cursor points to a reply or one of expanded replied statuses and
all of retrieved ancestor statuses have been already rendered but the oldest
one of them is also a reply, retrieve the replied status.
If the cursor points to a reply or one of expanded replied statuses and
all of ancestor replied statuses have been already rendered, hide them by
`twittering-hide-replied-statuses'."
  (interactive)
  (let* ((pos (point))
	 (pos
	  ;; POS points to the head of the direct reply of the status being
	  ;; retrieved.
	  (cond
	   ((twittering-replied-statuses-visible-p pos)
	    ;; If some replied statuses are visible, find the edge.
	    (if twittering-reverse-mode
		(twittering-get-beginning-of-visible-replied-statuses pos)
	      (twittering-get-previous-status-head
	       (twittering-get-end-of-visible-replied-statuses pos))))
	   (t
	    (twittering-get-current-status-head pos))))
	 (id (twittering-get-id-at pos))
	 (status (twittering-find-status id))
	 (reply-id (cdr (assq 'in-reply-to-status-id status)))
	 (reply-username (cdr (assq 'in-reply-to-screen-name status)))
	 (base-id (or (twittering-get-base-id-of-ancestor-at pos)
		      id)))
    (cond
     ((twittering-find-status reply-id)
      ;; The status corresponding to REPLY-ID has been already retrieved
      ;; but it has not been rendered.
      ;;
      ;; `twittering-render-replied-statuses' additionally renders all
      ;; of already retrieved statuses.
      (twittering-render-replied-statuses))
     (reply-id
      (let* ((pos
	      ;; POS points to the position where the new field will be
	      ;; inserted.
	      (if twittering-reverse-mode
		  pos
		(or (twittering-get-next-status-head pos)
		    (point-max))))
	     (field-id (twittering-make-field-id-from-id reply-id base-id))
	     (prefix "  ")
	     (label "[RETRIEVING...]")
	     (symbol-for-redisplay 'waiting-for-retrieval)
	     (properties
	      `(,@(twittering-make-properties-of-popped-ancestors base-id)
		,symbol-for-redisplay
		(twittering-render-a-status-with-delay ,reply-id ,prefix)))
	     (str (apply 'propertize (concat prefix label) properties))
	     (buffer-read-only nil))
	(twittering-call-api
	 'retrieve-single-tweet
	 `((id . ,reply-id)
	   (username . ,reply-username)
	   (format . ,(when (require 'json nil t)
			'json))
	   (sentinel . twittering-retrieve-single-tweet-sentinel))
	 `((buffer . ,(current-buffer))
	   (property-to-be-redisplayed . ,symbol-for-redisplay)))
	(save-excursion
	  (goto-char pos)
	  (twittering-render-a-field (point) field-id str))
	(goto-char pos)))
     ((twittering-replied-statuses-visible-p)
      ;; All ancestor replied statuses have been rendered.
      (twittering-hide-replied-statuses))
     (t
      ;; The pointed status is not a reply.
      (message "This status is not a reply.")))))

(defun twittering-show-replied-statuses (&optional count interactive)
  (interactive)
  (cond
   ((twittering-replied-statuses-visible-p)
    (when interactive
      (message "The replied statuses were already showed.")))
   ((twittering-render-replied-statuses (point) count)
    t)
   (t
    ;; Failed to render replied statuses.
    (when interactive
      (let ((base-id (twittering-get-id-at)))
	(if (twittering-have-replied-statuses-p base-id)
	    (message "The status this replies to has not been fetched yet.")
	  (message "This status is not a reply.")))))))

(defun twittering-hide-replied-statuses (&optional interactive)
  (interactive)
  (cond
   ((twittering-replied-statuses-visible-p)
    (let* ((pos (twittering-get-current-status-head (point)))
	   (base-id (or (twittering-get-base-id-of-ancestor-at pos)
			(twittering-get-id-at pos)))
	   (pointing-to-base-status
	    (not (twittering-rendered-as-ancestor-status-p pos)))
	   (beg (twittering-get-beginning-of-visible-replied-statuses pos))
	   (end (twittering-get-end-of-visible-replied-statuses pos))
	   (buffer-read-only nil))
      (unless pointing-to-base-status
	(goto-char (if twittering-reverse-mode
		       beg
		     (or (twittering-get-previous-status-head beg)
			 (point-min)))))
      (delete-region beg end)))
   (interactive
    (message "The status this replies to was already hidden."))))

(defun twittering-toggle-show-replied-statuses ()
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (twittering-hide-replied-statuses (interactive-p))
    (twittering-show-replied-statuses twittering-show-replied-tweets
				      (interactive-p))))
