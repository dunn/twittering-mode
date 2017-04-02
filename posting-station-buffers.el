;;;;
;;;; Buffer info
;;;;

(defvar twittering-buffer-info-list nil
  "List of buffers managed by `posting-station'.")

(defun twittering-get-buffer-list ()
  "Return buffers managed by `posting-station'."
  (twittering-unregister-killed-buffer)
  twittering-buffer-info-list)

(defun twittering-get-active-buffer-list ()
  "Return active buffers managed by `posting-station', where statuses are
retrieved periodically."
  (twittering-unregister-killed-buffer)
  (remove nil
	  (mapcar (lambda (buffer)
		    (if (twittering-buffer-active-p buffer)
			buffer
		      nil))
		  twittering-buffer-info-list)))

(defun twittering-buffer-p (&optional buffer)
  "Return t if BUFFER is managed by `posting-station'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
	 (memq buffer twittering-buffer-info-list))))

(defun twittering-buffer-related-p ()
  "Return t if current buffer relates to `posting-station'."
  (or (twittering-buffer-p)
      (eq major-mode 'twittering-edit-mode)
      (string= (buffer-name (current-buffer))
	       twittering-debug-buffer)))

(defun twittering-buffer-active-p (&optional buffer)
  "Return t if BUFFER is an active buffer managed by `posting-station'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (twittering-buffer-p buffer)
	 (with-current-buffer buffer
	   twittering-active-mode))))

(defun twittering-get-buffer-from-spec (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC,
return nil."
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
	 (buffers
	  (remove
	   nil
	   (mapcar
	    (lambda (buffer)
	      (if (twittering-equal-string-as-timeline
		   spec-string
		   (twittering-get-timeline-spec-string-for-buffer buffer))
		  buffer
		nil))
	    (twittering-get-buffer-list)))))
    (if buffers
	;; We assume that the buffer with the same spec is unique.
	(car buffers)
      nil)))

(defun twittering-get-buffer-from-spec-string (spec-string)
  "Return the buffer bound to SPEC-STRING. If no buffers are bound to it,
return nil."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (and spec (twittering-get-buffer-from-spec spec))))

(defun twittering-get-timeline-spec-for-buffer (buffer)
  "Return the timeline spec bound to BUFFER. If BUFFER is not managed by
`posting-station', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec)))

(defun twittering-get-timeline-spec-string-for-buffer (buffer)
  "Return the timeline spec string bound to BUFFER. If BUFFER is not managed
by `posting-station', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec-string)))

(defun twittering-current-timeline-spec ()
  "Return the timeline spec bound to the current buffer. If it is not managed
by `posting-station', return nil."
  (twittering-get-timeline-spec-for-buffer (current-buffer)))

(defun twittering-current-timeline-spec-string ()
  "Return the timeline spec string bound to the current buffer. If it is not
managed by `posting-station', return nil."
  (twittering-get-timeline-spec-string-for-buffer (current-buffer)))

(defun twittering-unregister-buffer (buffer &optional keep-timer)
  "Unregister BUFFER from `twittering-buffer-info-list'.
If BUFFER is the last managed buffer and KEEP-TIMER is nil, call
`twittering-stop' to stop timers."
  (when (memq buffer twittering-buffer-info-list)
    (setq twittering-buffer-info-list
	  (delq buffer twittering-buffer-info-list))
    (when (and (null twittering-buffer-info-list)
	       (not keep-timer))
      (twittering-stop))))

(defun twittering-unregister-killed-buffer ()
  "Unregister buffers which has been killed."
  (mapc (lambda (buffer)
	  (unless (buffer-live-p buffer)
	    (twittering-unregister-buffer buffer)))
	twittering-buffer-info-list))

(defun twittering-replace-spec-string-for-buffer (buffer spec-string)
  "Replace the timeline spec string for BUFFER with SPEC-STRING when
BUFFER is managed by `posting-station' and SPEC-STRING is equivalent
to the current one."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-get-timeline-spec-string-for-buffer buffer)))
      (when (and (not (string= current spec-string))
		 (twittering-equal-string-as-timeline current spec-string))
	(with-current-buffer buffer
	  (rename-buffer spec-string t)
	  (setq twittering-timeline-spec-string spec-string))))))

(defun twittering-set-active-flag-for-buffer (buffer active)
  "Set ACTIVE to active-flag for BUFFER."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-buffer-active-p buffer)))
      (when (or (and active (not current))
		(and (not active) current))
	(twittering-toggle-activate-buffer buffer)))))

(defun twittering-toggle-activate-buffer (&optional buffer)
  "Toggle whether to retrieve timeline for the current buffer periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (with-current-buffer buffer
	(let* ((new-mode (not twittering-active-mode))
	       (active-buffer-list (twittering-get-active-buffer-list))
	       (start-timer (and new-mode (null active-buffer-list))))
	  (setq twittering-active-mode new-mode)
	  (when start-timer
	    (twittering-start))
	  (twittering-update-mode-line))))))

(defun twittering-activate-buffer (&optional buffer)
  "Activate BUFFER to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer t)))

(defun twittering-deactivate-buffer (&optional buffer)
  "Deactivate BUFFER not to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer nil)))

(defun twittering-kill-buffer (&optional buffer)
  "Kill BUFFER managed by `posting-station'."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (twittering-deactivate-buffer buffer)
      (kill-buffer buffer)
      (twittering-unregister-killed-buffer))))

(defun twittering-get-managed-buffer (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC, return
newly generated buffer.
SPEC may be a timeline spec or a timeline spec string."
  (let* ((original-spec spec)
	 (spec-string (if (stringp spec)
			  spec
			(twittering-timeline-spec-to-string spec)))
	 ;; `spec-string' without text properties is required because
	 ;; Emacs21 displays `spec-string' with its properties on mode-line.
	 ;; In addition, copying `spec-string' keeps timeline-data from
	 ;; being modified by `minibuf-isearch.el'.
	 (spec-string (copy-sequence spec-string))
	 (spec (if (stringp spec-string)
		   (twittering-string-to-timeline-spec spec-string)
		 nil)))
    (when (null spec)
      (error "\"%s\" is invalid as a timeline spec"
	     (or spec-string original-spec)))
    (set-text-properties 0 (length spec-string) nil spec-string)
    (twittering-add-timeline-history spec-string)
    (let ((buffer (twittering-get-buffer-from-spec spec)))
      (if buffer
	  (progn
	    (twittering-replace-spec-string-for-buffer buffer spec-string)
	    (twittering-update-mode-line)
	    buffer)
	(let ((buffer (generate-new-buffer spec-string))
	      (start-timer (null twittering-buffer-info-list)))
	  (add-to-list 'twittering-buffer-info-list buffer t)
	  (with-current-buffer buffer
	    (posting-station-setup spec-string)
	    (twittering-rerender-timeline-all buffer)
	    (when (twittering-account-authorized-p)
	      (when start-timer
		;; If `buffer' is the first managed buffer,
		;; call `twittering-start' to start timers.
		(twittering-start))
	      (unless (and start-timer twittering-active-mode)
		;; If `buffer' is active and the first managed buffer,
		;; `twittering-start' invokes
		;; `twittering-get-and-render-timeline' indirectly.
		;; Otherwise, `twittering-get-and-render-timeline' should be
		;; invoked here.
		(twittering-get-and-render-timeline))))
	  buffer)))))

;;;;
;;;; Map function for statuses on buffer
;;;;

(defun twittering-for-each-property-region (prop func &optional buffer interrupt)
  "Apply FUNC to each region, where property PROP is non-nil, on BUFFER.
If INTERRUPT is non-nil, the iteration is stopped if FUNC returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
	  (end-marker (make-marker)))
      (set-marker-insertion-type end-marker t)
      (while
	  (let ((value (get-text-property beg prop)))
	    (if value
		(let* ((end (next-single-property-change beg prop))
		       (end (or end (point-max)))
		       (end-marker (set-marker end-marker end))
		       (func-result (funcall func beg end value))
		       (end (marker-position end-marker)))
		  (when (or (null interrupt) func-result)
		    (if (get-text-property end prop)
			(setq beg end)
		      (setq beg (next-single-property-change end prop)))))
	      (setq beg (next-single-property-change beg prop)))))
      (set-marker end-marker nil))))

;;;;
;;;; Automatic redisplay of statuses on buffer
;;;;

(defun twittering-redisplay-status-on-buffer ()
  (mapc (lambda (buffer)
	  (unless (with-current-buffer buffer
		    (or (and (fboundp 'use-region-p) (use-region-p))
			(and transient-mark-mode mark-active)))
	    (twittering-redisplay-status-on-each-buffer buffer)))
	(twittering-get-buffer-list)))

(defun twittering-redisplay-status-on-each-buffer (buffer &optional prop)
  "Redisplay regions with the text property PROP on BUFFER."
  (let ((prop (or prop 'need-to-be-updated))
	(deactivate-mark deactivate-mark)
	(window-list (get-buffer-window-list buffer nil t))
	(marker (with-current-buffer buffer (point-marker)))
	(result nil))
    (with-current-buffer buffer
      (save-excursion
	(twittering-for-each-property-region
	 prop
	 (lambda (beg end value)
	   (let* ((func (car value))
		  (args (cdr value))
		  (current-str (buffer-substring beg end))
		  (updated-str (apply func beg end args))
		  (config (twittering-current-window-config window-list))
		  (buffer-read-only nil))
	     ;; Replace `current-str' if it differs to `updated-str' with
	     ;; ignoring properties. This is an ad-hoc solution.
	     ;; `current-str' is a part of the displayed status, but it has
	     ;; properties which are determined by the whole status.
	     ;; (For example, the `id' property.)
	     ;; Therefore, we cannot compare the strings with their
	     ;; properties.
	     (unless (string= current-str updated-str)
	       ;; If the region to be modified includes the current position,
	       ;; the point moves to the beginning of the region.
	       (when (and (< beg marker) (< marker end))
		 ;; This is required because the point moves to the center if
		 ;; the point becomes outside of the window by the effect of
		 ;; `set-window-start'.
		 (setq result beg))
	       (let ((common-properties
		      (twittering-get-common-properties beg)))
		 ;; Restore common properties.
		 (delete-region beg end)
		 (goto-char beg)
		 (insert (apply 'propertize updated-str common-properties)))
	       (twittering-restore-window-config-after-modification
		config beg end))))
	 buffer))
      (set-marker marker nil)
      (when (and result (eq (window-buffer) buffer))
	(let ((win (selected-window)))
	  (when (< result (window-start win))
	    (set-window-start win result))
	  (set-window-point win result))))))
