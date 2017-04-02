;;;;
;;;; Icon mode
;;;;

(defvar twittering-icon-mode nil
  "You MUST NOT CHANGE this variable directly.
You should change through function `twittering-icon-mode'.")

(defun twittering-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive "P")
  (let ((prev-mode twittering-icon-mode))
    (setq twittering-icon-mode
	  (if (null arg)
	      (not twittering-icon-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-icon-mode)
      (twittering-update-mode-line)
      (twittering-rerender-timeline-all (current-buffer) t))))

(defvar twittering-icon-prop-hash (make-hash-table :test 'equal)
  "Hash table for storing display properties of icon. The key is the size of
icon and the value is a hash. The key of the child hash is URL and its value
is the display property for the icon.")

(defcustom twittering-convert-program (executable-find "convert")
  "*A path of the command which is invoked for image conversion.

The default is determined by searching \"convert\" in `exec-path'.
The command must be compatible with \"convert\" of ImageMagick."
  :group 'posting-station
  :type 'file)

(defcustom twittering-convert-fix-size 48
  "*Size of an icon image.

If nil, an icon image is displayed as is."
  :group 'posting-station
  :type '(choice (const nil)
		 integer))

(defcustom twittering-use-convert (not (null twittering-convert-program))
  "*If non-nil, use \"convert\" for converting or resizing icon images."
  :group 'posting-station
  :type 'boolean)

(defcustom twittering-fallback-image-format 'xpm
  "*Fallback format used for displaying an image without a supproted format.
Images which Emacs does not supports are converted into the fallback image
format."
  :group 'posting-station
  :type 'symbol)

(defcustom twittering-use-profile-image-api nil
  "*Whether to use `profile_image' API for retrieving scaled icon images.
NOTE: This API is rate limited and is obsolete in the Twitter REST API v1.1."
  :group 'posting-station
  :type 'boolean)

(defcustom twittering-icon-storage-file
  (expand-file-name "~/.posting-station-icons.gz")
  "*The file to which icon images are stored.
`twittering-icon-storage-limit' determines the number icons stored in the
file.
The file is loaded with `with-auto-compression-mode'."
  :group 'posting-station
  :type 'file)

(defcustom twittering-use-icon-storage nil
  "*Whether to use the persistent icon storage.
If this variable is non-nil, icon images are stored to the file specified
by `twittering-icon-storage-file'."
  :group 'posting-station
  :type 'boolean)

(defvar twittering-icon-storage-recent-icons nil
  "List of recently rendered icons.")

(defcustom twittering-icon-storage-limit 500
  "*How many icons are stored in the persistent storage.
If `twittering-use-icon-storage' is nil, this variable is ignored.
If a positive integer N, `twittering-save-icon-properties' saves N icons that
have been recently rendered.
If nil, the function saves all icons."
  :group 'posting-station
  :type '(choice (const nil)
		 integer))

(defconst twittering-error-icon-data-pair
  '(xpm . "/* XPM */
static char * yellow3_xpm[] = {
\"16 16 2 1\",
\" 	c None\",
\".	c #FF0000\",
\"................\",
\".              .\",
\". .          . .\",
\".  .        .  .\",
\".   .      .   .\",
\".    .    .    .\",
\".     .  .     .\",
\".      ..      .\",
\".      ..      .\",
\".     .  .     .\",
\".    .    .    .\",
\".   .      .   .\",
\".  .        .  .\",
\". .          . .\",
\".              .\",
\"................\"};
")
  "Image used when the valid icon cannot be retrieved.")

(defun twittering-update-icon-storage-recent-icons (size image-url spec)
  (unless (null twittering-icon-storage-limit)
    (let ((dummy-icon-properties (twittering-make-display-spec-for-icon
				  twittering-error-icon-data-pair)))
      (unless (equal spec dummy-icon-properties)
	(let ((history-delete-duplicates t))
	  (twittering-add-to-history 'twittering-icon-storage-recent-icons
				     (list size image-url)
				     twittering-icon-storage-limit))))))

(defun twittering-get-display-spec-for-icon (image-url)
  (let ((hash
	 (gethash twittering-convert-fix-size twittering-icon-prop-hash)))
    (when hash
      (let ((spec (gethash image-url hash))
	    (size twittering-convert-fix-size))
	(when spec
	  (twittering-update-icon-storage-recent-icons size image-url spec)
	  spec)))))

(defun twittering-convert-image-data (image-data dest-type &optional src-type)
  "Convert IMAGE-DATA into XPM format and return it. If it fails to convert,
return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (require-final-newline nil)
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (insert image-data)
      (let* ((args
	      `(,@(when (<= emacs-major-version 22)
		    ;; Emacs22 and earlier raises "Color allocation error"
		    ;; on decoding a XPM image with opacity. To ignore
		    ;; opacity, the option "+matte" is added.
		    '("+matte"))
		,@(unless (fboundp 'create-animated-image)
		    '("-flatten"))
		,(if src-type (format "%s:-" src-type) "-")
		,@(when (integerp twittering-convert-fix-size)
		    `("-resize"
		      ,(format "%dx%d" twittering-convert-fix-size
			       twittering-convert-fix-size)))
		,(format "%s:-" dest-type)))
	     (exit-status
	      (apply 'call-process-region (point-min) (point-max)
		     twittering-convert-program t `(t nil) nil args)))
	(if (equal 0 exit-status)
	    (buffer-string)
	  ;; failed to convert the image.
	  nil)))))

(defun twittering-create-image-pair (image-data)
  "Return a pair of image type and image data.
IMAGE-DATA is converted by `convert' if the image type of IMAGE-DATA is not
available and `twittering-use-convert' is non-nil."
  (let* ((image-type (and image-data (image-type-from-data image-data)))
	 (image-pair `(,image-type . ,image-data))
	 (converted-size
	  `(,twittering-convert-fix-size . ,twittering-convert-fix-size)))
    (cond
     ((null image-data)
      twittering-error-icon-data-pair)
     ((and (image-type-available-p image-type)
	   (or (fboundp 'create-animated-image)
	       (not (and twittering-use-convert
			 (eq image-type 'gif))))
	   (or (not (integerp twittering-convert-fix-size))
	       (equal (image-size (create-image image-data image-type t) t)
		      converted-size)))
      image-pair)
     (twittering-use-convert
      (let ((converted-data
	     (twittering-convert-image-data image-data
					    twittering-fallback-image-format)))
	(if converted-data
	    `(,twittering-fallback-image-format . ,converted-data)
	  twittering-error-icon-data-pair)))
     (t
      twittering-error-icon-data-pair))))

(defun twittering-register-image-spec (image-url spec size)
  (let ((hash (gethash size twittering-icon-prop-hash)))
    (unless hash
      (setq hash (make-hash-table :test 'equal))
      (puthash size hash twittering-icon-prop-hash))
    (puthash image-url spec hash)))

(defun twittering-register-image-data (image-url image-data &optional size)
  (let ((image-pair (twittering-create-image-pair image-data))
	(size (or size twittering-convert-fix-size)))
    (when image-pair
      (let ((spec (twittering-make-display-spec-for-icon image-pair)))
	(twittering-register-image-spec image-url spec size)
	spec))))

(defun twittering-make-slice-spec (image-spec)
  "Return slice property for reducing the image size by cropping it."
  (let* ((size (image-size image-spec t))
	 (width (car size))
	 (height (cdr size))
	 (fixed-length twittering-convert-fix-size)
	 (half-fixed-length (/ fixed-length 2)))
    (if (or (< fixed-length width) (< fixed-length height))
	`(slice ,(max 0 (- (/ width 2) half-fixed-length))
		,(max 0 (- (/ height 2) half-fixed-length))
		,fixed-length ,fixed-length)
      `(slice 0 0 ,fixed-length ,fixed-length))))

(defun twittering-make-display-spec-for-icon (image-pair)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-PAIR up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((type (car-safe image-pair))
	 (data (cdr-safe image-pair))
	 (raw-image-spec ;; without margins
	  (create-image data type t))
	 (slice-spec
	  (when (and twittering-convert-fix-size (not twittering-use-convert))
	    (twittering-make-slice-spec raw-image-spec)))
	 (image-spec
	  (if (fboundp 'create-animated-image) ;; Emacs24 or later
	      (create-animated-image data type t :margin 2 :ascent 'center)
	    (create-image data type t :margin 2 :ascent 'center))))
    (if slice-spec
	`(display (,image-spec ,slice-spec))
      `(display ,image-spec))))

(defun twittering-make-icon-string (beg end image-url)
  (let ((display-spec (twittering-get-display-spec-for-icon image-url))
	(image-data (gethash image-url twittering-url-data-hash))
	(properties (and beg (text-properties-at beg)))
	(icon-string (copy-sequence " ")))
    (when properties
      (add-text-properties 0 (length icon-string) properties icon-string))
    (cond
     (display-spec
      (let ((icon-string (apply 'propertize "_"
				(append properties display-spec))))
	;; Remove the property required no longer.
	(remove-text-properties 0 (length icon-string)
				'(need-to-be-updated nil)
				icon-string)
	icon-string))
     ((and (integerp image-data)
	   (<= twittering-url-request-retry-limit image-data))
      ;; Try to retrieve the image no longer.
      (twittering-register-image-data image-url nil)
      (twittering-make-icon-string beg end image-url))
     ((and image-data (not (integerp image-data)))
      (twittering-register-image-data image-url image-data)
      (twittering-make-icon-string beg end image-url))
     (t
      (put-text-property 0 (length icon-string)
			 'need-to-be-updated
			 `(twittering-make-icon-string ,image-url)
			 icon-string)
      (twittering-url-retrieve-async image-url 'twittering-register-image-data)
      icon-string))))

(defun twittering-save-icon-properties (&optional filename)
  (let ((filename (or filename twittering-icon-storage-file))
	(stored-data
	 (cond
	  ((null twittering-icon-storage-limit)
	   (let ((result nil)
		 (dummy-icon-properties (twittering-make-display-spec-for-icon
					 twittering-error-icon-data-pair)))
	     (maphash
	      (lambda (size hash)
		(maphash (lambda (url properties)
			   (unless (equal properties dummy-icon-properties)
			     (setq result (cons (list size url) result))))
			 hash))
	      twittering-icon-prop-hash)
	     result))
	  (t
	   (reverse twittering-icon-storage-recent-icons))))
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (when (require 'jka-compr nil t)
      (with-auto-compression-mode
	(let ((coding-system-for-write 'binary))
	  (with-temp-file filename
	    (insert "( 2 ")
	    (prin1 (cons 'emacs-version emacs-version) (current-buffer))
	    (insert "(icon-list ")
	    (mapc
	     (lambda (entry)
	       (let* ((size (elt entry 0))
		      (url (elt entry 1))
		      (properties
		       (gethash url
				(gethash size twittering-icon-prop-hash))))
		 (insert (if size
			     (format "(%d " size)
			   "(nil "))
		 (prin1 url (current-buffer))
		 (insert " ")
		 (prin1 properties (current-buffer))
		 (insert ")\n")))
	     stored-data)
	    (insert "))")))))))

(defun twittering-load-icon-properties (&optional filename)
  (let* ((filename (or filename twittering-icon-storage-file))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory)
	 (data
	  (with-temp-buffer
	    (condition-case err
		(cond
		 ((and (require 'jka-compr)
		       (file-exists-p filename))
		  (with-auto-compression-mode
		    (let ((coding-system-for-read 'binary)
			  (coding-system-for-write 'binary))
		      (insert-file-contents filename)))
		  (read (current-buffer)))
		 (t
		  nil))
	      (error
	       (message "Failed to load icon images. %s" (cdr err))
	       nil)))))
    (cond
     ((equal 2 (car data))
      (let ((version (cdr (assq 'emacs-version data))))
	(cond
	 ((or (equal version emacs-version)
	      (y-or-n-p
	       (format "%s is generated by Emacs %s! Use it?"
		       filename version)))
	  (mapc (lambda (entry)
		  (let ((size (elt entry 0))
			(url (elt entry 1))
			(properties (elt entry 2)))
		    (twittering-update-icon-storage-recent-icons size url
								 properties)
		    (twittering-register-image-spec url properties size)))
		(cdr (assq 'icon-list data))))
	 (t
	  (message "Stopped loading icons")))))
     (t
      (mapc (lambda (entry)
	      (let ((size (car entry))
		    (prop-alist (cdr entry)))
		(mapc (lambda (entry)
			(let ((url (car entry))
			      (properties (cdr entry)))
			  (twittering-update-icon-storage-recent-icons
			   size url properties)
			  (twittering-register-image-spec url properties
							  size)))
		      prop-alist)))
	    data)))))
