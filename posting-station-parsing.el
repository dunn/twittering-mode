;;;;
;;;; XML parser
;;;;

(defun twittering-ucs-to-char-internal (code-point)
  ;; Check (featurep 'unicode) is a workaround with navi2ch to avoid
  ;; error "error in process sentinel: Cannot open load file:
  ;; unicode".
  ;;
  ;; Details: navi2ch prior to 1.8.3 (which is currently last release
  ;; version as of 2010-01-18) always define `ucs-to-char' as autoload
  ;; file "unicode(.el)" (which came from Mule-UCS), hence it breaks
  ;; `ucs-to-char' under non Mule-UCS environment. The problem is
  ;; fixed in navi2ch dated 2010-01-16 or later, but not released yet.
  (if (and (featurep 'unicode) (functionp 'ucs-to-char))
      (ucs-to-char code-point)
    ;; Emacs21 have a partial support for UTF-8 text, so it can decode
    ;; only parts of a text with Japanese.
    (decode-char 'ucs code-point)))

(defvar twittering-unicode-replacement-char
  ;; "Unicode Character 'REPLACEMENT CHARACTER' (U+FFFD)"
  (or (twittering-ucs-to-char-internal #xFFFD)
      ??)
  "*Replacement character returned by `twittering-ucs-to-char' when it fails
to decode a code.")

(defun twittering-ucs-to-char (code-point)
  "Return a character specified by CODE-POINT in Unicode.
If it fails to decode the code, return `twittering-unicode-replacement-char'."
  (or (twittering-ucs-to-char-internal code-point)
      twittering-unicode-replacement-char))

(defadvice decode-char (after twittering-add-fail-over-to-decode-char)
  (when (null ad-return-value)
    (setq ad-return-value twittering-unicode-replacement-char)))

(defun twittering-xml-parse-region (&rest args)
  "Wrapped `xml-parse-region' in order to avoid decoding errors.
After activating the advice `twittering-add-fail-over-to-decode-char',
`xml-parse-region' is called. This prevents `xml-parse-region' from
exiting abnormally by decoding unknown numeric character reference."
  (let ((activated (ad-is-active 'decode-char)))
    (ad-enable-advice
     'decode-char 'after 'twittering-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (unwind-protect
	(condition-case err
	    (apply 'xml-parse-region args)
	  (error
	   (message "Failed to parse the retrieved XML.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twittering-add-fail-over-to-decode-char)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char)))))

;;;;
;;;; JSON parser with a fallback character
;;;;

(defconst twittering-surrogate-pair-regexp
  (if (<= 23 emacs-major-version)
      ;; Literal strings such as "\uXXXX" is not allowed in Emacs 21
      ;; and earlier. A character of invalid code point such as U+D800
      ;; is not allowed in Emacs 22.
      ;; To avoid errors caused by literal strings invalid in Emacs 22
      ;; and earlier, the regexp is generated indirectly.
      (format "[%c-%c][%c-%c]"
	      (decode-char 'ucs #xd800)
	      (decode-char 'ucs #xdbff)
	      (decode-char 'ucs #xdc00)
	      (decode-char 'ucs #xdfff))
    ;; A regexp that never matches any strings.
    "\\'\\`")
  "Regexp to match a surrogate pair for CESU-8.
In Emacs 22 and earlier, this variable is initialized by a regexp
that never matches any string because code points for a surrogate pair,
from U+D800 to U+DFFF, are invalid.")

(defun twittering-decode-surrogate-pairs-as-cesu-8 (str)
  "Decode surrogate pairs in STR similarly to CESU-8.
If STR includes surrogate pairs represented by code points from U+D800 to
U+DFFF, decode them with CESU-8 and return the result.

A character not in the Basic Multilingual Plane is represented by a surrogate
pair in JSON (RFC4627). This is similar to CESU-8. But the function
`json-read' in `json.el' does not correctly decode surrogate pairs. Therefore,
`json-read' may return a string including invalid code points from U+D800 to
U+DFFF. This function decodes such invalid code points."
  (let ((str str)
	(prev 0)
	(current 0)
	(result ""))
    (while (setq current
		 (string-match twittering-surrogate-pair-regexp str prev))
      (let* ((next (match-end 0))
	     (decoded-str
	      (decode-coding-string
	       (mapconcat
		(lambda (c)
		  (let* ((code-point (encode-char c 'ucs))
			 (b1 (/ code-point #x100))
			 (b2 (% code-point #x100)))
		    (unibyte-string b1 b2)))
		(match-string 0 str)
		"")
	       'utf-16)))
	(setq result
	      (concat result
		      (substring str prev current)
		      decoded-str))
	(setq prev next)))
    (setq result (concat result (substring str prev)))
    result))

(defadvice json-read-string (after twittering-decode-surrogate-pairs-as-cesu-8)
  (when (<= 23 emacs-major-version)
    (setq ad-return-value
	  (twittering-decode-surrogate-pairs-as-cesu-8 ad-return-value))))

(defun twittering-json-read (&rest args)
  "Wrapped `json-read' in order to avoid decoding errors.
`json-read' is called after activating the advice
`twittering-add-fail-over-to-decode-char'.
This prevents `json-read' from exiting abnormally by decoding an unknown
numeric character reference."
  (let ((activated (ad-is-active 'decode-char))
	(json-activated (ad-is-active 'json-read-string)))
    (ad-enable-advice
     'decode-char 'after 'twittering-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (ad-enable-advice 'json-read-string 'after
		      'twittering-decode-surrogate-pairs-as-cesu-8)
    (ad-activate 'json-read-string)
    (unwind-protect
	(condition-case err
	    (apply 'json-read args)
	  (error
	   (message "Failed to parse the retrieved JSON.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twittering-add-fail-over-to-decode-char)
      (ad-disable-advice 'json-read-string 'after
			 'twittering-decode-surrogate-pairs-as-cesu-8)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char))
      (if json-activated
	  (ad-activate 'json-read-string)
	(ad-deactivate 'json-read-string))
      )))

(defun twittering-atom-xmltree-to-status-datum (atom-xml-entry)
  (let* ((id-str (car (cddr (assq 'id atom-xml-entry))))
	 (time-str (car (cddr (assq 'updated atom-xml-entry))))
	 (author-str (car (cddr (assq 'name (assq 'author atom-xml-entry)))))
	 (formatted-time-str
	  ;; ISO 8601
	  ;; Twitter -> "2010-05-08T05:59:41Z"
	  ;; StatusNet -> "2010-05-08T08:44:39+00:00"
	  (cond
	   ((string-match
	     "\\(.*\\)T\\(.*\\)\\(Z\\|\\([-+][0-2][0-9]\\):?\\([0-5][0-9]\\)\\)"
	     time-str)
	    ;; time-str is formatted as
	    ;; "Combined date and time in UTC:" in ISO 8601.
	    (let ((timezone (match-string 3 time-str)))
	      (format "%s %s %s"
		      (match-string 1 time-str) (match-string 2 time-str)
		      (if (string= "Z" timezone)
			  "+0000"
			(concat (match-string 4 time-str)
				(match-string 5 time-str))))))
	   (t
	    ;; unknown format?
	    time-str))))
    `((created-at . ,(date-to-time formatted-time-str))
      (id . ,(progn
	       (string-match ":\\([0-9]+\\)$" id-str)
	       (match-string 1 id-str)))
      ,@(let ((source (twittering-decode-html-entities
		       (car (cddr (assq 'twitter:source atom-xml-entry))))))
	  `(,@(if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				source)
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . "")))))
      (text . ,(twittering-decode-html-entities
		(car (cddr (assq 'title atom-xml-entry)))))
      ,@(cond
	 ((and (eq twittering-service-method 'statusnet)
	       (string-match "^\\([^ ]+\\)\\( (\\(.*\\))\\)?$" author-str))
	  ;; StatusNet
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(or (match-string 3 author-str) ""))))
	 ((string-match "^\\([^ ]+\\) (\\(.*\\))$" author-str)
	  ;; Twitter (default)
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(match-string 2 author-str))))
	 (t
	  '((user-screen-name . "PARSING FAILED!!")
	    (user-name . ""))))
      (user-profile-image-url
       . ,(let* ((link-items
		  (mapcar
		   (lambda (item)
		     (when (eq 'link (car-safe item))
		       (cadr item)))
		   atom-xml-entry))
		 (image-urls
		  (mapcar
		   (lambda (item)
		     (cond
		      ((and (eq twittering-service-method 'statusnet)
			    (member '(rel . "related") item))
		       ;; StatusNet
		       (cdr (assq 'href item)))
		      ((member '(rel . "image") item)
		       ;; Twitter (default)
		       (cdr (assq 'href item)))
		      (t
		       nil)))
		   link-items)))
	    (car-safe (remq nil image-urls)))))))

(defun twittering-atom-xmltree-to-status (atom-xmltree)
  (let ((entry-list
	 (apply 'append
		(mapcar (lambda (x)
			  (if (eq (car-safe x) 'entry) `(,x) nil))
			(cdar atom-xmltree)))))
    (mapcar 'twittering-atom-xmltree-to-status-datum
	    entry-list)))

(eval-and-compile
  (defsubst twittering-make-gap-list (text)
    "Return a list representing index gaps between TEXT and the decoded and normalized text.
Indices of a text in a response from Twitter are calculated with the
assumption that \"&\", \"<\" and \">\" are encoded as \"&amp;\", \"&lt;\"
and \"&gt;\" respectively and a Unicode combining character is considered
as a character.
On rendering a tweet, posting-station decode \"&amp;\", \"&lt;\" and \"&gt;\".
And posting-station also normalize its text into canonically equivalent text
without combining characters.
Therefore, the indices in entities differ from the indices of the corresponding
positions in the decoded text.
In addition, the normalization to NFC also makes additional gaps between
the indices in entities and the corresponding positions.

This function assumes that TEXT is already decoded but not normalized.
From TEXT, the function calculates the gaps between the encoded text and the
decoded and normalized text.
This function returns a list of pairs representing the gaps.
For each pair, the car means the position in the original TEXT and the cdr
means the gap. The (car pair)-th character in the original TEXT corresponds
to the (- (car pair) (cdr pair))-th character in the decoded and normalized
text."
    (let ((result nil)
	  (regexp
	   (if (require 'ucs-normalize nil t)
	       (concat "\\(?:\\([<>&]\\)\\|\\("
		       ucs-normalize-combining-chars-regexp "\\)\\)")
	     "\\([<>&]\\)"))
	  (pos 0)
	  (gap 0))
      (while (string-match regexp text pos)
	(let* ((str (match-string 1 text))
	       (shift (if str
			  (if (string= str "&")
			      4
			    3)
			1)))
	  (setq result
		(cons `(,(+ gap (match-end 0)) . ,(+ gap shift)) result))
	  (setq gap (+ shift gap)))
	(setq pos (match-end 0)))
      (reverse result)))

  (defun twittering-get-gap (pos gap-list)
    "Return the gap at the specific position.
GAP-LIST must be generated by `twittering-make-gap-list'."
    (let ((rest-gaps gap-list)
	  (gap 0))
      (while (and rest-gaps (< (caar rest-gaps) pos))
	(setq gap (cdar rest-gaps))
	(setq rest-gaps (cdr rest-gaps)))
      gap)))

(defun twittering-normalize-raw-status (raw-status &optional ignore-retweet)
  (let* ((status-data (cddr raw-status))
	 (raw-retweeted-status (assq 'retweeted_status status-data)))
    (cond
     ((and raw-retweeted-status
	   (not ignore-retweet))
      (let ((retweeted-status
	     (twittering-normalize-raw-status raw-retweeted-status t))
	    (retweeting-status
	     (twittering-normalize-raw-status raw-status t))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (let ((assq-get (lambda (item seq)
			(car (cddr (assq item seq))))))
	`(,@(mapcar
	     (lambda (entry)
	       (let* ((sym (elt entry 0))
		      (sym-in-data (elt entry 1))
		      (encoded (elt entry 2))
		      (data (funcall assq-get sym-in-data status-data)))
		 `(,sym . ,(if encoded
			       (twittering-decode-html-entities
				(twittering-decode-entities-after-parsing-xml
				 data))
			     data))))
	     '(;; Raw entries.
	       (id id)
	       (in-reply-to-screen-name in_reply_to_screen_name)
	       (in-reply-to-status-id in_reply_to_status_id)
	       (recipient-screen-name recipient_screen_name)
	       ;; Encoded entries.
	       (text text t)
	       ))
	  ;; created_at
	  (created-at
	   . ,(date-to-time (funcall assq-get 'created_at status-data)))
	  ;; Replace "true" and "false" into t and nil.
	  ,@(mapcar (lambda (sym)
		      `(,sym . ,(string= "true"
					 (funcall assq-get sym status-data))))
		    '(favorited truncated))
	  ;; Entities.
	  ,(let* ((entity-data (cddr (assq 'entities status-data)))
		  (encoded-text (funcall assq-get 'text status-data))
		  (text
		   (twittering-decode-html-entities
		    (twittering-decode-entities-after-parsing-xml
		     encoded-text)))
		  (gap-list (twittering-make-gap-list text)))
	     (list
	      'entity
	      ;; hashtags
	      (cons
	       'hashtags
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'hashtag (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(text . ,(elt (assq 'text data) 2))))))
			(assq 'hashtags entity-data))))
	      ;; mentions
	      (cons
	       'mentions
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'user_mention (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(id . ,(elt (assq 'id data) 2))
				(screen-name
				 . ,(elt (assq 'screen_name data) 2))
				(name
				 . ,(elt (assq 'name data) 2))))))
			(assq 'user_mentions entity-data))))
	      ;; urls
	      (cons
	       'urls
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'url (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(url . ,(elt (assq 'url data) 2))
				(display-url
				 . ,(elt (assq 'display_url data) 2))
				(expanded-url
				 . ,(elt (assq 'expanded_url data) 2))))))
			(assq 'urls entity-data))))))
	  ;; Source.
	  ,@(let ((source (twittering-decode-html-entities
			   (funcall assq-get 'source status-data))))
	      (if (and source
		       (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				     source))
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . ""))))
	  ;; Items related to the user that posted the tweet.
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar
	       (lambda (entry)
		 (let* ((sym (elt entry 0))
			(sym-in-user-data (elt entry 1))
			(encoded (elt entry 2))
			(value (funcall assq-get sym-in-user-data user-data)))
		   `(,sym . ,(if encoded
				 (twittering-decode-html-entities value)
			       value))))
	       '(;; Raw entries.
		 (user-id id)
		 (user-profile-image-url profile_image_url)
		 (user-url url)
		 ;; Encoded entries.
		 (user-name name t)
		 (user-screen-name screen_name t)
		 (user-location location t)
		 (user-description description t))))
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar (lambda (entry)
			`(,(car entry)
			  . ,(string=
			      "true"
			      (funcall assq-get (cdr entry) user-data))))
		      '((user-protected . protected))))))))))

(defun twittering-xmltree-to-status (xmltree)
  (setq xmltree
	(cond
	 ((eq 'direct-messages (caar xmltree))
	  `(,@(mapcar
	       (lambda (c-node)
		 `(status nil
			  (created_at
			   nil ,(caddr (assq 'created_at c-node)))
			  (id nil ,(caddr (assq 'id c-node)))
			  (text nil ,(caddr (assq 'text c-node)))
			  (source nil ,(format "%s" (car c-node))) ;; fake
			  (truncated nil "false")
			  (in_reply_to_status_id nil)
			  (in_reply_to_user_id
			   nil ,(caddr (assq 'recipient_id c-node)))
			  (favorited nil "false")
			  (recipient_screen_name
			   nil ,(caddr (assq 'recipient_screen_name c-node)))
			  (user nil ,@(cdddr (assq 'sender c-node)))
			  (entities nil ,@(cdddr (assq 'entities c-node)))))
	       (remove nil
		       (mapcar
			(lambda (node)
			  (and (consp node) (eq 'direct_message (car node))
			       node))
			(cdr-safe (assq 'direct-messages xmltree))))
	       )))
	 ((eq 'statuses (caar xmltree))
	  (cddr (car xmltree)))
	 (t ;; unknown format?
	  nil)))

  (mapcar #'twittering-normalize-raw-status
 	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
 	  ;; On Emacs22, there may be blank strings
	  (remove nil (mapcar (lambda (x)
				(if (consp x) x))
			      xmltree))))

(defun twittering-decode-entities-after-parsing-xml (encoded-str)
  "Decode ENCODED-STR retrieved by parsing XML and return the result.
On Emacs 22 and later, `xml-parse-region' resolves numeric character
references. It is redundant to resolve numeric character references
again. However, in a XML response from Twitter, the two characters,
\"<\" and \">\", are doubly escaped as \"&amp;lt;\" and \"&amp;gt;\",
respectively. Then, they are represented as \"&lt;\" and \"&gt;\" in
the result of `xml-parse-region'. This function decodes them.

On Emacs 21, `xml-parse-region' does not resolve numeric character
references. This function decodes them."
  (cond
   ((null encoded-str)
    "")
   ((> 22 emacs-major-version)
    (replace-regexp-in-string
     "&#\\([0-9]+\\);"
     (lambda (str)
       (let ((number-entity
	      (progn
		(string-match "&#\\([0-9]+\\);" str)
		(match-string 1 str))))
	 (char-to-string
	  (twittering-ucs-to-char (string-to-number number-entity)))))
     encoded-str))
   (t
    (replace-regexp-in-string
     "&\\(?:\\(gt\\)\\|\\(lt\\)\\);"
     (lambda (str)
       (if (match-beginning 1)
	   ">"
	 "<"))
     encoded-str))))

(defun twittering-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (twittering-list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (twittering-list-push
		    (char-to-string
		     (twittering-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond
		    ((string= "amp" letter-entity) (twittering-list-push "&" result))
		    ((string= "gt" letter-entity) (twittering-list-push ">" result))
		    ((string= "lt" letter-entity) (twittering-list-push "<" result))
		    ((string= "quot" letter-entity) (twittering-list-push "\"" result))
		    (t (twittering-list-push "?" result))))
		  (t (twittering-list-push "?" result)))
	    (setq cursor (match-end 0))))
	(twittering-list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

;; JSON
(defun twittering-extract-common-element-from-json (json-object)
  "Extract common parameters of a tweet from JSON-OBJECT.
Return an alist including text, created_at and entities, which are common
to JSON objects from ordinary timeline and search timeline."
  (let* ((encoded-text
	  (cdr (or (assq 'text json-object)
		   (assq 'full_text json-object))))
	 (text
	  (twittering-decode-html-entities
	   (twittering-decode-entities-after-parsing-xml encoded-text)))
	 (gap-list (twittering-make-gap-list text))
	 (entities (cdr (assq 'entities json-object)))
	 (urls (cdr (assq 'urls entities)))
	 (hashtags (cdr (assq 'hashtags entities)))
	 (mentions (cdr (assq 'user_mentions entities)))
	 (media (cdr (assq 'media entities)))
	 (func
	  (lambda (entry sym-table)
	    (mapcar (lambda (sym-entry)
		      (let ((sym (car sym-entry))
			    (target (cdr sym-entry)))
			`(,sym . ,(cdr (assq target entry)))))
		    sym-table))))
    `((text . ,(twittering-normalize-string text))
      (created-at
       . ,(apply 'encode-time
		 (parse-time-string (cdr (assq 'created_at json-object)))))
      (entity
       (hashtags . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twittering-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (text
				   . ,(twittering-normalize-string
				       (cdr (assq 'text entry)))))))
			    hashtags))
       (mentions . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twittering-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (id . ,(cdr (assq 'id_str entry)))
				  (name
				   . ,(twittering-normalize-string
				       (cdr (assq 'name entry))))
				  (screen-name
				   . ,(cdr (assq 'screen_name entry))))))
			    mentions))
       (urls . ,(mapcar (lambda (entry)
			  (let* ((indices (cdr (assq 'indices entry)))
				 (start (elt indices 0))
				 (end (elt indices 1))
				 (gap (twittering-get-gap start gap-list)))
			    `((start . ,(- start gap))
			      (end . ,(- end gap))
			      (url . ,(cdr (assq 'url entry)))
			      (display-url
			       . ,(cdr (assq 'display_url entry)))
			      (expanded-url
			       . ,(cdr (assq 'expanded_url entry))))))
			urls))
       (media . ,(mapcar (lambda (entry)
			  (let* ((indices (cdr (assq 'indices entry)))
				 (start (elt indices 0))
				 (end (elt indices 1))
				 (gap (twittering-get-gap start gap-list)))
			    `((start . ,(- start gap))
			      (end . ,(- end gap))
			      (url . ,(cdr (assq 'url entry)))
			      (raw-entry . ,entry)
			      ,@(funcall func entry
					 '((media-url . media_url)
					   (display-url . display_url)
					   (expanded-url . expanded_url))))))
			 media)))
      (retweet-count . ,(cdr (assq 'retweet_count json-object)))
      (favorite-count . ,(cdr (assq 'favorite_count json-object)))
      )))

(defun twittering-json-object-to-a-status (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in an ordinary timeline, not a search timeline.
To convert a JSON object from a search timeline, use
`twittering-json-object-to-a-status-on-search'."
  (let* ((raw-retweeted-status (cdr (assq 'retweeted_status json-object))))
    (cond
     (raw-retweeted-status
      (let ((retweeted-status
	     (twittering-json-object-to-a-status-base raw-retweeted-status))
	    (retweeting-status
	     (twittering-json-object-to-a-status-base json-object))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (twittering-json-object-to-a-status-base json-object)))))

(defun twittering-json-object-to-a-status-base (json-object)
  (let* ((user-data (cdr (assq 'user json-object)))
	 (raw-quoted-status (cdr (assq 'quoted_status json-object)))
	 (quoted-status
	  (when raw-quoted-status
	    (twittering-json-object-to-a-status raw-quoted-status))))
    `(,@(twittering-extract-common-element-from-json json-object)
      ,@(let ((symbol-table
	       '((favorited . favorited)
		 (id_str . id)
		 (in_reply_to_screen_name . in-reply-to-screen-name)
		 (in_reply_to_status_id_str . in-reply-to-status-id)
		 (recipient_screen_name . recipient-screen-name)
		 (truncated . truncated))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (value
			     (if (and (memq sym '(favorited truncated))
				      (eq value :json-false))
				 nil
			       value))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
      ;; source
      ,@(let ((source (cdr (assq 'source json-object))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,(twittering-normalize-string caption))
		  (source-uri . ,uri)))
	    `((source . ,(twittering-normalize-string source))
	      (source-uri . ""))))
      ;; user data
      ,@(let ((symbol-table
	       '((id_str . user-id)
		 (profile_image_url . user-profile-image-url)
		 (url . user-url)
		 (protected . user-protected)
		 (name . user-name)
		 (screen_name . user-screen-name)
		 (location . user-location)
		 (description . user-description))))
	  (remove nil
		  (mapcar (lambda (entry)
			    (let* ((sym (car entry))
				   (value (cdr entry))
				   (value
				    (cond
				     ((and (eq sym 'protected)
					   (eq value :json-false))
				      nil)
				     ((memq sym '(name location description))
				      (twittering-normalize-string value))
				     (t
				      value))))
			      (when value
				(let ((dest (cdr (assq sym symbol-table))))
				  (when dest
				    `(,dest . ,value))))))
			  user-data)))
      ;; Quoted status.
      ,(when quoted-status
	 `(quoted-status . ,quoted-status))
      )))

(defun twittering-json-object-to-a-status-on-search (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in a search timeline.
To convert a JSON object from other timelines, use
`twittering-json-object-to-a-status'."
  `(,@(twittering-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (to_user . in-reply-to-screen-name)
	       (in_reply_to_status_id_str . in-reply-to-status-id)
	       ;; user data
	       (from_user_id_str . user-id)
	       (profile_image_url . user-profile-image-url)
	       (from_user_name . user-name)
	       (from_user . user-screen-name))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; source
    ,@(let ((source
	       (twittering-decode-html-entities
		(cdr (assq 'source json-object)))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,caption)
		  (source-uri . ,uri)))
	    `((source . ,source)
	      (source-uri . ""))))))

(defun twittering-json-object-to-a-status-on-direct-messages (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in timelines related to direct messages.
To convert a JSON object from other timelines, use
`twittering-json-object-to-a-status'."
  `(,@(twittering-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (recipient_screen_name . recipient-screen-name))))
	(remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; sender
    ,@(let ((symbol-table
	     '((id_str . user-id)
	       (name . user-name)
	       (profile_image_url . user-profile-image-url)
	       (protected . user-protected)
	       (screen_name . user-screen-name))))
	(remove nil
		(mapcar
		 (lambda (entry)
		   (let* ((sym (car entry))
			  (value (cdr entry))
			  (value
			   (cond
			    ((eq sym 'protected)
			     (if (eq value :json-false)
				 nil
			       t))
			    ((eq value :json-false)
			     nil)
			    (t
			     value))))
		     (when value
		       (let ((dest (cdr (assq sym symbol-table))))
			 (when dest
			   `(,dest . ,value))))))
		 (cdr (assq 'sender json-object)))))))
