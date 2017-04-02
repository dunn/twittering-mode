;;;;
;;;; Timeline spec
;;;;

;;; Timeline spec as S-expression
;;; - (user USER): timeline of the user whose name is USER. USER is a string.
;;; - (list USER LIST):
;;;     the list LIST of the user USER. LIST and USER are strings.
;;;
;;; - (direct_messages): received direct messages.
;;; - (direct_messages_sent): sent direct messages.
;;; - (favorites): favorites timeline for the current user.
;;; - (favorites USER): favorites timeline for the specified user.
;;; - (friends): friends timeline.
;;; - (home): home timeline.
;;; - (mentions): mentions timeline.
;;;     mentions (status containing @username) for the authenticating user.
;;; - (public): public timeline.
;;; - (replies): replies.
;;; - (retweeted_by_me): retweets posted by the authenticating user.
;;; - (retweeted_by_user USER): retweets posted by the user.
;;; - (retweeted_to_me): retweets posted by the authenticating user's friends.
;;; - (retweeted_to_user USER): retweets posted to the user.
;;; - (retweets_of_me):
;;;     tweets of the authenticated user that have been retweeted by others.
;;; - (single ID): the single tweet specified by ID.
;;;
;;; - (search STRING): the result of searching with query STRING.
;;;
;;; - (exclude-if FUNC SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that FUNC returns non-nil for.
;;; - (exclude-re REGEXP-STRING SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that matches the regular expression specified by REGEXP-STRING.
;;;
;;; - (merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;;;

;;; Timeline spec string
;;;
;;; SPEC ::= PRIMARY | COMPOSITE
;;; PRIMARY ::= USER | LIST | DIRECT_MESSSAGES | DIRECT_MESSSAGES_SENT
;;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;;             | RETWEETED_BY_ME | RETWEETED_BY_USER
;;;             | RETWEETED_TO_ME | RETWEETED_TO_USER | RETWEETS_OF_ME
;;;             | SEARCH
;;; COMPOSITE ::= EXCLUDE-IF | EXCLUDE-RE | MERGE
;;;
;;; USER ::= /[a-zA-Z0-9_-]+/
;;; LIST ::= USER "/" LISTNAME
;;; LISTNAME ::= /[a-zA-Z0-9_-]+/
;;; DIRECT_MESSSAGES ::= ":direct_messages"
;;; DIRECT_MESSSAGES_SENT ::= ":direct_messages_sent"
;;; FAVORITES ::= ":favorites" | ":favorites/" USER
;;; FRIENDS ::= ":friends"
;;; HOME ::= ":home" | "~"
;;; MENTIONS ::= ":mentions"
;;; PUBLIC ::= ":public"
;;; REPLIES ::= ":replies" | "@"
;;; RETWEETED_BY_ME ::= ":retweeted_by_me"
;;; RETWEETED_BY_USER ::= ":retweeted_by_user/" USER
;;; RETWEETED_TO_ME ::= ":retweeted_to_me"
;;; RETWEETED_TO_USER ::= ":retweeted_to_user/" USER
;;; RETWEETS_OF_ME ::= ":retweets_of_me"
;;; SINGLE ::= ":single/" ID
;;; ID ::= /[0-9]+/
;;;
;;; SEARCH ::= ":search/" QUERY_STRING "/"
;;; QUERY_STRING ::= any string, where "/" is escaped by a backslash.
;;;
;;; EXCLUDE-IF ::= ":exclude-if/" FUNC "/" SPEC
;;; FUNC ::= LAMBDA EXPRESSION | SYMBOL
;;; EXCLUDE-RE ::= ":exclude-re/" REGEXP "/" SPEC
;;;
;;; MERGE ::= "(" MERGED_SPECS ")"
;;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;;;

(defvar twittering-regexp-hash
  (let ((full-width-number-sign (twittering-ucs-to-char #xff03)))
    ;; Unicode Character 'FULLWIDTH NUMBER SIGN' (U+FF03)
    (concat "\\(?:#\\|" (char-to-string full-width-number-sign) "\\)")))

(defvar twittering-regexp-atmark
  (let ((full-width-commercial-at (twittering-ucs-to-char #xff20)))
    ;; Unicode Character 'FULLWIDTH COMMERCIAL AT' (U+FF20)
    (concat "\\(?:@\\|" (char-to-string full-width-commercial-at) "\\)")))

(defun twittering-timeline-spec-to-string (timeline-spec &optional shorten)
  "Convert TIMELINE-SPEC into a string.
If SHORTEN is non-nil, the abbreviated expression will be used."
  (let ((type (car timeline-spec))
	(value (cdr timeline-spec)))
    (cond
     ;; user
     ((eq type 'user) (car value))
     ;; list
     ((eq type 'list) (concat (car value) "/" (cadr value)))
     ;; simple
     ((eq type 'direct_messages) ":direct_messages")
     ((eq type 'direct_messages_sent) ":direct_messages_sent")
     ((eq type 'favorites)
      (if value
	  (concat ":favorites/" (car value))
	":favorites"))
     ((eq type 'friends) ":friends")
     ((eq type 'home) (if shorten "~" ":home"))
     ((eq type 'mentions) (if shorten "@" ":mentions"))
     ((eq type 'public) ":public")
     ((eq type 'replies) ":replies")
     ((eq type 'retweeted_by_me) ":retweeted_by_me")
     ((eq type 'retweeted_by_user) (concat ":retweeted_by_user/" (car value)))
     ((eq type 'retweeted_to_me) ":retweeted_to_me")
     ((eq type 'retweeted_to_user) (concat ":retweeted_to_user/" (car value)))
     ((eq type 'retweets_of_me) ":retweets_of_me")
     ((eq type 'single) (concat ":single/" (car value)))
     ((eq type 'search)
      (let ((query (car value)))
	(concat ":search/"
		(replace-regexp-in-string "\\(\\\\\\|/\\)" "\\\\\\1" query)
		"/")))
     ;; composite
     ((eq type 'exclude-if)
      (let ((func (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-if/" (prin1-to-string func) "/"
		(twittering-timeline-spec-to-string spec))))
     ((eq type 'exclude-re)
      (let ((regexp-str (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-re/"
		(replace-regexp-in-string "/" "\\\\\/" regexp-str)
		"/"
		(twittering-timeline-spec-to-string spec))))
     ((eq type 'merge)
      (concat "("
	      (mapconcat 'twittering-timeline-spec-to-string value "+")
	      ")"))
     (t
      nil))))

(eval-and-compile
  (defmacro twittering-make-user-timeline-spec-direct (user)
    `(list 'user ,user))
  (defmacro twittering-make-list-timeline-spec-direct (owner listname)
    `(list 'list ,owner ,listname))
  (defmacro twittering-make-hashtag-timeline-spec-direct (tag)
    `(list 'search (concat "#" ,tag)))
  (defmacro twittering-make-hashtag-timeline-spec-string-direct (tag)
    `(concat "#" ,tag)))

(defun twittering-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (cond
   ((null str)
    (error "STR is nil")
    nil)
   ((string-match "^\\([a-zA-Z0-9_-]+\\)/\\([[:word:]_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (listname (match-string 2 str))
	  (rest (substring str (match-end 0))))
      `(,(twittering-make-list-timeline-spec-direct user listname) . ,rest)))
   ((string-match "^\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (rest (substring str (match-end 0))))
      `(,(twittering-make-user-timeline-spec-direct user) . ,rest)))
   ((string-match "^~" str)
    `((home) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-atmark) str)
    `((mentions) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-hash "\\([[:alpha:]0-9_-]+\\)")
		  str)
    (let* ((tag (match-string 1 str))
	   (rest (substring str (match-end 0))))
      `(,(twittering-make-hashtag-timeline-spec-direct tag) . ,rest)))
   ((string-match "^:\\([a-z_-]+\\)" str)
    (let ((type (match-string 1 str))
	  (following (substring str (match-end 0)))
	  (alist '(("direct_messages" . direct_messages)
		   ("direct_messages_sent" . direct_messages_sent)
		   ("friends" . friends)
		   ("home" . home)
		   ("mentions" . mentions)
		   ("public" . public)
		   ("replies" . replies)
		   ("retweeted_by_me" . retweeted_by_me)
		   ("retweeted_to_me" . retweeted_to_me)
		   ("retweets_of_me" . retweets_of_me))))
      (cond
       ((assoc type alist)
	(let ((first-spec (list (cdr (assoc type alist)))))
	  (cons first-spec following)))
       ((string= type "favorites")
	(if (string-match "^:favorites/\\([a-zA-Z0-9_-]+\\)" str)
	    (let ((rest (substring str (match-end 0))))
	      `((favorites ,(match-string 1 str)) . ,rest))
	  `((favorites) . ,following)))
       ((string-match "^:retweeted_by_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_by_user ,user) . ,rest)))
       ((string-match "^:retweeted_to_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_to_user ,user) . ,rest)))
       ((string-match "^:single/\\([0-9]+\\)" str)
	(let ((id (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((single ,id) . ,rest)))
       ((string= type "search")
	(if (string-match "^:search/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			  str)
	    (let* ((escaped-query (or (match-string 1 str) ""))
		   (query (replace-regexp-in-string "\\\\\\(\\\\\\|/\\)" "\\1"
						    escaped-query))
		   (rest (substring str (match-end 0))))
	      (if (not (string= "" escaped-query))
		  `((search ,query) . ,rest)
		(error "\"%s\" has no valid regexp" str)
		nil))))
       ((string= type "exclude-if")
	(let ((result-pair
	       (cond
		((string-match "^:exclude-if/\\([^(/]+\\)/" str)
		 `(,(intern (match-string 1 str)) . ,(match-end 1)))
		((string-match "^:exclude-if/" str)
		 (condition-case err
		     (read-from-string str (match-end 0))
		   (error
		    nil))))))
	  (if result-pair
	      (let ((func (car result-pair))
		    (pos (cdr result-pair)))
		(cond
		 ((not (functionp func))
		  (error "\"%s\" has an invalid function" str)
		  nil)
		 ((<= (length str) (1+ pos))
		  (error "\"%s\" has no timeline spec" str)
		  nil)
		 ((not (char-equal ?/ (aref str pos)))
		  (error "\"%s\" has no delimiter" str)
		  nil)
		 (t
		  (let* ((pair (twittering-extract-timeline-spec
				(substring str (1+ pos)) unresolved-aliases))
			 (spec (car pair))
			 (rest (cdr pair)))
		    `((exclude-if ,func ,spec) . ,rest)))))
	    (error "\"%s\" has an invalid function" str)
	    nil)))
       ((string= type "exclude-re")
	(cond
	 ((string-match "^:exclude-re/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			str)
	  (let* ((escaped-regexp (or (match-string 1 str) ""))
		 (regexp
		  (replace-regexp-in-string "\\\\/" "/" escaped-regexp nil t))
		 (following (substring str (match-end 0))))
	    (cond
	     ((string= "" escaped-regexp)
	      (error "\"%s\" has no valid regexp" str)
	      nil)
	     (t
	      (let* ((pair (twittering-extract-timeline-spec
			    following unresolved-aliases))
		     (spec (car pair))
		     (rest (cdr pair)))
		`((exclude-re ,regexp ,spec) . ,rest))))))
	 (t
	  (error "\"%s\" has no valid regexp" str)
	  nil)))
       (t
	(error "\"%s\" is invalid as a timeline spec" str)
	nil))))
   ((string-match "^\\$\\([a-zA-Z0-9_-]+\\)\\(?:(\\([^)]*\\))\\)?" str)
    (let* ((name (match-string 1 str))
	   (rest (substring str (match-end 0)))
	   (value (cdr-safe (assoc name twittering-timeline-spec-alias)))
	   (arg (match-string 2 str)))
      (if (member name unresolved-aliases)
	  (error "Alias \"%s\" includes a recursive reference" name)
	(cond
	 ((stringp value)
	  (twittering-extract-timeline-spec
	   (concat value rest)
	   (cons name unresolved-aliases)))
	 ((functionp value)
	  (twittering-extract-timeline-spec
	   (funcall value arg)
	   (cons name unresolved-aliases)))
	 (t
	  (error "Alias \"%s\" is undefined" name))))))
   ((string-match "^(" str)
    (let ((rest (concat "+" (substring str (match-end 0))))
	  (result '()))
      (while (and rest (string-match "^\\+" rest))
	(let* ((spec-string (substring rest (match-end 0)))
	       (pair (twittering-extract-timeline-spec
		      spec-string unresolved-aliases))
	       (spec (car pair))
	       (next-rest (cdr pair)))
	  (setq result (cons spec result))
	  (setq rest next-rest)))
      (if (and rest (string-match "^)" rest))
	  (let ((spec-list
		 (twittering-remove-duplicates
		  (apply 'append
			 (mapcar (lambda (x) (if (eq 'merge (car x))
						 (cdr x)
					       (list x)))
				 (reverse result))))))
	    (if (= 1 (length spec-list))
		`(,(car spec-list) . ,(substring rest 1))
	      `((merge ,@spec-list) . ,(substring rest 1))))
	(if rest
	    ;; The string following the opening parenthesis `('
	    ;; can be interpreted without errors,
	    ;; but there is no corresponding closing parenthesis.
	    (error "\"%s\" lacks a closing parenthesis" str))
	;; Does not display additional error messages if an error
	;; occurred on interpreting the string following
	;; the opening parenthesis `('.
	nil)))
   (t
    (error "\"%s\" is invalid as a timeline spec" str)
    nil)
   ))

(defun twittering-string-to-timeline-spec (spec-str &optional noerror)
  "Convert SPEC-STR into a timeline spec.
If SPEC-STR is invalid as a timeline spec string, raise an error or return
nil if NOERROR is non-nil."
  (let ((result-pair
	 (condition-case err
	     (twittering-extract-timeline-spec spec-str)
	   (error
	    (if noerror
		nil
	      (signal (car err) (cdr err))
	      nil)))))
    (if (and result-pair (string= "" (cdr result-pair)))
	(car result-pair)
      nil)))

(defun twittering-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`merge'."
  (let ((primary-spec-types
	 '(user list
		direct_messages direct_messages_sent
		favorites friends home mentions public replies
		search
		retweeted_by_me retweeted_by_user
		retweeted_to_me retweeted_to_user
		retweets_of_me
		single))
	(type (car spec)))
    (memq type primary-spec-types)))

(defun twittering-timeline-spec-composite-p (spec)
  "Return non-nil if SPEC is a composite timeline spec.
`composite' means that the spec depends on other timelines."
  (let ((composite-spec-types
	 '(exclude-if exclude-re merge))
	(type (car spec)))
    (memq type composite-spec-types)))

(defun twittering-timeline-spec-depending-on-p (spec base-spec)
  "Return non-nil if SPEC depends on BASE-SPEC."
  (cond
   ((twittering-timeline-spec-primary-p spec)
    (equal spec base-spec))
   ((equal spec base-spec)
    t)
   (t
    (remove
     nil
     (mapcar
      (lambda (direct-base-spec)
	(twittering-timeline-spec-depending-on-p direct-base-spec base-spec))
      (twittering-get-base-timeline-specs spec))))))

(defun twittering-timeline-spec-is-user-p (spec)
  "Return non-nil if SPEC is a user timeline."
  (and (consp spec) (eq 'user (car spec))))

(defun twittering-timeline-spec-is-direct-messages-p (spec)
  "Return non-nil if SPEC is a timeline spec which is related of
direct_messages."
  (and spec
       (memq (car spec) '(direct_messages direct_messages_sent))))

(defun twittering-timeline-spec-is-search-p (spec)
  "Return non-nil if SPEC is a search timeline spec."
  (and (consp spec)
       (eq 'search (car spec))))

(defun twittering-extract-query-string-from-search-timeline-spec (spec)
  "Return the query string if SPEC is a search timeline spec.
If SPEC is not a search timeline spec, return nil."
  (and (eq 'search (car spec))
       (cadr spec)))

(defun twittering-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec.
If either SPEC-STR1 or SPEC-STR2 is invalid as a timeline spec string,
return nil."
  (if (and (stringp spec-str1) (stringp spec-str2))
      (let ((spec1 (twittering-string-to-timeline-spec spec-str1 t))
	    (spec2 (twittering-string-to-timeline-spec spec-str2 t)))
	(equal spec1 spec2))
    nil))

(defun twittering-get-base-timeline-specs (spec)
  "Return the timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines may be a composite timeline."
  (let ((type (car spec)))
    (cond
     ((twittering-timeline-spec-primary-p spec)
      `(,spec))
     ((memq type '(exclude-if exclude-re))
      `(,(elt spec 2)))
     ((eq type 'merge)
      (cdr spec))
     (t
      nil))))

(defun twittering-get-primary-base-timeline-specs (spec)
  "Return the primary timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines are primary."
  (if (twittering-timeline-spec-primary-p spec)
      `(,spec)
    (twittering-remove-duplicates
     (apply 'append
	    (mapcar 'twittering-get-primary-base-timeline-specs
		    (twittering-get-base-timeline-specs spec))))))

(defun twittering-get-dependent-timeline-specs (base-spec)
  "Return a list of timeline specs that depend on BASE-SPEC.
If BASE-SPEC is a primary timeline spec, the return value consists of
BASE-SPEC and composite timeline specs that depend on BASE-SPEC and are
bound to a live buffer.
If BASE-SPEC is a composite timeline spec, the return value consists of
composite timeline specs that depend on BASE-SPEC and are bound to a live
buffer."
  (twittering-remove-duplicates
   `(;; BASE-SPEC may not be bound to a live buffer.
     ,@(when (twittering-timeline-spec-primary-p base-spec)
	 `(,base-spec))
     ,@(remove
	nil
	(mapcar
	 (lambda (spec)
	   (when (twittering-timeline-spec-depending-on-p spec base-spec)
	     spec))
	 (mapcar 'twittering-get-timeline-spec-for-buffer
		 (twittering-get-buffer-list)))))))

(defun twittering-generate-composite-timeline (spec base-spec base-statuses)
  "Generate statuses for the timeline SPEC from BASE-STATUSES.
BASE-STATUSES must originate from the BASE-SPEC timeline.
If SPEC is a primary timeline and equals BASE-SPEC, just return BASE-STATUSES.
If SPEC is a primary timeline and does not equal BASE-SPEC, return nil."
  (let ((type (car spec)))
    (cond
     ((twittering-timeline-spec-primary-p spec)
      (if (equal spec base-spec)
	  (let ((pattern-list
		 (twittering-get-filter-list-for-timeline-spec
		  spec)))
	    (if pattern-list
		(remove
		 nil
		 (mapcar
		  (lambda (status)
		    (if (twittering-match-pattern-list status pattern-list)
			(progn
			  (debug-printf "Exclude the status: %s" status)
			  nil)
		      status))
		  base-statuses))
	      base-statuses))
	nil))
     ((eq type 'exclude-if)
      (let* ((direct-base (car (twittering-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twittering-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (func (elt spec 1)))
	(remove nil
		(mapcar (lambda (status)
			  (unless (funcall func status)
			    status))
			direct-base-statuses))))
     ((eq type 'exclude-re)
      (let* ((direct-base (car (twittering-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twittering-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (regexp (elt spec 1)))
	(remove nil
		(mapcar
		 (lambda (status)
		   (unless (string-match regexp (cdr (assq 'text status)))
		     status))
		 direct-base-statuses))))
     ((eq type 'merge)
      (sort
       (apply 'append
	      (mapcar (lambda (direct-base-spec)
			;; `copy-sequence' is required because `sort'
			;; modifies the appended list that may include
			;; `base-statuses' as a tail.
			;; `base-statuses' may refer to the original list
			;; which already retrieved tweets are registered
			;; with. It must not be modified.
			(copy-sequence
			 (twittering-generate-composite-timeline
			  direct-base-spec base-spec base-statuses)))
		      (twittering-get-base-timeline-specs spec)))
       (lambda (status1 status2)
	 (let ((id1 (cdr (assq 'id status1)))
	       (id2 (cdr (assq 'id status2))))
	   (twittering-status-id< id2 id1)))))
     (t
      nil))))

(defun twittering-remove-timeline-data (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (remhash spec twittering-timeline-data-table)))

;;;;
;;;; Status retrieval
;;;;

(defun twittering-add-timeline-history (spec-string)
  (when (or (null twittering-timeline-history)
	    (not (string= spec-string (car twittering-timeline-history))))
    (twittering-add-to-history 'twittering-timeline-history spec-string))
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (when (and (twittering-timeline-spec-is-user-p spec)
	       (or (null twittering-user-history)
		   (not (string= spec-string (car twittering-user-history)))))
      (twittering-add-to-history 'twittering-user-history (cadr spec)))))

(defun twittering-remove-timeline-spec-string-from-history (spec-string)
  (setq twittering-timeline-history
	(remove nil
		(mapcar
		 (lambda (str)
		   (if (twittering-equal-string-as-timeline spec-string str)
		       nil
		     str))
		 twittering-timeline-history))))

(defun twittering-make-alist-of-forbidden-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twittering-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . "SORRY, YOU ARE NOT AUTHORIZED TO SEE THIS TWEET.")
    )))

(defun twittering-make-alist-of-non-existent-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twittering-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . ,(format "THE TWEET WITH ID %s DOES NOT EXIST." id))
    )))

;;;;
;;;; Display replied statuses
;;;;

(defun twittering-replied-statuses-visible-p (&optional pos)
  "Return non-nil if a replied status related to POS is visible.
Return non-nil if a replied status has been rendered at POS by
`twittering-show-replied-statuses'.
Return non-nil if a reply is rendered at POS and the replied statuses
has been rendered by `twittering-show-replied-statuses'.
Otherwise, return nil."
  (let* ((pos (twittering-get-current-status-head pos))
	 (id (twittering-get-id-at pos))
	 (prev (twittering-get-previous-status-head pos))
	 (next (twittering-get-next-status-head pos)))
    (when id
      ;; If ID is nil, it means that no normal tweets are rendered at POS.
      (or
       (twittering-get-base-id-of-ancestor-at pos)
       (and prev
	    (twittering-status-id=
	     id (twittering-get-base-id-of-ancestor-at prev)))
       (and next
	    (twittering-status-id=
	     id (twittering-get-base-id-of-ancestor-at next)))))))

(defun twittering-get-beginning-of-visible-replied-statuses (&optional pos)
  "Return the beginning position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twittering-show-replied-statuses' has rendered a replied status at POS,
return the beginning position of the replied statuses with the common base
status.
If a reply is rendered at POS and its ancestors has been rendered by
`twittering-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twittering-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let* ((prev (twittering-get-previous-status-head pos))
		 (prev-base-id
		  (when prev
		    (twittering-get-base-id-of-ancestor-at prev))))
	    (and prev prev-base-id
		 (twittering-status-id= base-id prev-base-id)
		 (setq pos prev))))
      (or pos (point-min)))
     ((twittering-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twittering-reverse-mode
	  (twittering-get-beginning-of-visible-replied-statuses
	   (twittering-get-previous-status-head pos))
	(twittering-get-next-status-head pos)))
     (t
      nil))))

(defun twittering-get-end-of-visible-replied-statuses (&optional pos)
  "Return the end position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twittering-show-replied-statuses' has rendered a replied status at POS,
return the end position of the replied statuses with the common base status.
If a reply is rendered at POS and its ancestors has been rendered by
`twittering-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twittering-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let ((current-base-id (twittering-get-base-id-of-ancestor-at pos)))
	    (and current-base-id
		 (twittering-status-id= base-id current-base-id)
		 (setq pos (twittering-get-next-status-head pos)))))
      (or pos (point-max)))
     ((twittering-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twittering-reverse-mode
	  (twittering-get-current-status-head pos)
	(twittering-get-end-of-visible-replied-statuses
	 (twittering-get-next-status-head pos))))
     (t
      nil))))

(defun twittering-render-replied-statuses (&optional pos count)
  "Render replied statuses on the position specified by POS.
If POS is nil, the current position is used instead.
If COUNT is a positive integer, it specifies the number of rendered statuses.
If COUNT is nil, all ancestor statuses that have been already retrieved are
rendered.

Return non-nil if one or more statuses are rendered.
Return nil if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (id
	  ;; nil if no normal statuses are rendered at POS.
	  (twittering-get-id-at pos))
	 (replied-status-are-visible
	  (when id
	    (twittering-replied-statuses-visible-p pos)))
	 (base-id (if replied-status-are-visible
		      (or
		       ;; If a replied status is rendered at POS.
		       (twittering-get-base-id-of-ancestor-at pos)
		       ;; If the base reply is rendered at POS.
		       id)
		    id))
	 (statuses
	  (when base-id
	    (twittering-get-replied-statuses base-id (if (numberp count)
							 count))))
	 (statuses (if twittering-reverse-mode
		       statuses
		     (reverse statuses))))
    (cond
     ((null id)
      ;; No normal statuses are rendered here.
      nil)
     (statuses
      (let ((pos
	     (cond
	      ((twittering-replied-statuses-visible-p pos)
	       ;; Some replied statuses have been already rendered.
	       (twittering-get-beginning-of-visible-replied-statuses pos))
	      (twittering-reverse-mode
	       (twittering-get-current-status-head pos))
	      (t
	       (or (twittering-get-next-status-head pos)
		   (point-max)))))
	    (prefix "  ")
	    (buffer-read-only nil))
	(save-excursion
	  (goto-char pos)
	  (mapc
	   (lambda (status)
	     (twittering-render-a-field
	      (point)
	      (twittering-make-field-id status base-id)
	      (let ((formatted-status (twittering-format-status status prefix))
		    (field-properties
		     (twittering-make-properties-of-popped-ancestors base-id)))
		(add-text-properties 0 (length formatted-status)
				     field-properties formatted-status)
		formatted-status)))
	   statuses)
	  t)))
     (t
      nil))))
