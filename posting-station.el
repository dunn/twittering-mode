;;; posting-station.el --- Emacs interface for microblogging

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard
;;               2017 Alex Dunn

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;	Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
;;	Xavier Maillard <xavier@maillard.im>
;;      Alex Dunn <dunn.alex@gmail.com>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Identity: $Id: 4c7e21200d63d8f772cb9f100dd4bf609f3a282a $
;; Keywords: twitter web
;; Prefix: posting
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

;; twittering-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; * Status Input from Popup buffer and C-cC-c to POST.
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(eval-when-compile (require 'cl)
		   (require 'easymenu))
(require 'xml)

(eval-and-compile
  ;; On byte-compilation, Emacs21 requires loading the libraries
  ;; distributed with posting-station.el for macros defined in them.
  (when (> 22 emacs-major-version)
    (setq load-path
	  (append (mapcar (lambda (dir)
			    (expand-file-name
			     dir
			     (if load-file-name
				 (or (file-name-directory load-file-name)
				     ".")
			       ".")))
			  '("url-emacs21" "emacs21"))
		  load-path))))

(when (> 22 emacs-major-version)
  (and (require 'un-define nil t)
       ;; the explicitly require 'unicode to update a workaround with
       ;; navi2ch. see a comment of `twittering-ucs-to-char' for more
       ;; details.
       (require 'unicode nil t))
  (defadvice url-scheme-register-proxy (around twittering-fix-process-env (scheme) activate)
    (let ((process-environment
	   (apply 'append
		  (let ((env-var (concat scheme "_proxy")))
		    (mapcar
		     (lambda (str)
		       (if (string-match
			    (concat "^\\("
				    (regexp-opt (list (upcase env-var)
						      (downcase env-var)))
				    "\\)=$")
			    str)
			   nil
			 (list str)))
		     process-environment)))))
      ad-do-it)))
(require 'url)

(defgroup posting-station nil
  "Settings for posting-station."
  :link '(url-link "https://github.com/hayamiz/twittering-mode")
  :prefix "posting-"
  :group 'hypermedia)

(defconst posting-station-version "HEAD")
(defconst posting-station-identity "$Id: 4c7e21200d63d8f772cb9f100dd4bf609f3a282a $")

(defun posting-station-version ()
  "Display a message for posting-station version."
  (interactive)
  (let ((version-string
	 (format "posting-station-v%s" posting-station-version)))
    (if (called-interactively-p)
	(message "%s" version-string)
      version-string)))

(defcustom twittering-tinyurl-service 'tinyurl
  "*Shorten URI service to use.

This must be one of key symbols of `twittering-tinyurl-services-map'.

To use bit.ly or j.mp services, you have to configure
`twittering-bitly-login' and `twittering-bitly-api-key'."
  :type '(radio (symbol :tag "bit.ly"
			:value bit.ly)
		(symbol :tag "goo.gl"
			:value goo.gl)
		(symbol :tag "is.gd"
			:value is.gd)
		(symbol :tag "j.mp"
			:value j.mp)
		(symbol :tag "migre.me"
			:value migre.me)
		(symbol :tag "tinyurl"
			:value tinyurl)
		(symbol :tag "toly"
			:value toly))
  :group 'posting-station)

(defcustom twittering-tinyurl-services-map
  '((bit.ly twittering-make-http-request-for-bitly
	    (lambda (service reply)
	      (if (string-match "\n\\'" reply)
		  (substring reply 0 (match-beginning 0))
		reply)))
    (goo.gl
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" '(("Content-Type" . "application/json"))
	"https://www.googleapis.com/urlshortener/v1/url"
	(concat "{\"longUrl\": \"" longurl "\"}")))
     (lambda (service reply)
       (when (string-match "\"id\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\""
			   reply)
	 (match-string 1 reply))))
    (is.gd . "http://is.gd/create.php?format=simple&url=")
    (j.mp twittering-make-http-request-for-bitly
	  (lambda (service reply)
	    (if (string-match "\n\\'" reply)
		(substring reply 0 (match-beginning 0))
	      reply)))
    (migre.me . "http://migre.me/api.txt?url=")
    (tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" nil
	"http://to.ly/api.php"
	(concat "longurl=" (twittering-percent-encode longurl))))))
  "Alist of URL shortening services.
The key is a symbol specifying the service.
The value is a string or a list consisting of two elements at most.

If the value is a string, `(concat THE-FIRST-ELEMENT longurl)' is used as the
URL invoking the service.

If the value is a list, it is interpreted as follows.
The first element specifies how to make a HTTP request for shortening a URL.
If the first element is a string, `(concat THE-FIRST-ELEMENT longurl)' is
used as the URL invoking the service.
If the first element is a function, it is called as `(funcall THE-FIRST-ELEMENT
service-symbol longurl)' to obtain a HTTP request alist for invoking the
service, which must be generated by `twittering-make-http-request'.

The second element specifies how to post-process a HTTP reply by the HTTP
request.
If the second element is nil, the reply is directly used as a shortened URL.
If the second element is a function, it is called as `(funcall
THE-SECOND-ELEMENT service-symbol HTTP-reply-string)' and its result is used
as a shortened URL."
  :type 'alist
  :group 'posting-station)

(defcustom twittering-bitly-login nil
  "*The login name for URL shortening service bit.ly and j.mp."
  :type '(choice (const nil)
		 string)
  :group 'posting-station)

(defcustom twittering-bitly-api-key nil
  "*API key for `bit.ly' and `j.mp' URL shortening services."
  :type '(choice (const nil)
		 string)
  :group 'posting-station)

(defvar posting-station-map (make-sparse-keymap))
(defvar posting-station-menu-on-uri-map (make-sparse-keymap "Twittering Mode"))
(defvar posting-station-on-uri-map (make-sparse-keymap))

(defvar twittering-tweet-history nil)
(defvar twittering-user-history nil)
(defvar twittering-timeline-history nil)
(defvar twittering-hashtag-history nil)
(defvar twittering-search-history nil)

(defvar twittering-current-hashtag nil
  "A hash tag string currently set.
You can set it by calling `twittering-set-current-hashtag'.")

(defvar twittering-timer nil
  "Timer object for timeline refreshing will be stored here.
DO NOT SET VALUE MANUALLY.")

(defcustom twittering-timer-interval 90
  "Number of seconds to wait before an auto-reload occurs.

Number of API calls per hour is limited so this value should be 60 or more."
  :type 'integer
  :group 'posting-station)

(defvar twittering-timer-for-redisplaying nil
  "Timer object for timeline redisplay statuses will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-timer-interval-for-redisplaying 5.0
  "The interval of auto redisplaying statuses.
Each time Emacs remains idle for the interval, posting-station updates parts
requiring to be redrawn.")

(defcustom twittering-initial-timeline-spec-string ":home"
  "*An initial timeline spec string or a list of timeline spec strings.
This specifies one or more initial timeline spec strings, which are
automatically visited when invoking `posting-station' or `twit'.

If it is a string, it specifies a timeline spec string.
If it is a list of strings, it specifies multiple timeline spec strings."
  :type '(choice (const nil)
		 string
		 (repeat string))
  :group 'posting-station)

(defvar twittering-timeline-spec nil
  "The timeline spec for the current buffer.")
(defvar twittering-timeline-spec-string ""
  "The timeline spec string for the current buffer.")

(defcustom twittering-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME is a string and
SPEC-STRING is a string or a function that returns a timeline spec string.

The alias can be referred as \"$NAME\" or \"$NAME(ARG)\" in timeline spec
string.  If SPEC-STRING is a string, ARG is simply ignored.
If SPEC-STRING is a function, it is called with a string argument.
For the style \"$NAME\", the function is called with nil.
For the style \"$NAME(ARG)\", the function is called with a string ARG.

For example, if you specify
 `((\"FRIENDS\" . \"my-account/friends-list\")
   (\"related-to\" .
            ,(lambda (username)
               (if username
                   (format \":search/to:%s OR from:%s OR @%s/\"
                           username username username)
                 \":home\")))),
then you can use \"$FRIENDS\" and \"$related-to(USER)\" as
\"my-account/friends-list\" and \":search/to:USER OR from:USER OR @USER/\",
respectively."
  :type 'alist
  :group 'posting-station)

(defvar twittering-current-timeline-spec-string nil
  "The current timeline spec string.
This variable should not be referred directly.
Use `twittering-current-timeline-spec-string' or
`twittering-current-timeline-spec'.")
(defvar twittering-list-index-retrieved nil)

(defvar twittering-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twittering-timeline-spec-to-api-table '()
  "Alist of a timeline spec and an API identifier for retrieving the timeline.")

(defcustom posting-station-init-hook nil
  "*Hook run after initializing global variables for `posting-station'."
  :type 'hook
  :group 'posting-station)

(defcustom posting-station-hook nil
  "*Hook run every time a buffer is initialized as a `posting-station' buffer."
  :type 'hook
  :group 'posting-station)

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-spec nil
  "Timeline spec, which new tweets belong to, when `twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-statuses nil
  "New tweet status messages, when `twittering-new-tweets-hook' is run.")

(defcustom twittering-new-tweets-hook nil
  "*Hook run when new tweets are received.

You can read `twittering-new-tweets-count' or `twittering-new-tweets-spec'
to get the number of new tweets received when this hook is run."
  :type 'hook
  :group 'posting-station)

(defvar twittering-rendered-new-tweets-spec nil
  "A timeline spec of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twittering-new-tweets-rendered-hook'.")

(defvar twittering-rendered-new-tweets-spec-string nil
  "A timeline spec string of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twittering-new-tweets-rendered-hook'.")

(defvar twittering-rendered-new-tweets nil
  "A list of newly rendered tweets.
Hooks registered with `twittering-new-tweets-rendered-hook' can use this \
variable as a list of rendered tweets.  Each tweet is represented as an alist.
You can refer to a property of a tweet alist as
 (cdr (assq PROPERTY-SYMBOL TWEET-ALIST)).
Valid symbols are following; id, text, user-name, user-screen-name, user-id,
 source, source-uri.
In the list, tweets are placed in order of time.  The car of the list is the
latest one, and the last is the oldest one.")

(defcustom twittering-new-tweets-rendered-hook nil
  "*Hook run when new tweets are rendered.
When the registered functions are called, the current buffer is the buffer
that the new tweets are just rendered on.
The functions can refer to the timeline spec and timeline spec string as
`twittering-rendered-new-tweets-spec' and
`twittering-rendered-new-tweets-spec-string', repectively.
Hooks can also use the local variable `twittering-rendered-new-tweets' as a
list of rendered tweets.
For the detail of the representation of tweets, see the variable
`twittering-rendered-new-tweets'."
  :type 'hook
  :group 'posting-station)

(defvar twittering-active-mode nil
  "Non-nil if new statuses should be retrieved periodically.
Do not modify this variable directly.
Use `twittering-activate-buffer', `twittering-deactivate-buffer',
`twittering-toggle-activate-buffer' or
`twittering-set-active-flag-for-buffer'.")

(defvar twittering-jojo-mode nil)
(defcustom twittering-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode-line."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-display-connection-method t
  "*If non-nil, display the current connection method on the mode-line."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-status-format "%RT{%FACE[bold]{RT}}%i %s,  %@:\n%FOLD[  ]{%T // from %f%L%r%R%QT{\n+----\n%FOLD[|]{%i %s,  %@:\n%FOLD[  ]{%T // from %f%L%r%R}}\n+----}}\n "
  "Format string for rendering statuses.
Ex.: \"%i %s,  %@:\\n%FILL{  %T // from %f%L%r%R}\n \"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" sent to user\" (use on direct_messages{,_sent})
 %r - \" in reply to user\" (use on other standard timeline)
 %R - \" (retweeted by user)\"
 %RT{...} - strings rendered only when the tweet is a retweet.
            The braced strings are rendered with the information of the
            retweet itself instead of that of the retweeted original tweet.
            For example, %s for a retweet means who posted the original
            tweet, but %RT{%s} means who retweeted it.
 %QT{...} - strings rendered only when the tweet quotes a tweet.
            The braced strings are rendered with the information of the
            quoted tweet.  For example, %QT{%s} means the author of the
            quoted tweet.
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@{time-format-str} - X seconds ago (formatted with time-format-str)
 %T - raw text
 %t - text filled as one paragraph
 %' - truncated
 %FACE[face-name]{...} - strings decorated with the specified face.
 %FIELD[format-str]{field-name}
   - a value of the given field of a tweet formatted with format-str.
     The format-str is optional.  As a field-name, you can use
     \"retweet_count\", \"favorite_count\" and so on.
 %FIELD-IF-NONZERO[format-str]{field-name}
   - similar to %FIELD[...]{...} except that this makes an empty string
     if the field value is zero.
 %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
                      You can use any other specifiers in braces.
 %FOLD[prefix]{...} - strings folded within the frame width.
                      The prefix is optional. This keeps newlines and does not
                      squeeze a series of white spaces.
                      You can use any other specifiers in braces.
 %f - source
 %# - id"
  :type 'string
  :group 'posting-station)

(defcustom twittering-retweet-format '(nil _ " RT: %t (via @%s)")
  "*A format string or a skeleton for retweet.
If the value is a string, it means a format string for generating an initial
string of a retweet.  The format string is converted with the below replacement
table.  And then, the cursor is placed on the next of the initial string.
It is equivalent to the skeleton '(nil STRING _).
Note that this string is inserted before the edit skeleton specified by
`twittering-edit-skeleton' is performed.

If the value is a list, it is treated as a skeleton used with
`skeleton-insert'.  The strings included in the list are converted with the
following replacement table.  And then, the list with converted strings is
inserted by `skeleton-insert'.
Note that this skeleton is performed before the edit skeleton specified by
`twittering-edit-skeleton' is performed.

Replacement table:
 %s - The screen-name of the cited tweet.
 %t - The text of the cited tweet.
 %u - The URL of the cited tweet.
 %# - The ID of the cited tweet.
 %% - % itself."
  :type 'sexp
  :group 'posting-station)

(defcustom twittering-fill-column nil
  "*The `fill-column' used for \"%FILL{...}\" in `twittering-status-format'.
If nil, the fill-column is automatically calculated."
  :type '(choice (const nil)
		 integer)
  :group 'posting-station)

(defcustom twittering-show-replied-tweets t
  "*The number of replied tweets which will be showed in one tweet.

If the value is not a number and is non-nil, show all replied tweets
which is already fetched.
If the value is nil, doesn't show replied tweets."

  :type '(choice (const :tag "Do not show replied tweets"
			:value nil)
		 (const :tag "Show all replied tweets"
			:value t)
		 (integer :tag "Number of replied tweet"))
  :group 'posting-station)

(defcustom twittering-default-show-replied-tweets nil
  "*The number of default replied tweets which will be shown in one tweet.
This value will be used only when showing new tweets.

See `twittering-show-replied-tweets' for more details."
  :type '(choice (const nil)
		 integer)
  :group 'posting-station)

(defcustom twittering-disable-overlay-on-too-long-string nil
  "*If non-nil, disable overlay on too long string on edit buffer.

If nil, `twittering-edit-mode' puts an overlay `twittering-warning-overlay' on
characters exceeding the maximum length.

On some environments, some input methods seem to interfere the update of the
overlay.  In such case, you may avoid the problems by setting this variable to
non-nil."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)"
  :type 'boolean
  :group 'posting-station)

(defvar twittering-notify-successful-http-get t)

(defvar twittering-format-status-function-source ""
  "The status format string that has generated the current `twittering-format-status-function'.")
(defvar twittering-format-status-function nil
  "The formating function generated from `twittering-format-status-function-source'.")
(defvar twittering-format-status-function-without-compile nil
  "The formating function generated from \
`twittering-format-status-function-source',
which is a lambda expression without being compiled.")

(defvar twittering-timeline-data-table (make-hash-table :test 'equal))

(defcustom twittering-username-face 'twittering-username-face
  "*Face used to display USERNAME."
  :type 'face
  :group 'posting-station)

(defcustom twittering-uri-face 'twittering-uri-face
  "*Face used to display URIs."
  :type 'face
  :group 'posting-station)

(defcustom twittering-use-native-retweet nil
  "*If non-nil, post retweet using native retweets."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-update-status-function
  'twittering-update-status-from-pop-up-buffer
  "*The function which is used to post a tweet.

It takes the following 5 arguments, INIT-STR, REPLY-TO-ID, USERNAME,
TWEET-TYPE and CURRENT-SPEC.
The first argument INIT-STR is nil or an initial text to be edited.
REPLY-TO-ID and USERNAME are an ID and a user-screen-name of a tweet to
which you are going to reply.  If the tweet is not a reply, they are nil.
TWEET-TYPE is a symbol specifying a type of a tweet being edited.  It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
CURRENT-SPEC means on which timeline the function is called.

posting-station provides two functions for updating status:
* `twittering-update-status-from-minibuffer': edit tweets in minibuffer
* `twittering-update-status-from-pop-up-buffer': edit tweets in pop-up buffer"
  :type '(choice  (const :tag "built-in: from minibuffer"
			 twittering-update-status-from-minibuffer)
		  (const :tag "built-in: from a popup buffer"
			 twittering-update-status-from-pop-up-buffer)
		  (function :tag "Your own function"))
  :group 'posting-station)

(defcustom twittering-request-confirmation-on-posting nil
  "*If non-nil, confirmation will be requested on posting a tweet \
edited in pop-up buffer."
  :type 'boolean
  :group 'posting-station)

(defcustom twittering-use-master-password nil
  "*If non-nil, store private information encrypted with a master password."
  :type 'boolean
  :group 'twittering)

(defcustom twittering-private-info-file (expand-file-name "~/.posting-station.gpg")
  "*File for storing encrypted private information.

 Only used when `twittering-use-master-password' is non-nil."
  :group 'posting-station
  :type 'file)

(defvar twittering-private-info-file-loaded nil
  "Whether the private info file has been loaded or not.")
(defvar twittering-variables-stored-with-encryption
  '(twittering-oauth-access-token-alist))

(defconst twittering-service-method-table
  '((twitter (status-url . twittering-get-status-url-twitter)
	     (search-url . twittering-get-search-url-twitter))
    (twitter-api-v1.1
     (status-url . twittering-get-status-url-twitter)
     (search-url . twittering-get-search-url-twitter))
    (statusnet (status-url . twittering-get-status-url-statusnet)
	       (search-url . twittering-get-search-url-statusnet)))
  "A list of alist of service methods.")

(defcustom twittering-service-method 'twitter-api-v1.1
  "*Service method for `posting-station'.

The symbol `twitter' means Twitter Service.
The symbol `statusnet' means StatusNet Service.

Default to `twitter-api-v1.1' which is an alias for `twitter'.

See also `twittering-service-method-table'."
  :type (if (> (length (mapcar #'car twittering-service-method-table)) 0)
	    `(choice ,@(mapcar (lambda (entry) `(const ,(car entry)))
			       twittering-service-method-table))
	  'symbol)
  :group 'posting-station)

(defcustom twittering-timeline-header-face 'twittering-timeline-header-face
  "*Face for the header on `posting-station'.

The face is used for rendering `twittering-timeline-header'."
  :type 'face
  :group 'posting-station)

(defcustom twittering-timeline-footer-face 'twittering-timeline-footer-face
  "*Face for the footer on `posting-station'.

The face is used for rendering `twittering-timeline-footer'."
  :type 'face
  :group 'posting-station)

(defcustom twittering-timeline-header "-- Press Enter here to update --\n"
  "*Timeline header string on `posting-station'.

The string is rendered on the beginning of a `posting-station' buffer.
Its face is specified by `twittering-timeline-header-face'."
  :type 'string
  :group 'posting-station)

(defcustom twittering-timeline-footer "-- Press Enter here to update --"
  "*Timeline footer string on `posting-station'.

The string is rendered on the end of a `posting-station' buffer.
Its face is specified by `twittering-timeline-footer-face'."
  :type 'string
  :group 'posting-station)

(defcustom twittering-pop-to-buffer-function
  'twittering-pop-to-buffer-in-bottom-largest-window
  "*Function being invoked by `twittering-pop-to-buffer'.

It will receive an argument, the buffer being selected.
For example, the following functions can be used; `pop-to-buffer',
`twittering-pop-to-buffer-simple',
`twittering-pop-to-buffer-in-current-window',
`twittering-pop-to-buffer-in-largest-window', and
`twittering-pop-to-buffer-in-bottom-largest-window'."
  :type 'function
  :group 'posting-station)

;; FIXME: change to something better than alist
(defcustom twittering-relative-retrieval-interval-alist
  '(("\\`:direct.*\\'" 4)
    (":home" ":mentions" 1)
    (t 1))
  "*An alist of relative intervals of retrieving timelines.
Each element looks like (TIMELINE-SPEC-REGEXP RELATIVE-INTERVAL).

TIMELINE-SPEC-REGEXP must be t or a regexp string specifying primary
timeline specs.
If TIMELINE-SPEC-REGEXP is t, it matches all timelines.
RELATIVE-INTERVAL must be zero or a positive integer specifying relative
interval of retrieving timelines that match TIMELINE-SPEC-REGEXP.

An interval for a timeline is determined as follows;
1. Find the first element where TIMELINE-SPEC-REGEXP matches the
   timeline or TIMELINE-SPEC-REGEXP is t.
   If no elements are found, the interval is `twittering-timer-interval'.
2. Check the RELATIVE-INTERVAL of the element.
   If RELATIVE-INTERVAL is a positive integer, the interval is
   RELATIVE-INTERVAL times as long as `twittering-timer-interval'.

   If RELATIVE-INTERVAL is zero, the interval is infinity.
   The timeline is not retrieved automatically."
  :type 'alist
  :group 'posting-station)

(defvar twittering-relative-retrieval-count-alist '()
  "An alist for counting retrieval of primary timelines.")

(defvar twittering-filter-alist '()
  "*An alist of hidden tweet patterns for each primary timeline.
Each element looks like:
 (TIMELINE-SPECIFIER (SYM1 . REGEXP1) (SYM2 . REGEXP2) ...).

TIMELINE-SPECIFIER must be a string or a list of strings.
Each string is a regexp for specifying primary timelines.
Note that you cannot specify composite timelines such as \":merge\",
\":exclude-if\" or \":exclude-re\".
Following regexps (REGEXP1, REGEXP2, ...) specify which tweet should
be hidden in a certain timeline.

In a timeline that matches TIMELINE-SPECIFIER, a tweet is hidden if
its elements specified by SYM1, SYM2, ... match corresponding REGEXP1, REGEXP2,
... respectively.

If a timeline matches multiple specifiers, all regexps of matched elements
are effective.

For example, if you specify
 '(((\":home\" \":mentions\") (text . \"http://\"))
   (\"^[^:]\" (text . \"sample\") (user-screen-name . \"\\`FOO\\'\"))
   (\"twitter/.*\" (text . \"^aa\"))),
the following tweets are hidden.

- tweets including \"http://\" in the home timeline and the mentions timeline,
- tweets that are posted by the user FOO and include \"sample\"
  in user timelines and list timelines,
- tweets including \"aa\" at a beginning of a line in list timelines of
  twitter, such as \"twitter/media\" or \"twitter/support\".")


;;;;
;;;; Macro and small utility function
;;;;

(defmacro twittering-list-push (value listvar)
  "Add VALUE to the start of LISTVAR."
  `(setq ,listvar (cons ,value ,listvar)))

(defmacro twittering-case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

(defmacro twittering-wait-while (timeout interval condition &optional form &rest timeout-forms)
  "Wait while CONDITION returns non-nil until TIMEOUT seconds passes.

The form CONDITION is repeatedly evaluated for every INTERVAL seconds
until CONDITION returns nil or TIMEOUT seconds passes unless TIMEOUT is nil.
If TIMEOUT is nil, there is no time limit.

If CONDITION returns nil, evaluate the form FORM and return its value.
If TIMEOUT seconds passes, evaluate the forms TIMEOUT-FORMS and return
the value of the last form in TIMEOUT-FORMS."
  `(lexical-let (,@(when timeout `((timeout ,timeout)))
		 (interval ,interval)
		 (current 0.0))
     (while (and ,@(when timeout '((< current timeout)))
		 ,condition)
       (sleep-for interval)
       (setq current (+ current interval)))
     ,(when (or form timeout-forms)
	(if (null timeout)
	    form
	  `(if (< current timeout)
	       ,form
	     ,@timeout-forms)))))


(defun twittering-extract-matched-substring-all (regexp str)
  (let ((pos 0)
	(result nil))
    (while (string-match regexp str pos)
      (setq result (cons (match-string 1 str) result))
      (setq pos (match-end 0)))
    (reverse result)))

(defun twittering-process-alive-p (proc)
  "Return non-nil if PROC is alive."
  (not (memq (process-status proc) '(nil closed exit failed signal))))

(defun twittering-start-process-with-sentinel (name buffer program args sentinel)
  "Start a subprocess NAME in BUFFER running PROGRAM with ARGS with a sentinel.

This function is the same as `start-process' except that SENTINEL must
be invoked when the process is successfully started."
  (let ((proc (apply 'start-process name buffer program args)))
    (when (and proc (functionp sentinel))
      (if (twittering-process-alive-p proc)
	  (set-process-sentinel proc sentinel)
	;; Ensure that the sentinel is invoked if a subprocess is
	;; successfully started.
	(funcall sentinel proc "finished")))
    proc))

(defun twittering-parse-time-string (str &optional round-up)
  "Parse the time-string STR into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
This function is the same as `parse-time-string' except to complement the
lacked parameters with the current time.
If ROUND-UP is nil, complement the lacked parameters with the oldest ones.
If ROUND-UP is non-nil, complement the lacked parameters with the latest ones.
For example, (twittering-parse-time-string \"2012-04-20\")
returns (0 0 0 20 4 2012 nil nil 32400).
And (twittering-parse-time-string \"2012-04-20\" t)
returns (59 59 23 20 4 2012 nil nil 32400).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil."
  (let* ((parsed (parse-time-string str))
	 (current (decode-time (current-time)))
	 (replacement-alist
	  `((SEC . ,(if round-up
			59
		      0))
	    (MIN . ,(if round-up
			59
		      0))
	    (HOUR . ,(if round-up
			 23
		       0))
	    (DAY . nil)
	    (MON . nil)
	    (YEAR . nil)
	    (DOW . nil)
	    (DST . nil)
	    (TZ . nil)))
	 (sym-list (mapcar 'car replacement-alist))
	 (result nil))
    (while (and parsed current sym-list)
      (let* ((sym (car sym-list))
	     (v (or (car parsed)
		    (cdr (assq sym replacement-alist))
		    ;; If `sym' is not 'DOW and it is bound to nil
		    ;; in `replacement-alist', use `current'.
		    (unless (eq sym 'DOW)
		      (car current)))))
	(setq result (cons v result)))
      (setq parsed (cdr parsed))
      (setq current (cdr current))
      (setq sym-list (cdr sym-list)))
    (reverse result)))

(defun twittering-normalize-string (str)
  (if (require 'ucs-normalize nil t)
      (ucs-normalize-NFC-string str)
    str))

;;;;
;;;; Utility for portability
;;;;

(defun twittering-remove-duplicates (list)
  "Return a copy of LIST with all duplicate elements removed.
This is non-destructive version of `delete-dups' which is not
defined in Emacs21."
  (if (fboundp 'delete-dups)
      (delete-dups (copy-sequence list))
    (let ((rest list)
	  (result nil))
      (while rest
	(unless (member (car rest) result)
	  (setq result (cons (car rest) result)))
	(setq rest (cdr rest)))
      (nreverse result))))

(defun twittering-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
This is a modified version of `completing-read' and accepts candidates
as a list of a string on Emacs21."
  ;; completing-read() of Emacs21 does not accepts candidates as
  ;; a list. Candidates must be given as an alist.
  (let* ((collection (twittering-remove-duplicates collection))
	 (collection
	  (if (and (> 22 emacs-major-version)
		   (listp collection)
		   (stringp (car collection)))
	      (mapcar (lambda (x) (cons x nil)) collection)
	    collection)))
    (completing-read prompt collection predicate require-match
		     initial-input hist def inherit-input-method)))

(defun twittering-add-to-history (history-var elt &optional maxelt keep-all)
  (if (functionp 'add-to-history)
      (add-to-history history-var elt maxelt keep-all)
    (let* ((added (cons elt
			(if (and (not keep-all)
				 (boundp 'history-delete-duplicates)
				 history-delete-duplicates)
			    (delete elt (symbol-value history-var))
			  (symbol-value history-var))))
	   (maxelt (or maxelt history-length))
	   (len (length added)))
      (set history-var
	    (if (<= len maxelt)
		added
	      (butlast added (- len maxelt)))))))

(if (fboundp 'assoc-string)
    (defalias 'twittering-assoc-string 'assoc-string)
  (defun twittering-assoc-string (key list &optional case-fold)
    "Like `assoc' but specifically for strings (and symbols).
This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, case
is ignored.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.

This is reimplemented version of `assoc-string' which is not
defined in Emacs21."
    (let* ((key (if (stringp key)
		    key
		  (symbol-name key)))
	   (regexp (concat "\\`" key "\\'"))
	   (rest list)
	   (result nil)
	   (case-fold-search case-fold))
      (while (not (null rest))
	(let* ((current (car rest))
	       (current-key
		(if (listp current)
		    (car current)
		  current))
	       (current-key
		(if (stringp current-key)
		    current-key
		  (symbol-name current-key))))
	  (if (string-match key current-key)
	      (setq result current
		    rest nil)
	    (setq rest (cdr rest)))))
      result)))

;;;;
;;;; Debug mode
;;;;

(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer "*debug*")

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun twittering-debug-buffer ()
  (twittering-get-or-generate-buffer twittering-debug-buffer))

(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twittering-debug-mode
	   (with-current-buffer (twittering-debug-buffer)
	     (insert "[debug] " (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun debug-printf (fmt &rest args)
  (when twittering-debug-mode
    (with-current-buffer (twittering-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
	(not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

;;;;
;;;; Private storage
;;;;

(defun twittering-private-info-loaded-p ()
  twittering-private-info-file-loaded)

(defun twittering-load-private-info ()
  (let* ((file twittering-private-info-file)
	 (decrypted-str (twittering-read-from-encrypted-file file))
	 (loaded-alist
	  (when decrypted-str
	    (condition-case nil
		(read decrypted-str)
	      (error
	       nil)))))
    (when loaded-alist
      (remove
       nil
       (mapcar
	(lambda (pair)
	  (when (consp pair)
	    (let ((sym (car pair))
		  (value (cdr pair)))
	      (cond
	       ((memq sym twittering-variables-stored-with-encryption)
		(set sym value)
		sym)
	       (t
		nil)))))
	loaded-alist)))))

(defun twittering-load-private-info-with-guide ()
  (let ((str (concat
	      "Loading authorized access token for OAuth from\n"
	      (format "%s.\n" twittering-private-info-file)
	      "\n"
	      (propertize "Please input the master password.\n" 'face 'bold)
	      "\n"
	      "To cancel it, you may need to press C-g multiple times.\n"
	      )))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twittering-load-private-info)))))

(defun twittering-save-private-info ()
  (let* ((obj (mapcar (lambda (sym)
			`(,sym . ,(symbol-value sym)))
		      twittering-variables-stored-with-encryption))
	 (str (with-output-to-string (pp obj)))
	 (file twittering-private-info-file))
    (when (twittering-write-and-encrypt file str)
      (set-file-modes file #o600)
      (setq twittering-private-info-file-loaded t))))

(defun twittering-save-private-info-with-guide ()
  (let ((str (concat
	      "Saving authorized access token for OAuth to "
	      (format "%s.\n" twittering-private-info-file)
	      "\n"
	      (propertize "Please input a master password twice."
			  'face 'bold))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twittering-save-private-info)))))

(defun twittering-capable-of-encryption-p ()
  (and (or (require 'epa nil t) (require 'alpaca nil t))
       (or (executable-find "gpg") (executable-find "gpg2"))))

(eval-when-compile
  (require 'epa nil t)
  (require 'alpaca nil t))
(defun twittering-read-from-encrypted-file (file)
  "Decrypt contents from FILE and return them.
Read encrypted contents from FILE and return the decrypted contents.
This function requires `epa' or `alpaca' library."
  (cond
   ((not (file-readable-p file))
    (error "Failed to read %s" file)
    nil)
   ((require 'epa nil t)
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context
       (cons #'epa-progress-callback-function
	     (format "Decrypting %s..." (file-name-nondirectory file))))
      (message "Decrypting %s..." (file-name-nondirectory file))
      (condition-case err
	  (let ((full-path (expand-file-name file)))
	    ;; `epg-decrypt-file' included in EasyPG 1.0.0, which is
	    ;; distributed with Emacs 23.2, requires the expanded full path
	    ;; as the argument CIPHER. This is because CIPHER is directly
	    ;; used as an argument of the command `gpg'.
	    (epg-decrypt-file context full-path nil))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    (with-temp-buffer
      (let ((buffer-file-name (expand-file-name file))
	    (alpaca-regex-suffix ".*")
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (temp-buffer (current-buffer))
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
	(insert-file-contents-literally file)
	(set-buffer-modified-p nil)
	(condition-case nil
	    (progn
	      (alpaca-after-find-file)
	      (if (eq temp-buffer (current-buffer))
		  (buffer-string)
		;; `alpaca-after-find-file' kills the current buffer
		;; if the decryption is failed.
		nil))
	  (error
	   (when (eq temp-buffer (current-buffer))
	     (delete-region (point-min) (point-max)))
	   nil)))))
   (t
    nil)))

(defun twittering-write-and-encrypt (file str)
  (cond
   ((require 'epg nil t)
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context (cons #'epa-progress-callback-function "Encrypting..."))
      (message "Encrypting...")
      (condition-case err
	  (unwind-protect
	      ;; In order to prevent `epa-file' to encrypt the file double,
	      ;; `epa-file-name-regexp' is temorarily changed into the null
	      ;; regexp that never matches any string.
	      (let ((epa-file-name-regexp "\\`\\'")
		    (coding-system-for-read 'binary)
		    (coding-system-for-write 'binary))
		(when (fboundp 'epa-file-name-regexp-update)
		  (epa-file-name-regexp-update))
		(with-temp-file file
		  (set-buffer-multibyte nil)
		  (delete-region (point-min) (point-max))
		  (insert (epg-encrypt-string context str nil))
		  (message "Encrypting...wrote %s" file)
		  t))
	    (when (fboundp 'epa-file-name-regexp-update)
	      (epa-file-name-regexp-update)))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    ;; Create the file.
    ;; This is required because `alpaca-save-buffer' checks its timestamp.
    (with-temp-file file)
    (with-temp-buffer
      (let ((buffer-file-name file)
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
	(insert str)
	(condition-case nil
	    (if (alpaca-save-buffer)
		t
	      (delete-file file)
	      nil)
	  (error
	   (when (file-exists-p file)
	     (delete-file file))
	   nil)))))
   (t
    nil)))

(defun twittering-ensure-private-info ()
  "Ensure that private information is loaded if necessary.
Return non-nil if `twittering-use-master-password' is nil or private
information has been already loaded. Also, return non-nil
if `twittering-use-master-password' is non-nil and this function succeeded
in loading private information.
Return nil if private information cannot be loaded."
  (if (or (not twittering-use-master-password)
	  (twittering-private-info-loaded-p))
      ;; The private information is unnecessary or already loaded.
      t
    (cond
     ((not (twittering-capable-of-encryption-p))
      (message "You need GnuPG and (EasyPG or alpaca.el) for master password!")
      nil)
     ((and (memq twittering-auth-method '(oauth xauth))
	   (file-exists-p twittering-private-info-file))
      (cond
       ((twittering-load-private-info-with-guide)
	(setq twittering-private-info-file-loaded t)
	(message "The authorized token is loaded.")
	t)
       (t
	(message "Failed to load an authorized token from \"%s\"."
		 twittering-private-info-file)
	nil)))
     (t
      ;; The file for private infomation does not exist now.
      t))))

;;;;
;;;; Window configuration
;;;;

(defun twittering-set-window-end (window pos)
  (let* ((height (window-text-height window))
	 (n (- (- height 1))))
    (while (progn (setq n (1+ n))
		  (set-window-start
		   window
		   (with-current-buffer (window-buffer window)
		     (save-excursion
		       (goto-char pos)
		       (line-beginning-position n))))
		  (not (pos-visible-in-window-p pos window))))))

(defun twittering-current-window-config (window-list)
  "Return window parameters of WINDOW-LIST."
  (mapcar (lambda (win)
	    (let ((start (window-start win))
		  (point (window-point win)))
	      `(,win ,start ,point)))
	  window-list))

(defun twittering-restore-window-config-after-modification (config beg end)
  "Restore window parameters changed by modification on given region.
CONFIG is window parameters made by `twittering-current-window-config'.
BEG and END mean a region that had been modified."
  (mapc (lambda (entry)
	  (let ((win (elt entry 0))
		(start (elt entry 1))
		(point (elt entry 2)))
	    (when (and (< beg start) (< start end))
	      (set-window-start win start))
	    (when (and (< beg point) (< point end))
	      (set-window-point win point))))
	config))

(defun twittering-pop-to-buffer (buf)
  "Select the buffer BUF in some window.
The behavior is determined by the function specified by
`twittering-pop-to-buffer-function'."
  (funcall twittering-pop-to-buffer-function buf))

(defun twittering-pop-to-buffer-simple (buf)
  "Select the buffer BUF by using `pop-to-buffer'."
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    ;; This is required because the new window generated by `pop-to-buffer'
    ;; may hide the region following the current position.
    (twittering-ensure-whole-of-status-is-visible win)))

(defun twittering-pop-to-buffer-in-current-window (buf &optional win)
  "Select the buffer BUF in the window WIN by splitting it.
If WIN is nil, the selected window is splitted."
  (let* ((win (or win (selected-window)))
	 (size
	  (let ((rest (- (window-height win) 15)))
	    (if (<= rest 3)
		;; To avoid an error due to a too small window.
		nil
	      rest)))
	 (new-win (split-window win size)))
    (select-window new-win)
    (switch-to-buffer buf)))

(defun twittering-pop-to-buffer-in-largest-window (buf)
  "Select the buffer BUF in the largest window by splitting it."
  (let ((win
	 (lexical-let ((max-area 0)
		       (largest-win nil))
	   (walk-windows
	    (lambda (win)
	      (let ((area (* (window-height win) (window-width win))))
		(when (< max-area area)
		  (setq max-area area)
		  (setq largest-win win)))))
	   largest-win)))
    (twittering-pop-to-buffer-in-current-window buf win)))

(defun twittering-pop-to-buffer-in-bottom-largest-window (buf)
  "Select the buffer BUF in the window largest on bottom by splitting it."
  (let* ((bottom-win-list
	  (lexical-let ((win-list '())
			(max-bottom 0))
	    (walk-windows
	     (lambda (win)
	       (let ((bottom (nth 3 (window-edges win))))
		 (cond
		  ((< max-bottom bottom)
		   (setq max-bottom bottom)
		   (setq win-list `(,win)))
		  ((= max-bottom bottom)
		   (setq win-list (cons win win-list)))
		  (t
		   nil)))))
	    win-list))
	 (win
	  (lexical-let ((max-area 0)
			(largest-win nil))
	    (mapc (lambda (win)
		    (let ((area (* (window-height win) (window-width win))))
		      (when (< max-area area)
			(setq largest-win win)
			(setq max-area area))))
		  bottom-win-list)
	    largest-win)))
    (twittering-pop-to-buffer-in-current-window buf win)))

;;;;
;;;; Filter
;;;;

(defun twittering-get-filter-list-for-timeline-spec-string (spec-string)
  (let ((entry-list twittering-filter-alist))
    (remove
     nil
     (mapcar
      (lambda (entry)
	(let ((spec-regexp
	       (if (listp (car entry))
		   (concat "\\(?:"
			   (mapconcat 'identity (car entry) "\\|")
			   "\\)")
		 (car entry)))
	      (pattern-list (cdr entry)))
	  (when (string-match spec-regexp spec-string)
	    pattern-list)))
      entry-list))))

(defun twittering-get-filter-list-for-timeline-spec (spec)
  (when twittering-filter-alist
    (let* ((spec-string (twittering-timeline-spec-to-string spec))
	   (short-spec-string (twittering-timeline-spec-to-string spec t))
	   (regexp-list
	    (twittering-get-filter-list-for-timeline-spec-string
	     spec-string)))
      (if (string= spec-string short-spec-string)
	  regexp-list
	(append regexp-list
		(twittering-get-filter-list-for-timeline-spec-string
		 short-spec-string))))))

(defun twittering-match-pattern (status pattern)
  (let* ((rest pattern)
	 (matched t))
    (while (and rest matched)
      (let* ((current (car rest))
	     (sym (car current))
	     (regexp (cdr current))
	     (value (cdr (assq sym status)))
	     (value
	      (if (eq sym 'text)
		  (twittering-make-fontified-tweet-text-with-entity status)
		value)))
	(unless (and (stringp value)
		     (string-match regexp value))
	  (setq matched nil))
	(setq rest (cdr rest))))
    matched))

(defun twittering-match-pattern-list (status pattern-list)
  (let* ((rest pattern-list)
	 (matched nil))
    (while (and rest (not matched))
      (let ((current (car rest)))
	(when (twittering-match-pattern status current)
	  (setq matched t))
	(setq rest (cdr rest))))
    matched))

;;;;
;;;; Retrieved statuses (timeline data)
;;;;

(defun twittering-current-timeline-id-table (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
	(elt (gethash spec twittering-timeline-data-table) 0)
      nil)))

(defun twittering-current-timeline-referring-id-table (&optional spec)
  "Return the hash from a ID to the ID of the first observed status
referring the former ID."
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((memq type '(exclude-if exclude-re merge))
      ;; Use the first non-nil table instead of merging the all tables
      ;; because it may take a long time to merge them.
      (car
       (remove
	nil
	(mapcar (lambda (base-spec)
		  (elt (gethash base-spec twittering-timeline-data-table) 1))
		(twittering-get-primary-base-timeline-specs spec)))))
     ((eq type 'single)
      ;; Single tweet timelines are registered in a special way.
      ;; See `twittering-retrieve-single-tweet-sentinel'.
      (elt (gethash '(:single) twittering-timeline-data-table) 1))
     (t
      (elt (gethash spec twittering-timeline-data-table) 1)))))

(defun twittering-current-timeline-data (&optional spec)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((eq type 'single)
      (let* ((id (cadr spec))
	     (status (twittering-find-status id)))
	(if status
	    `(,status)
	  nil)))
     ((memq type '(exclude-if exclude-re merge))
      (let ((primary-base-specs
	     (twittering-get-primary-base-timeline-specs spec)))
	(sort
	 (apply
	  'append
	  (mapcar
	   (lambda (primary-spec)
	     ;; `copy-sequence' is required to prevent `sort'
	     ;; from modifying lists of statuses in the database
	     ;; `twittering-timeline-data-table'.
	     ;; The result of `twittering-generate-composite-timeline'
	     ;; may include a list in the database. If so, the simply
	     ;; appended list include it as a tail.
	     (copy-sequence
	      (twittering-generate-composite-timeline
	       spec
	       primary-spec (twittering-current-timeline-data primary-spec))))
	   primary-base-specs))
	 (lambda (status1 status2)
	   (let ((id1 (cdr (assq 'id status1)))
		 (id2 (cdr (assq 'id status2))))
	     (twittering-status-id< id2 id1))))))
     ((eq type :single)
      ;; The timeline spec '(:single) does not correspond to an ordinary
      ;; timeline. It means an unordered set of tweets retrieved by the
      ;; 'retrieve-single-tweet command of `twittering-call-api'.
      ;; If this function is used with the spec '(:single), a specific tweet
      ;; will be required with the user's intention.
      ;; In this case, exclusion by patterns does not required.
      (elt (gethash spec twittering-timeline-data-table) 2))
     (t
      (let ((statuses (elt (gethash spec twittering-timeline-data-table) 2))
	    (pattern-list
	     (twittering-get-filter-list-for-timeline-spec spec)))
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
	      statuses))
	  statuses))))))

(defun twittering-find-status (id)
  (let ((result nil))
    (maphash
     (lambda (spec pair)
       (let* ((id-table (car pair))
	      (entry (gethash id id-table)))
	 ;; Take the most detailed status.
	 (when (and entry
		    (or (null result) (< (length result) (length entry))))
	   (setq result entry))))
     twittering-timeline-data-table)
    result))

(defun twittering-delete-status-from-data-table (id)
  (let ((modified-spec nil))
    (maphash
     (lambda (spec data)
       (let* ((id-table (elt data 0))
	      (referring-id-table (elt data 1))
	      (timeline-data (elt data 2))
	      (status (gethash id id-table)))
	 (when status
	   (remhash id id-table)
	   ;; Here, `referring-id-table' is not modified.
	   ;; Therefore, the retweet observed secondly will not appear even
	   ;; if the retweet observed first for the same tweet is deleted.
	   (setq modified-spec
		 (cons `(,spec
			 ,id-table
			 ,referring-id-table
			 ,(remove status timeline-data))
		       modified-spec)))))
     twittering-timeline-data-table)
    (mapc
     (lambda (spec)
       (let ((buffer (twittering-get-buffer-from-spec spec)))
	 (when (buffer-live-p buffer)
	   (with-current-buffer buffer
	     (save-excursion
	       (twittering-for-each-property-region
		'id
		(lambda (beg end value)
		  (when (twittering-status-id= id value)
		    (let ((buffer-read-only nil)
			  (separator-pos (min (point-max) (1+ end))))
		      (delete-region beg separator-pos)
		      (goto-char beg))))
		buffer))))))
     (twittering-remove-duplicates
      (apply 'append
	     (mapcar
	      (lambda (data)
		(let ((spec (car data)))
		  ;; Update the entry for `spec' in
		  ;; `twittering-timeline-data-table' with the new
		  ;; timeline-data that does not include `status'.
		  (puthash spec (cdr data) twittering-timeline-data-table)
		  (twittering-get-dependent-timeline-specs spec)))
	      modified-spec))))))

(defun twittering-get-replied-statuses (id &optional count)
  "Return a list of replied statuses starting from the status specified by ID.
Statuses are stored in ascending-order with respect to their IDs."
  (let ((result nil)
	(status (twittering-find-status id)))
    (while
	(and (if (numberp count)
		 (<= 0 (setq count (1- count)))
	       t)
	     (let ((replied-id (or (cdr (assq 'in-reply-to-status-id status))
				   "")))
	       (unless (string= "" replied-id)
		 (let ((replied-status (twittering-find-status replied-id)))
		   (when replied-status
		     (setq result (cons replied-status result))
		     (setq status replied-status)
		     t))))))
    result))

(defun twittering-have-replied-statuses-p (id)
  (let ((status (twittering-find-status id)))
    (when status
      (let ((replied-id (cdr (assq 'in-reply-to-status-id status))))
	(and replied-id (not (string= "" replied-id)))))))

(defun twittering-add-statuses-to-timeline-data (statuses &optional spec)
  "Add STATUSES as new statuses for SPEC and update derived timelines.
The function returns a list of lists including an updated timeline spec
string and the number of new statuses for the timeline."
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (id-table
	  (or (twittering-current-timeline-id-table spec)
	      (make-hash-table :test 'equal)))
	 (referring-id-table
	  (or (twittering-current-timeline-referring-id-table spec)
	      (make-hash-table :test 'equal)))
	 (timeline-data (twittering-current-timeline-data spec)))
    (let* ((new-statuses
	    (remove nil
		    (mapcar
		     (lambda (status)
		       (let ((id (cdr (assq 'id status)))
			     (retweeted-id (cdr (assq 'retweeted-id status))))
			 (unless (or (not retweeted-id)
				     (gethash retweeted-id referring-id-table))
			   ;; Store the id of the first observed tweet
			   ;; that refers `retweeted-id'.
			   (puthash retweeted-id id referring-id-table))
			 (if (gethash id id-table)
			     nil
			   (puthash id status id-table)
			   (puthash id id referring-id-table)
			   `((source-spec . ,spec)
			     ,@status))))
		     statuses)))
	   (new-statuses
	    ;; Sort tweets by ID.
	    ;; This is necessary because `twittering-render-timeline' assumes
	    ;; that given tweets are ordered.
	    (sort new-statuses
		  (lambda (status1 status2)
		    (let ((id1 (cdr (assq 'id status1)))
			  (id2 (cdr (assq 'id status2))))
		      (twittering-status-id< id2 id1))))))
      (when new-statuses
	(let ((new-timeline-data
	       (sort (append new-statuses timeline-data)
		     (lambda (status1 status2)
		       (let ((id1 (cdr (assq 'id status1)))
			     (id2 (cdr (assq 'id status2))))
			 (twittering-status-id< id2 id1))))))
	  (puthash spec `(,id-table ,referring-id-table ,new-timeline-data)
		   twittering-timeline-data-table))
	(let ((twittering-new-tweets-spec spec)
	      (twittering-new-tweets-statuses new-statuses)
	      (twittering-new-tweets-count (length new-statuses)))
	  (run-hooks 'twittering-new-tweets-hook)))
      ;; Update timelines derived from SPEC and return the number of
      ;; new tweets for each updated timeline.
      (remove
       nil
       (mapcar
	(lambda (buffer)
	  (let ((other-spec (twittering-get-timeline-spec-for-buffer buffer))
		(other-spec-string
		 (twittering-get-timeline-spec-string-for-buffer buffer)))
	    (when (twittering-timeline-spec-depending-on-p other-spec spec)
	      (let* ((twittering-new-tweets-spec other-spec)
		     (twittering-new-tweets-statuses
		      (twittering-generate-composite-timeline
		       other-spec spec new-statuses))
		     (twittering-new-tweets-count
		      (length twittering-new-tweets-statuses))
		     (rendered-tweets
		      (twittering-render-timeline
		       buffer twittering-new-tweets-statuses t)))
		(when rendered-tweets
		  (when (not (equal spec other-spec))
		    ;; The hook has been alreadly invoked for `spec'.
		    (run-hooks 'twittering-new-tweets-hook))
		  `(,other-spec-string ,(length rendered-tweets)))))))
	(twittering-get-buffer-list))))))

;;;;
;;;; URIs related to a tweet
;;;;

(defun twittering-get-status-url (username &optional id)
  "Generate a URL of a user or a specific status."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table)))))
    (funcall func username id)))

(defun twittering-get-status-url-from-alist (status)
  "Generate a URL of a tweet specified by an alist STATUS."
  (let ((username (cdr (or (assq 'retweeted-user-screen-name status)
			   (assq 'user-screen-name status))))
	(id (cdr (or (assq 'retweeted-id status)
		     (assq 'id status))))
	(func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table)))))
    (funcall func username id)))

(defun twittering-get-list-url (username listname)
  "Generate a URL of a specific list."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table))))
	(str (concat username "/" listname)))
    (funcall func str nil)))

(defun twittering-get-status-url-twitter (username &optional id)
  "Generate status URL for Twitter."
  (if id
      (format "http://%s/%s/status/%s" twittering-web-host username id)
    (format "http://%s/%s" twittering-web-host username)))

(defun twittering-get-status-url-statusnet (username &optional id)
  "Generate status URL for StatusNet."
  (if id
      (format "http://%s/%s/notice/%s" twittering-web-host twittering-web-path-prefix id)
    (format "http://%s/%s/%s" twittering-web-host twittering-web-path-prefix username)))

(defun twittering-get-search-url (query-string)
  "Generate a URL for searching QUERY-STRING."
  (let ((func (cdr (assq
		    'search-url (assq twittering-service-method
				      twittering-service-method-table)))))
    (funcall func query-string)))

(defun twittering-get-search-url-twitter (query-string)
  (format "http://%s/search?q=%s"
	  twittering-web-host (twittering-percent-encode query-string)))

(defun twittering-get-search-url-statusnet (query-string)
  (if (string-match "^#\\(.+\\)" query-string)
      (format "http://%s/%s/tag/%s"
	      twittering-web-host
	      twittering-web-path-prefix
	      (twittering-percent-encode (match-string 1 query-string)))
    (format "http://%s/search?q=%s"
	    twittering-web-host (twittering-percent-encode query-string))))

(defun twittering-extract-id-from-url (url-string)
  "Extract the ID from URL-STRING.
Return nil if URL-STRING cannot be interpreted as a URL pointing a tweet."
  (when (string-match
	 "\\`https?://twitter.com/\\(?:#!/\\)?[^/]+/status\\(?:es\\)?/\\([0-9]+\\)/?\\'"
	 url-string)
    (match-string 1 url-string)))

;;;;
;;;; Utility of status IDs
;;;;

(defun twittering-status-id< (id1 id2)
  (let ((len1 (length id1))
	(len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twittering-status-id= (id1 id2)
  (equal id1 id2))

(defun twittering-snowflake-epoch-time ()
  "Return the epoch time of Snowflake."
  (require 'calc)
  (let ((epoch-str
	 ;; This corresponds to 2010-11-04 01:42:54+00:00 in RFC3339.
	 ;; The value comes from the following page.
	 ;; https://github.com/twitter/snowflake/blob/6d4634aa490de26e22425538291fe0a03071a170/src/main/scala/com/twitter/service/snowflake/IdWorker.scala#L22
	 ;; 22    val twepoch = 1288834974657L
	 "1288834974657"))
    (let ((str
	   (calc-eval `(,(concat "floor(10#" epoch-str "/10#1000)")
			calc-word-size 64 calc-number-radix 16)))
	  (milisec-str
	   (calc-eval `(,(concat "10#" epoch-str "%10#1000")
			calc-word-size 64 calc-number-radix 10))))
      (mapcar (lambda (s) (string-to-number s 16))
	      `(,(substring str 3 7) ,(substring str 7)
		,milisec-str)))))

(defun twittering-id-to-time (id)
  "Return the time corresonding to ID generated by Snowflake.
If ID is a string consisting of 12 or more digits, return the corresponding
time in Emacs internal representation the same as `encode-time'.
Otherwise, return nil.

This is because ID consisting of 11 or less digits may not be generated
by Snowflake."
  (require 'calc)
  (cond
   ((not (stringp id))
    nil)
   ((< (length id) 12)
    ;; The given ID may not be generated by Snowflake.
    ;;
    ;; The first tweet in the example response of
    ;; https://dev.twitter.com/docs/api/1/get/statuses/home_timeline
    ;; has "18700887835" as the ID.
    ;; Assuming that it is generated by Snowflake, it corresponds to
    ;; "2010-11-04 01:42:58+00:00".
    ;; However, the created_at is "Fri Jul 16 16:58:46 +0000 2010".
    ;; It can be confirmed at http://twitter.com/cindyli/status/18700887835 .
    ;;
    ;; Therefore we can not suppose that an ID is generated by Snowflake
    ;; if the ID consists of 11-digits.
    ;; Assuming that an ID reaches 10^11 before Snowflake is turned on at
    ;; Nov 04 2010, the average number of tweets per day will exceed
    ;; 533 million.
    ;; ( 10^11 - 18,700,887,835 > 80,000,000,000
    ;;   80 billion / 5 month (from Jul 16 to Nov 4) > 533 million / day )
    ;;
    ;; It is impossible.
    ;; So, I assume that an ID is generated by Snowflake if the ID consists of
    ;; 12 or more digits.
    ;; The first ID with 12-digits, 100,000,000,000 (10^11), corresponds to
    ;; 2010-11-04 01:43:17+00:00.
    nil)
   (t
    (let* ((epoch (twittering-snowflake-epoch-time))
	   (str (calc-eval
		 `(,(format "floor(rsh(10#%s,22)/1000)" id)
		   calc-word-size 64 calc-number-radix 16)))
	   (milisec
	    (string-to-number
	     (calc-eval
	      `(,(format "rsh(10#%s,22)%%1000" id)
		calc-word-size 64 calc-number-radix 10))
	     10)))
      (when (string-match "^16#\\([[:xdigit:]]+\\)" str)
	(let* ((hex-str (match-string 1 str))
	       (hex-str
		(if (< (length hex-str) 4)
		    (concat "0000" hex-str)
		  hex-str)))
	  (time-add epoch
		    `(,(string-to-number
			(substring hex-str 0 (- (length hex-str) 4)) 16)
		      ,(string-to-number
			(substring hex-str (- (length hex-str) 4)) 16)
		      ,(* milisec 1000)
		      ))))))))

(defun twittering-time-to-id (time)
  "Return the ID corresponding to TIME by Snowflake.
Bits other than timestamp are zero. The least significant 22 bits are zero.
TIME must be an Emacs internal representation as a return value of
`current-time'."
  (require 'calc)
  (let* ((epoch-time (twittering-snowflake-epoch-time))
	 (dt (if (time-less-p epoch-time time)
		 (time-subtract time epoch-time)
	       nil))
	 (sec-high (nth 0 dt))
	 (sec-low (nth 1 dt))
	 (microsec (or (nth 2 dt) "0")))
    (when dt
      (calc-eval
       `(,(format "lsh((16#%04x%04x) * 1000 + floor(%d/1000), 22)"
		  sec-high sec-low microsec)
	 calc-word-size 64 calc-number-radix 10)))))

;;;;
;;;; Process info
;;;;

(defun twittering-register-process (proc spec &optional str)
  (let ((str (or str (twittering-timeline-spec-to-string spec))))
    (add-to-list 'twittering-process-info-alist `(,proc ,spec ,str))))

(defun twittering-release-process (proc)
  (let ((pair (assoc proc twittering-process-info-alist)))
    (when pair
      (setq twittering-process-info-alist
	    (delq pair twittering-process-info-alist)))))

(defun twittering-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
	(elt entry 1)
      nil)))

(defun twittering-get-timeline-spec-string-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
	(elt entry 2)
      nil)))

(defun twittering-find-processes-for-timeline-spec (spec)
  (apply 'append
	 (mapcar
	  (lambda (pair)
	    (let ((proc (car pair))
		  (spec-info (cadr pair)))
	      (if (equal spec-info spec)
		  `(,proc)
		nil)))
	  twittering-process-info-alist)))

(defun twittering-remove-inactive-processes ()
  (let ((inactive-statuses '(nil closed exit failed signal)))
    (setq twittering-process-info-alist
	  (apply 'append
		 (mapcar
		  (lambda (pair)
		    (let* ((proc (car pair))
			   (info (cdr pair))
			   (status (process-status proc)))
		      (if (memq status inactive-statuses)
			  nil
			`((,proc ,@info)))))
		  twittering-process-info-alist)))))

(defun twittering-process-active-p (&optional spec)
  (twittering-remove-inactive-processes)
  (if spec
      (twittering-find-processes-for-timeline-spec spec)
    twittering-process-info-alist))

;;;;
;;;; Server info
;;;;

(defun twittering-update-api-table (spec api-string)
  "Register a pair of a timeline spec and an API for retrieving the timeline.
SPEC is a timeline spec. API-STRING is an identifier of an API for retrieving
the timeline."
  (let ((current (assoc spec twittering-timeline-spec-to-api-table)))
    (if (null current)
	(add-to-list 'twittering-timeline-spec-to-api-table
		     `(,spec . ,api-string))
      (setcdr current api-string))))

(defun twittering-make-rate-limit-alist (header-info)
  "Make a rate-limit information alist from HEADER-INFO.
Key symbols of a returned alist are following; limit, remaining, reset-time.
Values bound to limit and remaining is a positive integer and
one bound to reset-time is an Emacs time (result of `seconds-to-time')."
  (let ((symbol-table
	 '(("X-Rate-Limit-Limit" . limit)
	   ("X-Rate-Limit-Remaining" . remaining)
	   ("X-Rate-Limit-Reset" . reset-time)
	   ;; For Twitter API v1.0.
	   ("X-RateLimit-Limit" . limit)
	   ("X-RateLimit-Remaining" . remaining)
	   ("X-RateLimit-Reset" . reset-time))))
    (remove
     nil
     (mapcar (lambda (entry)
	       (let ((sym
		      (cdr
		       (twittering-assoc-string (car entry) symbol-table t))))
		 (cond
		  ((memq sym '(limit remaining))
		   `(,sym . ,(string-to-number (cdr entry))))
		  ((eq sym 'reset-time)
		   `(,sym
		     . ,(seconds-to-time (string-to-number (cdr entry)))))
		  (t
		   nil))))
	     header-info))))

(defun twittering-update-rate-limit-info (api-string spec header-info)
  "Register rate-limit information.
API-STRING is an identifier of an API. SPEC is a timeline spec that had been
retrieved by the API. HEADER-INFO is an alist generated from the HTTP response
header of the API."
  (let* ((api-string
	  (if (eq twittering-service-method 'twitter)
	      ;; The key for Twitter API v1.0 is nil.
	      nil
	    api-string))
	 (current (assoc api-string twittering-api-limit-info-alist))
	 (rate-limit-alist (twittering-make-rate-limit-alist header-info)))
    (twittering-update-api-table spec api-string)
    (if (null current)
	(add-to-list 'twittering-api-limit-info-alist
		     `(,api-string . ,rate-limit-alist))
      (setcdr current rate-limit-alist))))

(defun twittering-update-server-info (connection-info header-info)
  (let* ((new-entry-list (mapcar 'car header-info))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (account
	  (twittering-get-from-account-info "screen_name" account-info))
	 (spec (cdr (assq 'timeline-spec connection-info)))
	 (api-string
	  (cdr (assq 'uri-without-query (assq 'request connection-info)))))
    (twittering-update-rate-limit-info api-string spec header-info)
    (when (remove t (mapcar
		     (lambda (entry)
		       (equal (assoc entry header-info)
			      (assoc entry twittering-server-info-alist)))
		     new-entry-list))
      (setq twittering-server-info-alist
	    (append header-info
		    (remove nil (mapcar
				 (lambda (entry)
				   (if (member (car entry) new-entry-list)
				       nil
				     entry))
				 twittering-server-info-alist))))
      (when twittering-display-remaining
	(mapc (lambda (buffer)
		(with-current-buffer buffer
		  (twittering-update-mode-line)))
	      (twittering-get-buffer-list))))
    ;; cookie
    (let* ((new-cookies
	    (twittering-extract-cookie connection-info header-info))
	   (old-cookies (cdr (assoc account twittering-cookie-alist)))
	   (updated-cookies
	    (append new-cookies
		    (remove nil
			    (mapcar (lambda (cookie)
				      (unless (assoc (car cookie) new-cookies)
					cookie))
				    old-cookies)))))
      (setq twittering-cookie-alist
	    (cons (cons account updated-cookies)
		  (remove nil
			  (mapcar (lambda (entry)
				    (unless (equal account (car entry))
				      entry))
				  twittering-cookie-alist)))))
    header-info))

(defun twittering-get-ratelimit-alist (&optional spec)
  (let ((api-string
	 (cdr (assoc spec twittering-timeline-spec-to-api-table))))
    (cdr (assoc api-string twittering-api-limit-info-alist))))

(defun twittering-get-ratelimit-remaining (&optional spec)
  (or (cdr (assq 'remaining (twittering-get-ratelimit-alist spec)))
      0))

(defun twittering-get-ratelimit-limit (&optional spec)
  (or (cdr (assq 'limit (twittering-get-ratelimit-alist spec)))
      0))

(defun twittering-get-ratelimit-indicator-string (&optional spec)
  "Make an indicator string of rate-limit information of SPEC."
  (cond
   ((eq twittering-service-method 'twitter)
    ;; Twitter API v1.0.
    (format "%d/%d"
	    (twittering-get-ratelimit-remaining)
	    (twittering-get-ratelimit-limit)))
   (t
    (mapconcat
     (lambda (api-string)
       (let* ((alist (cdr (assoc api-string twittering-api-limit-info-alist)))
	      (remaining (cdr (assq 'remaining alist)))
	      (limit (cdr (assq 'limit alist))))
	 (format "%s/%s"
		 (if remaining (number-to-string remaining) "?")
		 (if limit (number-to-string limit) "?"))))
     (twittering-remove-duplicates
      (mapcar (lambda (spec)
		(cdr (assoc spec twittering-timeline-spec-to-api-table)))
	      (twittering-get-primary-base-timeline-specs spec)))
     "+"))))

;;;;
;;;; Service configuration
;;;;

(defconst twittering-service-configuration-default
  '((dm_text_character_limit . 10000)
    (short_url_length . 23)
    (short_url_length_https . 23))
  "Default value of `twittering-service-configuration'.")
(defvar twittering-service-configuration nil
  "Current server configuration.")
(defvar twittering-service-configuration-queried nil)
(defvar twittering-service-configuration-update-interval 86400
  "*Interval of updating `twittering-service-configuration'.")

(defun twittering-get-service-configuration (entry)
  (let ((pair (assq entry twittering-service-configuration)))
    (if (null pair)
	(cdr (assq entry twittering-service-configuration-default))
      (cdr pair))))

(defun twittering-update-service-configuration (&optional ignore-time)
  "Update `twittering-service-configuration' if necessary."
  (when (and
	 (memq twittering-service-method '(twitter twitter-api-v1.1))
	 (null twittering-service-configuration-queried)
	 (or ignore-time
	     (let ((current (twittering-get-service-configuration 'time))
		   (interval
		    (seconds-to-time
		     twittering-service-configuration-update-interval)))
	       (if (null current)
		   t
		 ;; If time passed more than `interval',
		 ;; update the configuration.
		 (time-less-p interval (time-since current))))))
    (setq twittering-service-configuration-queried t)
    (twittering-call-api
     'get-service-configuration
     '((sentinel . twittering-update-service-configuration-sentinel)
       (clean-up-sentinel
	. twittering-update-service-configuration-clean-up-sentinel)))))

(defun twittering-update-service-configuration-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (twittering-case-string
     status-code
     (("200")
      (let* ((conf-alist
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (mapcar
		   (lambda (entry)
		     `(,(car entry) . ,(elt entry 2)))
		   (cddr (assq 'configuration xml)))))
	       ((eq format 'json)
		(twittering-json-read))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (entries '(dm_text_character_limit
			short_url_length short_url_length_https)))
	(setq twittering-service-configuration
	      `((time . ,(current-time))
		,@(mapcar (lambda (entry)
			    (let ((value (cdr (assq entry conf-alist))))
			      (cons
			       entry
			       (cond
				((stringp value)
				 (string-to-number value))
				(t
				 value)))))
			  entries)))
	(setq twittering-service-configuration-queried nil)
	nil))
     (("400")
      ;; Rate limit exceeded.
      (setq twittering-service-configuration-queried nil)
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info)))
     (t
      (setq twittering-service-configuration-queried nil)
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-update-service-configuration-clean-up-sentinel (proc status connection-info)
  (when (not (twittering-process-alive-p proc))
    (setq twittering-service-configuration-queried nil)))

(defun twittering-get-maximum-message-length (&optional tweet-type)
  "Return the maximum message length of TWEET-TYPE.
If TWEET-TYPE is a symbol `direct-message', return the value of the
 service configuration `dm_text_character_limit'.
Otherwise, return 140."
  (let ((max-length
	 (if (eq tweet-type 'direct-message)
	     (twittering-get-service-configuration 'dm_text_character_limit)
	   140)))
    max-length))

;;;;
;;;; Mode-line
;;;;

;;; SSL
(defconst twittering-ssl-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * lock[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"                \",
\"                \",
\"    ........    \",
\"   ..######..   \",
\"   .##....##.   \",
\"   .#..  ..#.   \",
\"   .#.    .#.   \",
\"  ..#......#..  \",
\"  .##########.  \",
\"  .##########.  \",
\"  .####..####.  \",
\"  .###....###.  \",
\"  .###....###.  \",
\"  .##########.  \",
\"  ............  \",
\"                \"};
"
	    ))
  "Image for indicator of SSL state.")

(defconst posting-stationline-ssl
  (if twittering-ssl-indicator-image
      (propertize "SSL"
		  'display twittering-ssl-indicator-image
		  'help-echo "SSL is enabled.")
    "SSL"))

;;; ACTIVE/INACTIVE
(defconst twittering-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * connected[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"      .##. ...  \",
\"      .##. .#.  \",
\"      .##...#.. \",
\"      .##..###..\",
\"      .##..###..\",
\"      .##.#####.\",
\"  ... .##.#.#.#.\",
\"  .#. ..#...#...\",
\"...#...#.. .#.  \",
\".#.#.#.##. .#.  \",
\".#####.##. ...  \",
\"..### .##.      \",
\" .###..##.      \",
\" ..#...##.      \",
\"  .#. .##.      \",
\"  ... .##.      \"};
"))
  "Image for indicator of active state."
)

(defconst twittering-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * disconnected[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"      .##.      \",
\"      .##.      \",
\"    . .##.      \",
\"    ...##.      \",
\"     ..##.      \",
\"      ..#.      \",
\"    .. ...      \",
\"     .. ..      \",
\"      .. ..     \",
\"      ... ..    \",
\"      .#..      \",
\"      .##..     \",
\"      .##...    \",
\"      .##. .    \",
\"      .##.      \",
\"      .##.      \"};
"))
  "Image for indicator of inactive state."
)

(defconst posting-stationline-properties
  (when (display-mouse-p)
    `(local-map
      ,(purecopy (make-mode-line-mouse-map
		  'mouse-2 #'twittering-toggle-activate-buffer))
      help-echo "mouse-2 toggles activate buffer")))

(defconst posting-stationline-active
  (if twittering-active-indicator-image
      (apply 'propertize " "
	     `(display ,twittering-active-indicator-image
		       ,@posting-stationline-properties))
    " "))

(defconst posting-stationline-inactive
  (if twittering-inactive-indicator-image
      (apply 'propertize "INACTIVE"
	     `(display ,twittering-inactive-indicator-image
		       ,@posting-stationline-properties))
    "INACTIVE"))

(defun posting-station-line-buffer-identification ()
  (let ((active-mode-indicator
	 (if twittering-active-mode
	     posting-stationline-active
	   posting-stationline-inactive))
	(enabled-options
	 `(,(if twittering-display-connection-method
		(concat
		 (when twittering-use-ssl (concat posting-stationline-ssl ":"))
		 (twittering-get-connection-method-name twittering-use-ssl))
	      (when twittering-use-ssl posting-stationline-ssl))
	   ,@(when twittering-jojo-mode '("jojo"))
	   ,@(when twittering-icon-mode '("icon"))
	   ,@(when twittering-reverse-mode '("reverse"))
	   ,@(when twittering-proxy-use '("proxy")))))
    (concat active-mode-indicator
	    (when twittering-display-remaining
	      (let ((spec (twittering-current-timeline-spec)))
		(twittering-get-ratelimit-indicator-string spec)))
	    (when enabled-options
	      (concat "[" (mapconcat 'identity enabled-options " ") "]")))))

(defun twittering-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;;
;;;; Unread statuses info
;;;;

(defvar twittering-unread-status-info nil
  "A list of (buffer unread-statuses-counter), where `unread-statuses-counter'
means the number of statuses retrieved after the last visiting of the buffer.")

(defun twittering-reset-unread-status-info-if-necessary ()
  (when (twittering-buffer-p)
    (twittering-set-number-of-unread (current-buffer) 0)))

(defun twittering-set-number-of-unread (buffer number)
  (let* ((entry (assq buffer twittering-unread-status-info))
	 (current (or (cadr entry) 0)))
    (unless (= number current)
      (setq twittering-unread-status-info
	    (cons
	     `(,buffer ,number)
	     (if entry
		 (remq entry twittering-unread-status-info)
	       twittering-unread-status-info))))))

(defun twittering-make-unread-status-notifier-string ()
  "Generate a string that displays unread statuses."
  (setq twittering-unread-status-info
	(remove nil
		(mapcar (lambda (entry)
			  (when (buffer-live-p (car entry))
			    entry))
			twittering-unread-status-info)))
  (let ((sum (apply '+ (mapcar 'cadr twittering-unread-status-info))))
    (if (= 0 sum)
	""
      (format "tw(%d)" sum))))

(defun twittering-update-unread-status-info ()
  "Update `twittering-unread-status-info' with new tweets."
  (let* ((buffer (twittering-get-buffer-from-spec
		  twittering-rendered-new-tweets-spec))
	 (current (or (cadr (assq buffer twittering-unread-status-info)) 0))
	 (result (+ current (length twittering-rendered-new-tweets))))
    (when buffer
      (twittering-set-number-of-unread buffer result))))

(defun twittering-enable-unread-status-notifier ()
  "Enable a notifier of unread statuses on `posting-station'."
  (interactive)
  (setq twittering-unread-status-info
	(mapcar (lambda (buffer) `(,buffer ,0))
		(twittering-get-buffer-list)))
  (add-hook 'twittering-new-tweets-rendered-hook
	    'twittering-update-unread-status-info)
  (add-hook 'post-command-hook
	    'twittering-reset-unread-status-info-if-necessary)
  (add-to-list 'global-mode-string
	       '(:eval (twittering-make-unread-status-notifier-string))
	       t))

(defun twittering-disable-unread-status-notifier ()
  "Disable a notifier of unread statuses on `posting-station'."
  (interactive)
  (setq twittering-unread-status-info nil)
  (remove-hook 'twittering-new-tweets-hook
	       'twittering-update-unread-status-info)
  (remove-hook 'post-command-hook
	       'twittering-reset-unread-status-info-if-necessary)
  (setq global-mode-string
	(remove '(:eval (twittering-make-unread-status-notifier-string))
		global-mode-string)))

;;;;
;;;; Timer
;;;;

(defvar twittering-idle-timer-for-redisplay nil)

(defun twittering-timer-action (func)
  (let ((buf (twittering-get-active-buffer-list)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

(defun twittering-run-on-idle (idle-interval func &rest args)
  "Run FUNC the next time Emacs is idle for IDLE-INTERVAL.
Even if Emacs has been idle longer than IDLE-INTERVAL, run FUNC immediately.
Since immediate invocation requires `current-idle-time', it is available
on Emacs 22 and later.
FUNC is called as (apply FUNC ARGS)."
  (let ((sufficiently-idling
	 (and (fboundp 'current-idle-time)
	      (current-idle-time)
	      (time-less-p (seconds-to-time idle-interval)
			   (current-idle-time)))))
    (if (not sufficiently-idling)
	(apply 'run-with-idle-timer idle-interval nil func args)
      (apply func args)
      nil)))

(defun twittering-run-repeatedly-on-idle (check-interval var idle-interval func &rest args)
  "Run FUNC every time Emacs is idle for IDLE-INTERVAL.
Even if Emacs remains idle longer than IDLE-INTERVAL, run FUNC every
CHECK-INTERVAL seconds. Since this behavior requires `current-idle-time',
invocation on long idle time is available on Emacs 22 and later.
VAR is a symbol of a variable to which the idle-timer is bound.
FUNC is called as (apply FUNC ARGS)."
  (apply 'run-at-time "0 sec"
	 check-interval
	 (lambda (var idle-interval func &rest args)
	   (let ((registerd (symbol-value var))
		 (sufficiently-idling
		  (and (fboundp 'current-idle-time)
		       (current-idle-time)
		       (time-less-p (seconds-to-time idle-interval)
				    (current-idle-time)))))
	     (when (or (not registerd) sufficiently-idling)
	       (when (and registerd sufficiently-idling)
		 (cancel-timer (symbol-value var))
		 (apply func args))
	       (set var (apply 'run-with-idle-timer idle-interval nil
			       (lambda (var func &rest args)
				 (set var nil)
				 (apply func args))
			       var func args)))))
	 var idle-interval func args))

(defun twittering-start (&optional action)
  (interactive)
  (unless twittering-timer
    (let ((action (or action #'twittering-update-active-buffers)))
      ;; Update all active timelines forcibly.
      (twittering-update-active-buffers t)
      (setq twittering-timer
	    (run-at-time (format "%d sec" twittering-timer-interval)
			 twittering-timer-interval
			 #'twittering-timer-action action))))
  (unless twittering-timer-for-redisplaying
    (setq twittering-timer-for-redisplaying
	  (twittering-run-repeatedly-on-idle
	   (* 2 twittering-timer-interval-for-redisplaying)
	   'twittering-idle-timer-for-redisplay
	   twittering-timer-interval-for-redisplaying
	   #'twittering-redisplay-status-on-buffer))))

(defun twittering-stop ()
  (interactive)
  (when twittering-timer
    (cancel-timer twittering-timer)
    (setq twittering-timer nil))
  (when twittering-timer-for-redisplaying
    (when twittering-idle-timer-for-redisplay
      (cancel-timer twittering-idle-timer-for-redisplay)
      (setq twittering-idle-timer-for-redisplay nil))
    (cancel-timer twittering-timer-for-redisplaying)
    (setq twittering-timer-for-redisplaying nil)))

(defun twittering-get-relative-interval (spec)
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
	 (normalized-alist
	  (apply 'append
		 (mapcar
		  (lambda (entry)
		    (let ((interval (car (last entry)))
			  (regexp-list (butlast entry 1)))
		      (when (integerp interval)
			(mapcar (lambda (regexp) `(,regexp . ,interval))
				regexp-list))))
		  twittering-relative-retrieval-interval-alist)))
	 (rest normalized-alist)
	 (current normalized-alist)
	 (result 0))
    (while (not
	    (or (and (stringp (car current))
		     (string-match (car current) spec-string))
		(eq t (car current))))
      (setq current (car rest))
      (setq rest (cdr rest)))
    (if (integerp (cdr current))
	(cdr current)
      ;; The default relative interval is 1.
      1)))

(defun twittering-get-retrieval-count (spec)
  (cdr (assoc spec twittering-relative-retrieval-count-alist)))

(defun twittering-set-retrieval-count (spec count)
  (let ((current (assoc spec twittering-relative-retrieval-count-alist)))
    (if (null current)
	(add-to-list 'twittering-relative-retrieval-count-alist
		     `(,spec . ,count))
      (setcdr current count))))

(defun twittering-initialize-retrieval-count (spec)
  (twittering-set-retrieval-count spec
				  (twittering-get-relative-interval spec)))

(defun twittering-update-active-buffers (&optional force noninteractive)
  "Update active buffers managed by `posting-station' at a certain interval.

If FORCE is nil, each active buffer is updated at a relative interval
determined by `twittering-relative-retrieval-interval-alist'.
If a relative interval of a timeline is 3, the timeline is updated once
by three invocations of this function.

If FORCE is non-nil, all active buffers are updated forcibly."
  (when (twittering-account-authorized-p)
    (twittering-update-service-configuration)
    (let* ((buffer-list (twittering-get-active-buffer-list))
	   (primary-spec-list
	    (twittering-remove-duplicates
	     (apply 'append
		    (mapcar
		     (lambda (buffer)
		       (twittering-get-primary-base-timeline-specs
			(twittering-get-timeline-spec-for-buffer buffer)))
		     buffer-list)))))
      (mapc
       (lambda (spec)
	 (let ((current
		(if force
		    1
		  (twittering-get-retrieval-count spec))))
	   (cond
	    ((null current)
	     ;; Initialize the count if no entry for the primary timeline
	     ;; exists.
	     (twittering-initialize-retrieval-count spec))
	    ((and (integerp current) (= 0 current))
	     ;; Do nothing.
	     )
	    ((and (integerp current) (= 1 current))
	     ;; Retrieve the timeline and initialize count.
	     (let ((spec-string
		    (twittering-timeline-spec-to-string spec)))
	       (twittering-get-and-render-timeline
		noninteractive nil spec spec-string)
	       (twittering-initialize-retrieval-count spec)))
	    ((and (integerp current) (< 1 current))
	     ;; Decrement count.
	     (twittering-set-retrieval-count spec (1- current)))
	    (t
	     nil))))
       primary-spec-list))))

;;;;
;;;; Keymap
;;;;

(if posting-station-map
    (let ((km posting-station-map))
      (define-key km (kbd "C-c C-f") 'twittering-friends-timeline)
      (define-key km (kbd "C-c C-r") 'twittering-replies-timeline)
      (define-key km (kbd "C-c C-n") 'twittering-mentions-timeline)
      (define-key km (kbd "C-c C-u") 'twittering-user-timeline)
      (define-key km (kbd "C-c C-d") 'twittering-direct-messages-timeline)
      (define-key km (kbd "C-c C-s") 'twittering-update-status-interactive)
      (define-key km (kbd "C-c C-e") 'twittering-erase-old-statuses)
      (define-key km (kbd "C-c C-m") 'twittering-retweet)
      (define-key km (kbd "C-c C-t") 'twittering-set-current-hashtag)
      (define-key km (kbd "C-m") 'twittering-enter)
      (define-key km (kbd "C-c C-l") 'twittering-update-lambda)
      (define-key km (kbd "<mouse-1>") 'twittering-click)
      (define-key km (kbd "C-<down-mouse-3>") 'mouse-set-point)
      (define-key km (kbd "C-<mouse-3>") 'twittering-push-tweet-onto-kill-ring)
      (define-key km (kbd "C-c C-v") 'twittering-view-user-page)
      (define-key km (kbd "C-c D") 'twittering-delete-status)
      (define-key km (kbd "C-c C-w") 'twittering-delete-status)
      (define-key km (kbd "a") 'twittering-toggle-activate-buffer)
      (define-key km (kbd "g") 'twittering-current-timeline)
      (define-key km (kbd "u") 'twittering-update-status-interactive)
      (define-key km (kbd "U") 'twittering-push-uri-onto-kill-ring)
      (define-key km (kbd "d") 'twittering-direct-message)
      (define-key km (kbd "v") 'twittering-other-user-timeline)
      (define-key km (kbd "V") 'twittering-visit-timeline)
      (define-key km (kbd "L") 'twittering-other-user-list-interactive)
      (define-key km (kbd "f") 'twittering-switch-to-next-timeline)
      (define-key km (kbd "b") 'twittering-switch-to-previous-timeline)
      ;; (define-key km (kbd "j") 'next-line)
      ;; (define-key km (kbd "k") 'previous-line)
      (define-key km (kbd "j") 'twittering-goto-next-status)
      (define-key km (kbd "k") 'twittering-goto-previous-status)
      (define-key km (kbd "l") 'forward-char)
      (define-key km (kbd "h") 'backward-char)
      (define-key km (kbd "0") 'beginning-of-line)
      (define-key km (kbd "^") 'beginning-of-line-text)
      (define-key km (kbd "$") 'end-of-line)
      (define-key km (kbd "n") 'twittering-goto-next-status-of-user)
      (define-key km (kbd "p") 'twittering-goto-previous-status-of-user)
      (define-key km (kbd "C-i") 'twittering-goto-next-thing)
      (define-key km (kbd "M-C-i") 'twittering-goto-previous-thing)
      (define-key km (kbd "<backtab>") 'twittering-goto-previous-thing)
      (define-key km (kbd "<backspace>") 'twittering-scroll-down)
      (define-key km (kbd "M-v") 'twittering-scroll-down)
      (define-key km (kbd "SPC") 'twittering-scroll-up)
      (define-key km (kbd "C-v") 'twittering-scroll-up)
      (define-key km (kbd "G") 'twittering-goto-last-status)
      (define-key km (kbd "H") 'twittering-goto-first-status)
      (define-key km (kbd "i") 'twittering-icon-mode)
      (define-key km (kbd "r") 'twittering-toggle-show-replied-statuses)
      (define-key km (kbd "R") 'twittering-toggle-or-retrieve-replied-statuses)
      (define-key km (kbd "t") 'twittering-toggle-proxy)
      (define-key km (kbd "C-c C-p") 'twittering-toggle-proxy)
      (define-key km (kbd "q") 'twittering-kill-buffer)
      (define-key km (kbd "C-c C-q") 'twittering-search)
      nil))

(let ((km posting-station-menu-on-uri-map))
  (when km
    (define-key km [ct] '("Copy tweet" . twittering-push-tweet-onto-kill-ring))
    (define-key km [cl] '("Copy link" . twittering-push-uri-onto-kill-ring))
    (define-key km [ll] '("Load link" . twittering-click))
    (let ((km-on-uri posting-station-on-uri-map))
      (when km-on-uri
	(define-key km-on-uri (kbd "C-<down-mouse-3>") 'mouse-set-point)
	(define-key km-on-uri (kbd "C-<mouse-3>") km)))))

(defun twittering-keybind-message ()
  (let ((important-commands
	 '(("Timeline" . twittering-friends-timeline)
	   ("Replies" . twittering-replies-timeline)
	   ("Mentions" . twittering-mentions-timeline)
	   ("Update status" . twittering-update-status-interactive)
	   ("Next" . twittering-goto-next-status)
	   ("Prev" . twittering-goto-previous-status))))
    (mapconcat (lambda (command-spec)
		 (let ((descr (car command-spec))
		       (command (cdr command-spec)))
		   (format "%s: %s" descr (key-description
					   (where-is-internal
					    command
					    overriding-local-map t)))))
	       important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twittering-buffer)
;;       (message (twittering-keybind-message)))))


;;;;
;;;; Initialization
;;;;

(defvar twittering-initialized nil)
(defvar posting-station-syntax-table nil "")

(unless posting-station-syntax-table
  (setq posting-station-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" posting-station-syntax-table)
  (modify-syntax-entry ?\" "w" posting-station-syntax-table)
  )

(defun twittering-initialize-global-variables-if-necessary ()
  "Initialize global variables for `posting-station' if they have not
been initialized yet."
  (unless twittering-initialized
    (defface twittering-username-face
      `((t ,(append '(:underline t)
		    (face-attr-construct
		     (if (facep 'font-lock-string-face)
			 'font-lock-string-face
		       'bold)))))
      "" :group 'faces)
    (defface twittering-uri-face `((t (:underline t))) "" :group 'faces)
    (defface twittering-timeline-header-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline header on posting-station" :group 'faces)
    (defface twittering-timeline-footer-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline footer on posting-station" :group 'faces)
    (twittering-update-status-format)
    (when twittering-use-convert
      (if (null twittering-convert-program)
	  (setq twittering-use-convert nil)
	(with-temp-buffer
	  (let ((coding-system-for-read 'iso-safe)
		(coding-system-for-write 'iso-safe)
		;; Bind `default-directory' to the temporary directory
		;; because it is possible that the directory pointed by
		;; `default-directory' has been already removed.
		(default-directory temporary-file-directory))
	    (call-process twittering-convert-program nil (current-buffer) nil
			  "-version")
	    (goto-char (point-min))
	    (if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick"
					     nil t))
		(setq twittering-use-convert nil))))))
    (twittering-setup-proxy)
    (when twittering-use-icon-storage
      (cond
       ((require 'jka-compr nil t)
	(twittering-load-icon-properties)
	(add-hook 'kill-emacs-hook 'twittering-save-icon-properties))
       (t
	(setq twittering-use-icon-storage nil)
	(error "Disabled icon-storage because it failed to load jka-compr."))))
    (cond
     ((and
       (boundp 'twittering-sign-simple-string)
       twittering-sign-simple-string
       (or (not (boundp 'twittering-sign-string-function))
	   (null twittering-sign-string-function))
       (eq twittering-edit-skeleton 'none)
       (or (null twittering-edit-skeleton-footer)
	   (string= twittering-edit-skeleton-footer "")))
      ;; Configure `twittering-edit-skeleton' as an alternative of
      ;; `twittering-sign-simple-string'.
      (twittering-edit-skeleton-change-footer
       (format " [%s]" twittering-sign-simple-string))
      (setq twittering-edit-skeleton 'footer)
      (message "Warning: `twittering-sign-simple-string' is obsolete. Use `twittering-edit-skeleton-footer' instead."))
     ((or (boundp 'twittering-sign-simple-string)
	  (boundp 'twittering-sign-string-function))
      (message "Warning: `twittering-sign-simple-string' and `twittering-sign-string-function' are obsolete. Use the new feature `twittering-edit-skeleton'.")
      ))
    (add-hook 'twittering-new-tweets-rendered-hook
	      'twittering-jojo-mode-hook-function)
    (run-hooks 'posting-station-init-hook)
    (setq twittering-initialized t)))

(defun posting-station-setup (spec-string)
  "Set up the current buffer for `posting-station'."
  (kill-all-local-variables)
  (setq major-mode 'posting-station)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq mode-name "posting-station")
  (setq mode-line-buffer-identification
	`(,(default-value 'mode-line-buffer-identification)
	  (:eval (posting-station-line-buffer-identification))))

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not posting-station))

  ;; Prevent the field property attached to tweets from interfering
  ;; the cursor motion based on logical lines.
  (make-local-variable 'inhibit-field-text-motion)
  (setq inhibit-field-text-motion t)

  (make-local-variable 'twittering-timeline-spec)
  (make-local-variable 'twittering-timeline-spec-string)
  (make-local-variable 'twittering-active-mode)
  (make-local-variable 'twittering-icon-mode)
  (make-local-variable 'twittering-jojo-mode)
  (make-local-variable 'twittering-reverse-mode)

  (setq twittering-timeline-spec-string spec-string)
  (setq twittering-timeline-spec
	(twittering-string-to-timeline-spec spec-string))
  (setq twittering-active-mode t)

  (use-local-map posting-station-map)
  (twittering-update-mode-line)
  (set-syntax-table posting-station-syntax-table)
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1))
  (add-to-list 'twittering-buffer-info-list (current-buffer) t)
  (run-hooks 'posting-station-hook))

(defun posting-station ()
  "Major mode for Twitter
\\{posting-station-map}"
  (interactive)
  (let ((timeline-spec-list
	 (if (listp twittering-initial-timeline-spec-string)
	     twittering-initial-timeline-spec-string
	   (cons twittering-initial-timeline-spec-string nil))))
    (twittering-visit-timeline (car timeline-spec-list))
    (when (twittering-account-authorized-p)
      (mapc 'twittering-visit-timeline (cdr timeline-spec-list)))))

;;;;
;;;; Reading username/listname with completion
;;;;

(defun twittering-get-usernames-from-timeline (&optional timeline-data)
  (let ((timeline-data (or timeline-data (twittering-current-timeline-data))))
    (twittering-remove-duplicates
     (mapcar
      (lambda (status)
	(let* ((base-str (cdr (assq 'user-screen-name status)))
	       ;; `copied-str' is independent of the string in timeline-data.
	       ;; This isolation is required for `minibuf-isearch.el',
	       ;; which removes the text properties of strings in history.
	       (copied-str (copy-sequence base-str)))
	  (set-text-properties 0 (length copied-str) nil copied-str)
	  copied-str))
      timeline-data))))

(defun twittering-read-username-with-completion (prompt init-user &optional history)
  (let ((collection (append twittering-user-history
			    (twittering-get-usernames-from-timeline))))
    (twittering-completing-read prompt collection nil nil init-user history)))

(defun twittering-read-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twittering-get-list-index-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's list: " username))
	 (listname
	  (if list-index
	      (twittering-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twittering-read-subscription-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twittering-get-list-subscriptions-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's subscription: " username))
	 (listname
	  (if list-index
	      (twittering-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twittering-read-timeline-spec-with-completion (prompt initial &optional as-string)
  (let* ((dummy-hist
	  (append twittering-timeline-history
		  (twittering-get-usernames-from-timeline)
		  '(":direct_messages" ":direct_messages_sent"
		    ":favorites" ":friends"
		    ":home" ":mentions" ":public" ":replies"
		    ":retweeted_by_me" ":retweeted_by_user/"
		    ":retweeted_to_me" ":retweeted_to_user/"
		    ":retweets_of_me")
		  (mapcar (lambda (cell)
			    (concat "$" (car cell) (if (listp (cdr cell)) "()" "")))
			  twittering-timeline-spec-alias)))
	 (spec-with-username
	  '((":favorites/" . "Whose favorites: ")
	    (":retweeted_by_user/" . "Who has retweeted? ")
	    (":retweeted_to_user/" . "Who has received the retweets? ")))
	 (regexp-spec-with-username
	  (concat "\\`\\("
		  (mapconcat (lambda (entry) (car entry))
			     spec-with-username "\\|")
		  "\\)\\'"))
	 (spec-string (twittering-completing-read prompt dummy-hist
						  nil nil initial 'dummy-hist))
	 (spec-string
	  (cond
	   ((string-match regexp-spec-with-username spec-string)
	    (let* ((spec-and-prompt
		    (assoc (match-string 1 spec-string)
			   spec-with-username))
		   (prefix (car spec-and-prompt))
		   (prompt (cdr spec-and-prompt))
		   (username
		    (twittering-read-username-with-completion
		     prompt ""
		     'twittering-user-history)))
	      (if username
		  (concat prefix username)
		nil)))
	   ((string-match "^\\([a-zA-Z0-9_-]+\\)/$" spec-string)
	    (let* ((username (match-string 1 spec-string))
		   (list-index (twittering-get-list-index-sync username))
		   (listname
		    (if list-index
			(twittering-read-list-name username list-index)
		      nil)))
	      (if listname
		  (concat username "/" listname)
		nil)))
	   (t
	    spec-string)))
	 (spec (if (stringp spec-string)
		   (condition-case error-str
		       (twittering-string-to-timeline-spec spec-string)
		     (error
		      (message "Invalid timeline spec: %s" error-str)
		      nil))
		 nil)))
    (cond
     ((null spec)
      nil)
     (spec (if as-string
	       spec-string
	     spec))
     ((string= "" spec-string)
      (message "No timeline specs are specified.")
      nil)
     (t
      (message "\"%s\" is invalid as a timeline spec." spec-string)
      nil))))

;;;;
;;;; Commands
;;;;

;;;; Commands for changing modes
(defun twittering-jojo-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-jojo-mode))
    (setq twittering-jojo-mode
	  (if (null arg)
	      (not twittering-jojo-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-jojo-mode)
      (twittering-update-mode-line))))

(defun twittering-toggle-reverse-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-reverse-mode))
    (setq twittering-reverse-mode
	  (if (null arg)
	      (not twittering-reverse-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-reverse-mode)
      (twittering-update-mode-line)
      (twittering-rerender-timeline-all (current-buffer)))))

(defun twittering-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (twittering-completing-read "hashtag (blank to clear): #"
					  twittering-hashtag-history
					  nil nil
					  twittering-current-hashtag
					  'twittering-hashtag-history))
    (message
     (if (eq 0 (length tag))
	 (progn (setq twittering-current-hashtag nil)
		"Current hashtag is not set.")
       (progn
	 (setq twittering-current-hashtag tag)
	 (format "Current hashtag is #%s" twittering-current-hashtag))))))

;;;; Commands for switching buffers
(defun twittering-switch-to-next-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (twittering-get-buffer-list))
	   (following-buffers (cdr (memq (current-buffer) buffer-list)))
	   (next (if following-buffers
		     (car following-buffers)
		   (car buffer-list))))
      (unless (eq (current-buffer) next)
	(switch-to-buffer next)))))

(defun twittering-switch-to-previous-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (reverse (twittering-get-buffer-list)))
	   (preceding-buffers (cdr (memq (current-buffer) buffer-list)))
	   (previous (if preceding-buffers
			 (car preceding-buffers)
		       (car buffer-list))))
      (unless (eq (current-buffer) previous)
	(switch-to-buffer previous)))))

;;;; Commands for visiting a timeline
(defun twittering-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (cond
   ((twittering-ensure-preparation-for-api-invocation)
    (let ((timeline-spec
	   (or timeline-spec
	       (twittering-read-timeline-spec-with-completion
		"timeline: " initial t))))
      (when timeline-spec
	(switch-to-buffer (twittering-get-managed-buffer timeline-spec)))))
   (t
    nil)))

(defun twittering-friends-timeline ()
  (interactive)
  (twittering-visit-timeline '(friends)))

(defun twittering-home-timeline ()
  (interactive)
  (twittering-visit-timeline '(home)))

(defun twittering-replies-timeline ()
  (interactive)
  (twittering-visit-timeline '(replies)))

(defun twittering-mentions-timeline ()
  (interactive)
  (twittering-visit-timeline '(mentions)))

(defun twittering-public-timeline ()
  (interactive)
  (twittering-visit-timeline '(public)))

(defun twittering-user-timeline ()
  (interactive)
  (twittering-visit-timeline `(user ,(twittering-get-username))))

(defun twittering-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages)))

(defun twittering-sent-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages_sent)))

(defun twittering-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (goto-spec (get-text-property (point) 'goto-spec))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (uri (or (get-text-property (point) 'expanded-uri)
		  (get-text-property (point) 'uri)))
	 (mentioned-id (when uri
			 (twittering-extract-id-from-url uri)))
	 (spec (cond (goto-spec goto-spec)
		     (screen-name-in-text `(user ,screen-name-in-text))
		     (mentioned-id `(single ,mentioned-id))
		     (username `(user ,username))
		     (t nil))))
    (if spec
	(twittering-visit-timeline spec)
      (message "No user selected"))))

(defun twittering-other-user-timeline-interactive ()
  (interactive)
  (let ((username (or (twittering-read-username-with-completion
		       "user: " nil
		       'twittering-user-history)
		      "")))
    (if (string= "" username)
	(message "No user selected")
      (twittering-visit-timeline `(user ,username)))))

(defun twittering-other-user-list-interactive (&optional subscriptions)
  (interactive "P")
  (let* ((username (copy-sequence (get-text-property (point) 'username)))
	 (username (progn
		     (set-text-properties 0 (length username) nil username)
		     (or (twittering-read-username-with-completion
			  (if subscriptions
			      "Whose subscription: "
			    "Whose list: ")
			  username
			  'twittering-user-history)
			 ""))))
    (if (string= "" username)
	(message "No user selected")
      (let* ((list-name (if subscriptions
			    (twittering-read-subscription-list-name username)
			  (twittering-read-list-name username)))
	     (spec (cond
		    ((null list-name)
		     nil)
		    (subscriptions
		     (and (string-match "\\`\\(.*\\)/\\(.*\\)\\'" list-name)
			  `(list ,(match-string 1 list-name)
				 ,(match-string 2 list-name))))
		    (t
		     `(list ,username ,list-name)))))
	(if spec
	    (twittering-visit-timeline spec)
	  ;; Don't show message here to prevent an overwrite of a
	  ;; message which is outputted by `twittering-read-list-name'.
	  )))))

(defun twittering-search (&optional word)
  (interactive)
  (let ((word (or word
		  (read-from-minibuffer "search: " nil nil nil
					'twittering-search-history nil t)
		  "")))
    (if (string= "" word)
	(message "No query string")
      (let ((spec `(search ,word)))
	(twittering-visit-timeline spec)))))

;;;; Commands for retrieving statuses

(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec-string (twittering-current-timeline-spec-string)))
      (twittering-get-and-render-timeline noninteractive))))

(defun twittering-get-tweets-within-specific-time-range (time-beg time-end)
  "Get tweets within a time range between TIME-BEG and TIME-END.
TIME-BEG and TIME-END must be nil or an internal representation of time as
same as the returned value of `current-time'."
  (let* ((since_id (when time-beg
		     (twittering-time-to-id time-beg)))
	 (max_id (when time-end
		   (twittering-time-to-id time-end)))
	 (spec-string (twittering-current-timeline-spec-string))
	 (noninteractive t)
	 (args
	  `(,@(cond
	       (max_id `((max_id . ,max_id)))
	       (since_id `((since_id . ,since_id)))
	       (t nil)))))
    (twittering-retrieve-timeline spec-string noninteractive args nil)))

(defun twittering-get-tweets-before (&optional before-str)
  (interactive)
  (let* ((id (when (null before-str)
	       (twittering-get-id-at)))
	 (init-str
	  (when id
	    (let* ((status (twittering-find-status id))
		   (init-time
		    (or (cdr (assq 'retweeting-created-at status))
			(cdr (assq 'created-at status)))))
	      (format-time-string "%Y-%m-%d %T" init-time))))
	 (before-str
	  (or before-str
	      (read-string "before [YYYY-MM-DD [HH:MM:SS]]: " init-str)))
	 (time-beg nil)
	 (time-end
	  (apply 'encode-time (twittering-parse-time-string before-str t))))
    (twittering-get-tweets-within-specific-time-range time-beg time-end)))

;;;; Commands for posting a status

(defun twittering-update-status (&optional init-string-or-skeleton reply-to-id username tweet-type ignore-current-spec)
  "Post a tweet.
The first argument INIT-STRING-OR-SKELETON is nil, an initial text or a
skeleton to be inserted with `skeleton-insert'.
REPLY-TO-ID is an ID of a tweet which you are going to cite or reply to.
USERNAME is a recipient of a direct message.
TWEET-TYPE is a symbol meaning the type of the tweet being edited. It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
If TWEET-TYPE is nil, it is equivalent to 'normal, which means that a tweet
is edited as a normal tweet.
If IGNORE-CURRENT-SPEC is non-nil, the timeline spec of the current buffer
is sent to the function specified by `twittering-update-status-function'.

How to edit a tweet is determined by `twittering-update-status-funcion'."
  (let ((current-spec (unless ignore-current-spec
			(twittering-current-timeline-spec)))
	(tweet-type (or tweet-type 'normal)))
    (funcall twittering-update-status-function init-string-or-skeleton
	     reply-to-id username
	     tweet-type current-spec)))

(defun twittering-update-status-interactive ()
  (interactive)
  (twittering-update-status))

(defun twittering-update-lambda ()
  (interactive)
  (when (and (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (let ((text (mapconcat
		 'char-to-string
		 (mapcar 'twittering-ucs-to-char
			 '(955 12363 12431 12356 12356 12424 955)) "")))
      (twittering-call-api 'update-status `((status . ,text))))))

(defun twittering-post-predicted-message-like-jojo (status)
  (let ((screen-name (cdr (assq 'user-screen-name status)))
	(text (cdr (assq 'text status))))
    (when (and (not (string= screen-name (twittering-get-username)))
	       (string-match
		(mapconcat 'char-to-string
			   (mapcar
			    'twittering-ucs-to-char
			    '(#x6b21 #x306b #x005c #x0028 #x304a #x524d
				     #x005c #x007c #x8cb4 #x69d8 #x005c #x0029
				     #x306f #x300c #x005c #x0028 #x005b #x005e
				     #x300d #x005d #x002b #x005c #x0029 #x300d
				     #x3068 #x8a00 #x3046))
			   "")
		text))
      (let ((text
	     (concat "@" screen-name " "
		     (match-string-no-properties 2 text)
		     (mapconcat 'char-to-string
				(mapcar 'twittering-ucs-to-char
					'(#x3000 #x306f #x3063 #x0021 #x003f))
				""))))
	(twittering-call-api 'update-status `((status . ,text)))))))

(defun twittering-jojo-mode-hook-function ()
  (when (and twittering-jojo-mode
	     (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (mapcar 'twittering-post-predicted-message-like-jojo
	    twittering-rendered-new-tweets)))

(defun twittering-direct-message ()
  (interactive)
  (let ((username (twittering-read-username-with-completion
		   "Who would you like to receive the DM? "
		   (get-text-property (point) 'username)
		   'twittering-user-history)))
    (if (string= "" username)
	(message "No user selected")
      (twittering-update-status nil nil username 'direct-message))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status (concat "@" username " "))
      (message "No user selected"))))

;;;; Command for deleting a status

(defun twittering-delete-status (&optional id)
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (twittering-find-status id))
	 (is-retweet (assq 'retweeted-id status))
	 (username (if is-retweet
		       (cdr (assq 'retweeting-user-screen-name status))
		     (cdr (assq 'user-screen-name status))))
	 (text (if is-retweet
		   (cdr (assq 'retweeting-text status))
		 (cdr (assq 'text status))))
	 (width (max 40 ;; XXX
		     (- (frame-width)
			1 ;; margin for wide characters
			11 ;; == (length (concat "Delete \"" "\"? "))
			9) ;; == (length "(y or n) ")
		     ))
	 (mes (format "Delete \"%s\"? "
		      (if (< width (string-width text))
			  (concat
			   (truncate-string-to-width text (- width 3))
			   "...")
			text))))
    (cond
     ((not (string= username (twittering-get-username)))
      (message "The status is not yours!"))
     ((not id)
      (message "No status selected"))
     ((y-or-n-p mes)
      (twittering-call-api 'destroy-status `((id . ,id))))
     (t
      (message "Request canceled")))))

;;;; Commands for retweet

(defun twittering-retweet (&optional arg)
  (interactive "P")
  (let ((use-native-retweet-flag (if arg
				     (not twittering-use-native-retweet)
				   twittering-use-native-retweet)))
    (if use-native-retweet-flag
	(twittering-native-retweet)
      (twittering-organic-retweet))))

(defun twittering-organic-retweet ()
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (twittering-find-status id))
	 (username (cdr (assq 'user-screen-name status)))
	 (text (cdr (assq 'text status)))
	 (retweet-time (current-time))
	 (skeleton-with-format-string
	  (cond
	   ((null twittering-retweet-format)
	    '(nil _ " RT: %t (via @%s)"))
	   ((stringp twittering-retweet-format)
	    `(nil ,twittering-retweet-format _))
	   ((listp twittering-retweet-format)
	    twittering-retweet-format)
	   (t
	    nil))))
    (cond
     ((cdr (assq 'user-protected status))
      (error "Cannot retweet protected tweets."))
     (username
      (let ((prefix "%")
	    (replace-table
	     `(("%" . "%")
	       ("s" . ,username)
	       ("t" . ,text)
	       ("#" . ,id)
	       ("u" . ,(twittering-get-status-url-from-alist status))
	       ("C{\\([^}]*\\)}" .
		(lambda (context)
		  (let ((str (cdr (assq 'following-string context)))
			(match-data (cdr (assq 'match-data context))))
		    (store-match-data match-data)
		    (format-time-string (match-string 1 str) ',retweet-time))))
	       ))
	    )
	(twittering-update-status
	 (mapcar (lambda (element)
		   (if (stringp element)
		       (twittering-format-string element prefix replace-table)
		     element))
		 skeleton-with-format-string)
	 id nil 'organic-retweet)
	)))))

(defun twittering-native-retweet ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       12 ;; == (length (concat "Retweet \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    )))
    (set-text-properties 0 (length text) nil text)
    (if id
	(let ((mes (format "Retweet \"%s\"? "
			   (if (< width (string-width text))
			       (concat
				(truncate-string-to-width text (- width 3))
				"...")
			     text))))
	  (if (y-or-n-p mes)
	      (twittering-call-api 'retweet `((id . ,id)))
	    (message "Request canceled")))
      (message "No status selected"))))

;;;; Commands for browsing information related to a status

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-enter ()
  "Default function to run when RET is pressed;
determines whether to refresh feed, reply, or DM."
  (interactive)
  (let* ((username (or (get-text-property (point) 'screen-name-in-text)
		       (get-text-property (point) 'username)))
	 (id (twittering-get-id-at (point)))
	 (uri (get-text-property (point) 'uri))
	 (tweet-type
	  (cond
	   ((twittering-timeline-spec-is-direct-messages-p
	     (get-text-property (point) 'source-spec))
	    'direct-message)
	   (t
	    'reply)))
	 (initial-str
	  (when (and (not (eq tweet-type 'direct-message))
		     username)
	    (concat "@" username " ")))
	 (field-id (get-text-property (point) 'field))
	 (is-latest-end (twittering-field-id-is-timeline-latest-end field-id))
	 (is-oldest-end (twittering-field-id-is-timeline-oldest-end field-id)))
    (cond
     (is-latest-end
      (message "Get more of the recent timeline...")
      (if twittering-reverse-mode
	  (twittering-goto-last-normal-field)
	(twittering-goto-first-normal-field))
      (twittering-get-and-render-timeline))
     (is-oldest-end
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status))))
	(message "Get more of the previous timeline...")
	(if twittering-reverse-mode
	    (twittering-goto-first-normal-field)
	  (twittering-goto-last-normal-field))
	(twittering-get-and-render-timeline nil oldest-id)))

     ;; Must come before `username' test
     (uri
      (browse-url uri))

     (username
      (twittering-update-status initial-str
				id
				username
				tweet-type)))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

;;;;
;;;; Commands corresponding to operations on Twitter
;;;;

(defun twittering-follow (&optional remove)
  (interactive "P")
  (let* ((method (if remove 'destroy-friendships 'create-friendships))
	 (mes (if remove "unfollow" "follow"))
	 (id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (default (if remove
				retweeting-username
			      retweeted-username))
		   (prompt (format "Who do you %s? (default:%s): "
				   mes default))
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t
					  nil nil default)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    (twittering-read-username-with-completion
	     (format "Who do you %s? " mes) "" 'twittering-user-history)))))
    (if (string= "" username)
	(message "No user selected")
      (if (y-or-n-p (format "%s %s? " (capitalize mes) username))
	  (twittering-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twittering-unfollow ()
  (interactive)
  (twittering-follow t))

(defun twittering-favorite (&optional remove)
  (interactive "P")
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       15 ;; == (length (concat "Unfavorite \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    ))
	(method (if remove 'destroy-favorites 'create-favorites)))
    (set-text-properties 0 (length text) nil text)
    (if id
	(let ((mes (format "%s \"%s\"? "
			   (if remove "Unfavorite" "Favorite")
			   (if (< width (string-width text))
			       (concat
				(truncate-string-to-width text (- width 3))
				"...")
			     text))))
	  (if (y-or-n-p mes)
	      (twittering-call-api method `((id . ,id)))
	    (message "Request canceled")))
      (message "No status selected"))))

(defun twittering-unfavorite ()
  (interactive)
  (twittering-favorite t))

(defun twittering-mute (&optional remove)
  (interactive "P")
  (let* ((method (if remove 'unmute 'mute))
	 (mes (if remove "unmute" "mute"))
	 (id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (default (if remove
				retweeting-username
			      retweeted-username))
		   (prompt (format "Who do you %s? (default:%s): "
				   mes default))
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t
					  nil nil default)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    (twittering-read-username-with-completion
	     (format "Who do you %s? " mes) "" 'twittering-user-history)))))
    (if (string= "" username)
	(message "No user selected")
      (if (y-or-n-p (format "%s %s? " (capitalize mes) username))
	  (twittering-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twittering-unmute ()
  (interactive)
  (twittering-mute t))

(defun twittering-block ()
  "Block a user who posted the tweet at the current position."
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you block? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p (format "Really block \"%s\"? " username))
      (twittering-call-api 'block `((username . ,username))))
     (t
      (message "Request canceled")))))

(defun twittering-block-and-report-as-spammer ()
  "Report a user who posted the tweet at the current position as a spammer.
The user is also blocked."
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you report as a spammer? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p
       (format "Really block \"%s\" and report him or her as a spammer? "
	       username))
      (twittering-call-api 'block-and-report-as-spammer
			   `((username . ,username))))
     (t
      (message "Request canceled")))))

;;;; Commands for clearing stored statuses.

(defun twittering-erase-old-statuses ()
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec (twittering-current-timeline-spec)))
      (twittering-remove-timeline-data spec) ;; clear current timeline.
      (twittering-rerender-timeline-all (current-buffer)) ;; clear buffer.
      (twittering-get-and-render-timeline))))

;;;; Cursor motion

(defun twittering-get-id-at (&optional pos)
  "Return ID of the status at POS. If a separator is rendered at POS, return
the ID of the status rendered before the separator. The default value of POS
is `(point)'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'id)
	(let ((prev (or (twittering-get-current-status-head pos)
			(point-min))))
	  (and prev (get-text-property prev 'id))))))

(defun twittering-get-current-status-head (&optional pos)
  "Return the head position of the status at POS.
If POS is nil, the value of point is used for POS.
If a separator is rendered at POS, return the head of the status followed
by the separator.
Return POS if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 ;; Find the beginning of the field regardless of stickiness.
	 (head (field-beginning pos t)))
    (cond
     ((null field-id)
      ;; A separator is rendered at `pos'.
      (if (get-text-property head 'field)
	  ;; When `pos' points the head of the separator, `head' points
	  ;; to the beginning of the status followed by the separator.
	  head
	;; In the case that `pos' points to a character of the separator,
	;; but not to the head of the separator.
	(field-beginning head t)))
     ((null (get-text-property head 'field))
      ;; When `head' points to a separator, `pos' points to the head
      ;; of a status.
      pos)
     ((not (twittering-field-id= field-id (get-text-property head 'field)))
      ;; When `pos' points to the beginning of the field and it also
      ;; points to the end of the previous field, `head' points to the
      ;; head of the previous status.
      pos)
     (t
      head))))

(defun twittering-goto-first-status ()
  "Go to the first status."
  (interactive)
  (goto-char (or (twittering-get-first-status-head)
		 (point-min))))

(defun twittering-get-first-status-head ()
  "Return the head position of the first status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-min) 'field)
      (point-min)
    (twittering-get-next-status-head (point-min))))

(defun twittering-goto-last-status ()
  "Go to the last status."
  (interactive)
  (goto-char (or (twittering-get-last-status-head)
		 (point-min))))

(defun twittering-get-last-status-head ()
  "Return the head position of the last status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-max) 'field)
      (point-max)
    (twittering-get-previous-status-head (point-max))))

(defun twittering-goto-first-normal-field ()
  "Go to the first normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twittering-get-first-normal-field-head)
		 (point-min))))

(defun twittering-goto-last-normal-field ()
  "Go to the last normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twittering-get-last-normal-field-head)
		 (point-max))))

(defun twittering-get-first-normal-field-head ()
  "Return the head position of the first normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twittering-get-first-status-head)))
    (while (and pos
		(< pos (point-max))
		(null (get-text-property pos 'id)))
      (setq pos (twittering-get-next-status-head pos)))
    (when (and pos (< pos (point-max)) (get-text-property pos 'id))
      pos)))

(defun twittering-get-last-normal-field-head ()
  "Return the head position of the last normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twittering-get-last-status-head)))
    (while (and pos
		(< (point-min) pos)
		(null (get-text-property pos 'id)))
      (setq pos (twittering-get-previous-status-head pos)))
    (when (and pos (< (point-min) pos) (get-text-property pos 'id))
      pos)))

(defun twittering-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos (twittering-get-next-status-head)))
    (cond
     (pos
      (goto-char pos))
     (twittering-reverse-mode
      (message "The latest status."))
     (t
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twittering-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the footer field or the end of
	  ;; the buffer. It should be moved backward to a normal tweet.
	  (twittering-goto-last-normal-field)
	  (twittering-get-and-render-timeline nil oldest-id))))))))

(defun twittering-get-next-status-head (&optional pos)
  "Search forward from POS for the nearest head of a status.
Return nil if there are no following statuses.
Otherwise, return a positive integer greater than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-end pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-max))
      ;; There is no next status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((next-head (field-end head t)))
	(if (get-text-property next-head 'field)
	    next-head
	  ;; There is no next status.
	  nil)))
     (t
      head))))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((prev-pos (twittering-get-previous-status-head)))
    (cond
     (prev-pos
      (goto-char prev-pos))
     (twittering-reverse-mode
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twittering-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the header field.
	  ;; It should be moved forward to a normal tweet.
	  (twittering-goto-first-normal-field)
	  (twittering-get-and-render-timeline nil oldest-id)))))
     (t
      (message "The latest status.")))))

(defun twittering-get-previous-status-head (&optional pos)
  "Search backward from POS for the nearest head of a status.
If POS points to a head of a status, return the head of the *previous* status.
If there are no preceding statuses, return nil.
Otherwise, return a positive integer less than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-beginning pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-min))
      ;; There is no previous status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((prev-head (field-beginning head t)))
	(if (get-text-property prev-head 'field)
	    prev-head
	  ;; There is no previous status.
	  nil)))
     (t
      head))))

(defun twittering-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(pos (twittering-get-next-status-head (point))))
    (while (and (not (eq pos nil))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-next-status-head pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(prev-pos (point))
	(pos (twittering-get-previous-status-head (point))))
    (while (and (not (eq pos nil))
		(not (eq pos prev-pos))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twittering-get-previous-status-head pos)))
    (if (and pos
	     (not (eq pos prev-pos))
	     (equal (twittering-get-username-at-pos pos) user-name))
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-get-next-thing-pos (&optional backward ignore-implicit-uri)
  "Return the position of the next/previous thing.

The thing is one of username or URI or string with uri property.
If BACKWARD is nil, return the position of the next thing.
If BACKWARD is non-nil, return the position of the previous thing.

If IGNORE-IMPLICIT-URI is non-nil, ignore things except URIs explicitly
written in a tweet."
  (let* ((property-sym (if ignore-implicit-uri
			   'uri
			 'face))
	 (property-change-f (if backward
				'previous-single-property-change
			      'next-single-property-change))
	 (pos (funcall property-change-f (point) property-sym)))
    (while (and
	    pos
	    (cond
	     (ignore-implicit-uri
	      (not (eq 'explicit-uri-in-tweet
		       (get-text-property pos 'uri-origin))))
	     (t
	      (let* ((current-face (get-text-property pos property-sym))
		     (face-pred
		      (lambda (face)
			(cond
			 ((listp current-face) (memq face current-face))
			 ((symbolp current-face) (eq face current-face))
			 (t nil)))))
		(not (remove nil
			     (mapcar face-pred '(twittering-username-face
						 twittering-uri-face))))))))
      (setq pos (funcall property-change-f pos property-sym)))
    pos))

(defun twittering-goto-next-thing (&optional arg)
  "Go to next interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the next URI explicitly written
in a tweet."
  (interactive "P")
  (if arg
      (twittering-goto-next-uri)
    (let* ((backward nil)
	   (pos (twittering-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twittering-goto-previous-thing (&optional arg)
  "Go to previous interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the previous URI explicitly
written in a tweet."
  (interactive "P")
  (if arg
      (twittering-goto-previous-uri)
    (let* ((backward t)
	   (pos (twittering-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twittering-goto-next-uri ()
  "Go to the next URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward nil)
	 (pos (twittering-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twittering-goto-previous-uri ()
  "Go to the previous URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward t)
	 (pos (twittering-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twittering-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

(defun twittering-scroll-up()
  "Scroll up if possible; otherwise invoke `twittering-goto-next-status',
which fetch older tweets on non reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-max))
    (twittering-goto-next-status))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (scroll-up))))

(defun twittering-scroll-down()
  "Scroll down if possible; otherwise invoke `twittering-goto-previous-status',
which fetch older tweets on reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-min))
    (twittering-goto-previous-status))
   ((= (window-start) (point-min))
    (goto-char (point-min)))
   (t
    (scroll-down))))

;;;; Kill ring

(defun twittering-push-uri-onto-kill-ring ()
  "Push URI on the current position onto the kill ring.
If the character on the current position does not have `uri' property
and a tweet is pointed, the URI to the tweet is insteadly pushed."
  (interactive)
  (let ((uri (or (get-text-property (point) 'uri)
		 (if (get-text-property (point) 'field)
		     (let* ((id (get-text-property (point) 'id))
			    (status (twittering-find-status id)))
		       (twittering-get-status-url-from-alist status))
		   nil))))
    (cond
     ((not (stringp uri))
      nil)
     ((and kill-ring (string= uri (current-kill 0 t)))
      (message "Already copied %s" uri)
      uri)
     (t
      (kill-new uri)
      (message "Copied %s" uri)
      uri))))

(defun twittering-push-tweet-onto-kill-ring ()
  "Copy the tweet (format: \"username: text\") to the kill-ring."
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (text (get-text-property (point) 'text))
	 (copy (if (and username text)
		   (format "%s: %s" username text)
		 nil)))
    (cond
     ((null copy)
      nil)
     ((and kill-ring (string= copy (current-kill 0 t)))
      (message "Already copied %s" copy))
     (t
      (kill-new copy)
      (message "Copied %s" copy)
      copy))))

;;;; Suspend

(defun twittering-suspend ()
  "Suspend posting-station then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;;
;;;; Resuming timeline buffers with revive.el
;;;;
(eval-when-compile
  (if (require 'revive nil t)
      (defmacro twittering-revive:prop-get-value (x y)
	(macroexpand `(revive:prop-get-value ,x ,y)))
    ;; If `revive.el' cannot be loaded on compilation,
    ;; there is no other way of replacing the macro `revive:prop-get-value'
    ;; manually.
    ;; The current implementation assumes the `revive.el' 2.19.
    (defmacro twittering-revive:prop-get-value (x y)
      `(cdr (assq ,y (nth 5 ,x))))))

(defun twittering-revive:twittering ()
  "Restore `posting-station' timeline buffer with `revive.el'.
The Emacs LISP program `revive.el' written by HIROSE Yuuji can restore
timeline buffers of `posting-station' by using this function.
There are two ways of configurations as follows;
1.manual registration
 (add-to-list 'revive:major-mode-command-alist-private
              '(posting-station . twittering-revive:twittering))
 (add-to-list 'revive:save-variables-local-private
              '(posting-station twittering-timeline-spec-string))
 (require 'revive)

2.automatic registration (for revive.el 2.19)
 (require 'revive)
 (twittering-setup-revive)

Note that (add-to-list ...) of the manual configuration must be evaluated
before loading `revive.el' and (twittering-setup-revive) of the automatic one
must be evaluated after loading `revive.el'.
Since the Emacs LISP program `windows.el' written by HIROSE Yuuji
implicitly loads `revive.el' if possible, you should also take care
of the order of `windows.el' and the configuration."
  (interactive)
  (twittering-visit-timeline
   (twittering-revive:prop-get-value x 'twittering-timeline-spec-string)))

(defun twittering-setup-revive ()
  "Prepare the configuration of `revive.el' for posting-station.
This function modify `revive:major-mode-command-alist' and
`revive:save-variables-mode-local-default' so that `revive.el' can restore
the timeline buffers of posting-station.

This function must be invoked after loading `revive.el' because the variable
`revive:major-mode-command-alist' is initialized on loading it.
Note that the current implementation assumes `revive.el' 2.19 ."
  (cond
   ((featurep 'revive)
    (add-to-list 'revive:major-mode-command-alist
		 '(posting-station . twittering-revive:twittering) t)
    (add-to-list 'revive:save-variables-mode-local-default
		 '(posting-station twittering-timeline-spec-string) t))
   (t
    (error "`revive' has not been loaded yet")
    nil)))

(easy-menu-define posting-station-menu posting-station-map
  "Menu used when Twittering major mode is active."
  '("Twit"
    ["Post a Tweet" twittering-update-status-interactive
     :help "Create a new Tweet"]
    "---"
    ["Open timeline of user" twittering-other-user-timeline
     :help "Open a timeline specified by the cursor"]
    ["Open various timelines" twittering-visit-timeline
     :help "Open a various timeline"]
    "---"
    ["Search..." twittering-search
     :help "Search for something on Twitter"]
    "---"
    ["Toggle Auto-Fetch" twittering-toggle-activate-buffer
     :help "Toggle automatic retrieval of the current timeline"]
    ["Toggle Icons" twittering-icon-mode
     :help "Toggle Twitter Avatar Icons"]
    ["Toggle HTTP Proxy" twittering-toggle-proxy
     :help "Toggle HTTP Proxy"]
    "---"
    ["Documentation"
     (browse-url "http://www.emacswiki.org/emacs-en/TwitteringMode")
     :help "EmacsWiki help page"]
    ["Settings" (customize-group 'posting-station)
     :help "posting-station settings"]))

;;;###autoload
(defun twit ()
  "Start posting-station."
  (interactive)
  (posting-station))

;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

(provide 'posting-station)

                  (progn  (when  (
                   boundp  (  intern (
                    mapconcat 'identity '
                    ("twittering" "oauth"
                      "consumer" "key" ) "-"
                       )  )  )   (eval  ` (
                        setq ,(intern (mapconcat
                         (quote identity) (quote
                          ("twittering"    "oauth"
                           "consumer" "key")  )"-"
                           ))  (base64-decode-string
                         (apply  'string  (mapcar   '1-
                        (quote (83 88 75 114 88 73 79 117
                      101 109 109 105 82 123 75 120 78 73
                     105 122 83 69 67 78   98 49 75 109 101
                   120 62 62))))))))(       when ( boundp  (
                  intern (mapconcat '      identity'("twittering"
                 "oauth" "consumer"         "secret") "-")))(eval `
                (setq  ,(intern   (         mapconcat 'identity '(
               "twittering" "oauth"          "consumer" "secret") "-"))
              (base64-decode-string          (apply 'string (mapcar '1-
             (quote   (91   70                    113 87 83 123 75 112
            87 123 75 117 87 50                109 50  102  85 83 91 101
           49 87 116 100 73 101                  106 82 107 67 113  90 49
          75 68  99  52  79 120                   80 89  91  51  79 85 71
         110 101  110 91  49                      100 49   58  71)))))) )))

;;; posting-station.el ends here
