(in-package :blog)

(defhand (admin "/admin" &key
		(title *title*)
		(maxchar *maxchar*)
		(maxpost *maxpost*)
                (maxfeed *maxfeed*)
                (rss-desc *rss-description*)
                (htoot-url (htoot-url))
		(msg "Enter the new values:"))
  (w/auth
   (w/html ()
     (:form :class "config" :method "post" :action "update"
	    (:div :class "message" (str msg))
	    (:table
	     (:tr
	      (:td "Reset login information")
	      (:td (:a :href "reset" "reset")))
	     (loop
		for (desc name value) in
		`(("Blog title" "title" ,(escape-string title))
		  ("Number of characters in a post excerpt"
		   "maxchar" ,maxchar)
		  ("Number of posts per page" "maxpost" ,maxpost)
                  ("RSS feed description" "rss-desc" ,rss-desc)
                  ("Number of items in the RSS feed" "maxfeed" ,maxfeed)
                  ("URL for accessing Hunchentoot" "htoot-url" ,htoot-url))
		do (htm
		    (:tr
		     (:td (str desc))
		     (:td (:input :type "text"
				  :name name
				  :value value))))))
	    (:div :class "submit"
		  (:input :type "submit" :value "update"))))))

(define-easy-handler (update :uri "/update"
			     :default-request-type :post)
    (title
     rss-desc
     htoot-url
     (maxchar :parameter-type 'integer)
     (maxpost :parameter-type 'integer)
     (maxfeed :parameter-type 'integer))
  (w/auth
   (cond ((or (not maxchar)
	      (not maxpost)
              (not maxfeed)
	      (<= maxchar 0)
	      (<= maxpost 0)
              (<= maxfeed 0))
	  (admin
	   :msg "The numbers should be positive:"
	   :title title
	   :maxchar (or maxchar *maxchar*)
	   :maxpost (or maxpost *maxpost*)
           :maxfeed (or maxfeed *maxfeed*)
           :rss-desc rss-desc
           :htoot-url htoot-url))
	 (t
	  (setf *title* title
		*maxchar* maxchar
		*maxpost* maxpost
                *maxfeed* maxfeed
                *rss-description* rss-desc
                *htoot-url* htoot-url)
	  (save-blog)
	  (redirect "/blog")))))
