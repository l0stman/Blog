(in-package :blog)

(defhand (admin "/admin" &key
		(title *title*)
		(maxchar *maxchar*)
		(maxpost *maxpost*)
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
		  ("Number of characters for a post excerpt"
		   "maxchar" ,maxchar)
		  ("Number of posts per page" "maxpost" ,maxpost))
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
     (maxchar :parameter-type 'integer)
     (maxpost :parameter-type 'integer))
  (w/auth
   (cond ((or (not maxchar)
	      (not maxpost)
	      (<= maxchar 0)
	      (<= maxpost 0))
	  (admin
	   :msg "The numbers should be positive:"
	   :title title
	   :maxchar (or maxchar *maxchar*)
	   :maxpost (or maxpost *maxpost*)))
	 (t
	  (setf *title* title 
		*maxchar* maxchar
		*maxpost* maxpost)
	  (save-blog)
	  (redirect "/blog")))))
