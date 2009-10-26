(in-package :blog)

(defun admin (&key
	      (title *title*)
	      (maxchar *maxchar*)
	      (maxpost *maxpost*)
	      (msg "Enter the new values:"))
  (with-auth
      (with-html () 
	(:div :class "message" (str msg))
	(:form :id "admin" :method "post" :action "update"
	       (:table
		(loop
		   for (desc name value type) in
		   `(("Blog title" "title" ,(escape-string title)) 
		     ("Username" "username" ,(escape-string user))
		     ("Password" "password" ,(escape-string pass) "password")
		     ("Number of characters for post excerpt"
		      "maxchar" ,maxchar)
		     ("Number of posts per page" "maxpost" ,maxpost))
		   do (htm
		       (:tr
			(:td (str desc))
			(:td (:input :type (or type "text")
				     :name name 
				     :value value)))))
		(:tr
		 (:td)
		 (:td
		  (:input :type "submit" :value "update"))))))))

(push (create-prefix-dispatcher "/admin" 'admin)
      *dispatch-table*)

(define-easy-handler (update :uri "/update"
			     :default-request-type :post)
    (title
     username
     password
     (maxchar :parameter-type 'integer)
     (maxpost :parameter-type 'integer))
  (with-auth
      (cond ((or (string= password "")
		 (not maxchar)
		 (not maxpost)
		 (<= maxchar 0)
		 (<= maxpost 0))
	     (admin
	      :msg "The numbers should be positive and the password non-empty:"
	      :title title
	      :maxchar (or maxchar *maxchar*)
	      :maxpost (or maxpost *maxpost*)))
	    (t
	     (setf *title* title
		   *user* username
		   *salt* (salt)
		   *hash* (hash password)
		   *maxchar* maxchar
		   *maxpost* maxpost)
	     (save-blog)
	     (redirect (redir-url "blog"))))))