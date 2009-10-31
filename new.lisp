(in-package :blog)

(defun add-post (id title body) 
  (when (string= title "")
    (setq title "No title"))
  (if id
      (edit-post id title body)
      (ins-post title body))
  (save-blog)
  (redirect "/blog"))

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer) title body action)
  (with-auth
      (cond ((string= action "delete")
	     (delete-post id)
	     (redirect "/blog"))
	    ((string= action "add") (add-post id title body)) 
	    (t
	     (cond ((string= action "view")
		    (setf title (post-parameter "title")
			  body (post-parameter "body")))
		   (id
		    (let ((p (find-post id)))
		      (when p
			(setf title (out-fmt (title p))
			      body (out-fmt (body p))))))) 
	     (with-html ()	  
	       (:form :method "post" :action "new" 
		      (when id
			(htm (:input :type "hidden" :name "id" :value id))) 
		      (:div :class "post-title" (str (in-fmt title)))
		      (:div :class "post-body" (str (in-fmt body)))
		      (:table
		       (:tr
			(:td "title")
			(:td (:input :type "text" :name "title"
				     :value (escape-string title))))
		       (:tr
			(:td "body")
			(:td (:textarea :name "body" (str body))))
		       (:tr
			(:td)
			(:td
			 (:input :type "submit" :name "action" :value "add")
			 (:span :class "separator" " ")
			 (:input :type "submit"  :name "action"
				 :value "view"))))))))))
