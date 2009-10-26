(in-package :blog)

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer) action)
  (with-auth
      (if (string= action "delete")
	  (progn (delete-post id) (redirect (redir-url "blog"))) 
	  (let ((title "") (body ""))
	    (when id
	      (let ((p (find-post id)))
		(when p
		  (setf title (title p)
			body (body p))))) 
	    (with-html ()	  
	      (:form :id "post" :method "post" :action "add"
		     (when id
		       (htm (:input :type "hidden" :name "id" :value id)))
		     (:table
		      (:tr
		       (:td "title")
		       (:td (:input :type "text" :name "title" :value  title)))
		      (:tr
		       (:td "body")
		       (:td (:textarea :name "body" (str body))))
		      (:tr
		       (:td)
		       (:td
			(:input :type "submit" :value "add post"))))))))))

(defun edit-post (id title body)
  (let ((p (find-post id)))
    (cond (p  
	   (setf (title p) title
		 (body p) body))
	  (t (blog-error)))))

(defun delete-post (id)
  (labels ((id= (p) (= id (id p)))
	   (drop (posts)
	     (cond ((null posts) nil)
		   ((id= (cadr posts))
		    (setf (cdr posts) (cddr posts)))
		   (t (drop (cdr posts))))))
    (cond ((null *blog*) nil)
	  ((id= (car *blog*)) (setq *blog* (cdr *blog*)))
	  (t (drop *blog*)))))

(define-easy-handler (add-post :uri "/add"
			       :default-request-type :post)
    (title body (id :parameter-type 'integer))
  (with-auth
      (when (string= title "")
	(setq title "No title"))
    (if id
	(edit-post id title body)
	(ins-post title body))
    (redirect (redir-url "blog"))))