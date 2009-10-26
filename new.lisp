(in-package :blog)

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer))
  (let ((title "") (body ""))
    (when id
      (let ((p (find-post id)))
	(when p
	  (setf title (title p)
		body (body p)))))
    (with-auth
	(with-html ()	  
	  (:form :method "post" :action "add"
		 (when id
		   (htm (:input :type "hidden" :name "id" :value id)))
		 (:table :border 0
			 (:tr
			  (:td "title")
			  (:td (:input :type "text"
				       :name "title"
				       :size 50
				       :value  title)))
			 (:tr
			  (:td "body")
			  (:td (:textarea :name "body"
					  :rows 10
					  :cols 50 (str body))))
			 (:tr
			  (:td)
			  (:td
			   (:input :type "submit" :value "add post")))))))))

(defun edit-post (id title body)
  (let ((p (find-post id)))
    (cond (p  
	   (setf (title p) title
		 (body p) body))
	  (t (blog-error)))))

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