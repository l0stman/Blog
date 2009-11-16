(in-package :blog)

(defun show (post &key limitp)
  (html/s
    (:div :class "post"
	  (:a :href
	      (conc "view?id=" (write-to-string (id post)))
	      (:span :class "post-title" (str (title post))))
	  (:div :class "post-date" (str (date post)))
	  (:div :class "post-body" (str (if limitp (stub post) (body post))))
	  (:form :method "post" :action "new" :class "post-edit"
		 (:input :type "hidden" :name "id" :value (id post))
		 (:input :type "submit" :name "action" :value "edit")
		 (:input :type "submit" :name "action" :value "delete")))))

(defun header ()
  (html/s
    (:div :id "header"
	  (:a :href "new" "new")
	  (:span :class "separator" "|")
	  (:a :href "admin" "admin"))
    (:div :id "title" (:a :href "blog" (str *title*)))))

(defmacro link (pred page msg)
  `(html/s
     (when ,pred
      (htm
       (:a :class "page"
	   :href (conc "blog?page=" (write-to-string ,page)) ,msg)))))

(define-easy-handler (blog :uri "/blog"
			   :default-request-type :get)
    ((page :parameter-type 'integer))
  (let* ((page (or page 1))
	 (blog (loop repeat (1+ (* (1- page) *maxpost*))
		  for b on *blog*
		  finally (return b))))
    (w/html () 
      (str (header))
      (loop repeat *maxpost*
	 for post in blog
	 for rest on blog
	 do (str (show post :limitp t))
	 finally (let ((pp (> page 1))
		       (pn (and rest (cdr rest))))
		   (str (link pp (1- page) "prev"))
		   (htm (:span :class "separator" (str (when (and pp pn) "|"))))
		   (str (link pn (1+ page) "next")))))))

(define-easy-handler (view-post :uri "/view"
				:default-request-type :get)
    ((id :parameter-type 'integer))
  (let ((p (find-post id)))
    (cond ((not p) (blog-error))
	  (t (w/html (:title (title p))
	       (str (header))
	       (str (show p)))))))
