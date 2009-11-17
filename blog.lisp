(in-package :blog)

(defun show (post logged-p &key limit-p)
  "Show a post with an optional character limit defined by *maxchar*."
  (html/s
    (:div :class "post"
	  (:a :href
	      (conc "view?id=" (write-to-string (id post)))
	      (:span :class "post-title" (str (title post))))
	  (:div :class "post-date" (str (date post)))
	  (:div :class "post-body" (str (if limit-p (stub post) (body post))))
	  (if logged-p
	      (htm
	       (:form :method "post"
		      :action "new"
		      :class "post-edit"
		      (:input :type "hidden" :name "id" :value (id post))
		      (:input :type "submit" :name "action" :value "edit")
		      (:input :type "submit" :name "action" :value "delete")))))))

(defun header (log-p)
  (html/s
    (:div :id "header"
	  (if log-p
	      (htm
	       (:a :href "new" "new") (:span :class "separator" "|")
	       (:a :href "admin" "admin") (:span :class "separator" "|")
	       (:a :href "logout" "logout"))
	      (htm
	       (:a :href "login" "login"))))
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
  (let* ((page (if (and page (> page 0)) page 1))
	 (blog (nthcdr (* (1- page) *maxpost*) *blog*))
	 (log-p (loggedp))) 
    (w/html () 
      (str (header log-p))
      (loop repeat *maxpost*
	 for post in blog
	 for rest on blog
	 do (str (show post log-p :limit-p t))
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
	  (t (let ((log-p (loggedp)))
	       (w/html (:title (title p))
		 (str (header log-p))
		 (str (show p log-p))))))))
