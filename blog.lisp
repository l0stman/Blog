(in-package :blog)

(defun show (post logged-p &key limit-p)
  "Show a post with an optional character limit defined by *maxchar*."
  (html/s ()
   (:div :class "post"
	 (:a :href
	     (conc "view?id=" (write-to-string (id post)))
	     (:span :class "post-title" (str (title post))))
	 (:div :class "post-date" (str (date post)))
	 (:div :class "post-body" (str (if limit-p (stub post) (body post))))
	 (when logged-p
	   (htm
	    (:form :method "post" :action "new" :class "post-edit"
		   (:input :type "hidden" :name "id" :value (id post))
		   (:input :type "submit" :name "action" :value "edit")
		   (:input :type "submit" :name "action" :value "delete")))))))

(defun header (log-p)
  (html/s ()
    (:div :id "header"
          (:div :id "header-left"
                (:img :src "static/feed-icon-14x14.png" :alt "[feed icon]")
                (:a :href "feed" "rss"))
          (:div :id "header-right"
           (if log-p
               (htm
                (:a :href "new" "new") (:span :class "separator" "|")
                (:a :href "admin" "admin") (:span :class "separator" "|")
                (:a :href "logout" "logout"))
               (htm
                (:a :href "login" "login")))))
    (:div :id "title" (:a :href "blog" (esc *title*)))))

(define-easy-handler (blog :uri "/blog"
			   :default-request-type :get)
    ((page :parameter-type 'integer))
  (let ((page (if (and page (> page 0)) page 1))
	(log-p (loggedp)))
    (w/html ()
      (str (header log-p))
      (flet ((link (pred page msg)
               (html/s ()
                 (when pred
                   (htm (:a :class "page"
                            :href (conc "blog?page=" (write-to-string page))
                            (str msg)))))))
	(loop
	   with skip = (* (1- page) *maxpost*)
	   with count = 0
           for post = (find-from *id*) then (find-from (1- (id post)))
	   while (and post (< count *maxpost*))
	   do (cond ((zerop skip)
                     (str (show post log-p :limit-p t))
                     (incf count))
                    (t (decf skip)))
	   finally (let ((pp (> page 1))
			 (pn (and (= count *maxpost*) post)))
		     (str (link pp (1- page) "prev"))
		     (when (and pp pn) (htm (:span :class "separator" "|")))
		     (str (link pn (1+ page) "next"))))))))

(define-easy-handler (view-post :uri "/view"
                                :default-request-type :get)
    ((id :parameter-type 'integer))
  (aif (find-post id)
       (let ((log-p (loggedp)))
         (w/html (:title (title it))
           (str (header log-p))
           (str (show it log-p))))
       (blog-error)))
