(in-package :blog)

(defun add-post (id title body uri)
  "Add a new post or edit an existing one."
  (when (string= title "")
    (setq title "No title"))
  (cond (id
         (edit-post id title body)
         (redirect uri))
        (t
         (ins-post title body)
         (redirect "/blog")))
  (save-blog))

(defun fmt-help ()
  "Text formatting help."
  (html/s ()
    (:table
     (:tr (:td "_emphasize_") (:td (:em "emphasize")))
     (:tr (:td "*strong*") (:td (:strong "strong")))
     (:tr (:td "> blockquote") (:td (:blockquote "blockquote")))
     (:tr
      (:td "[google](http://www.google.com)")
      (:td (:a :href "http://www.google.com" "google")))
     (:tr (:td "foo -- bar") (:td "foo &mdash; bar")))))

(defun new-form (&key id title body uri)
  "Form to add or edit a post."
  (w/html ()
    (:form :class "config" :method "post" :action "new"
	   (when id
	     (htm (:input :type "hidden" :name "id" :value id)))
           (:input :type "hidden" :name "uri" :value uri)
	   (:div :class "post-title" (str (in-fmt (or title ""))))
	   (:div :class "post-body" (str (in-fmt (or body ""))))
	   (:table
	    (:tr
	     (:td "title")
	     (:td (:input :type "text" :name "title"
			  :value (escape-string title))))
	    (:tr
	     (:td "body")
	     (:td (:textarea :name "body" (str body))))
	    (:tr
	     (:td)))
	   (:div :class "submit"
		 (:input :type "submit" :name "action" :value "add")
		 (:span :class "separator" " ")
		 (:input :type "submit"  :name "action" :value "view"))
	   (str (fmt-help)))))

(define-easy-handler (del-conf :uri "/delete"
                               :default-request-type :post)
    ((id :parameter-type 'integer) delete-p uri)
  "Ask confirmation before deleting a post."
  (w/auth
   (cond ((not delete-p)
          (w/html ()
            (:form :method "post" :action "delete"
                   (:div :class "message"
                         (fmt "Do you really want to delete post ~D?" id))
                   (:input :type "hidden" :name "id" :value id)
                   (:input :type "hidden" :name "uri" :value uri)
                   (:div :class "submit"
                         (:input :type "submit" :name "delete-p" :value "yes")
                         (:span :class "separator" " ")
                         (:input :type "submit" :name "delete-p"
                                 :value "no")))))
         (t (when (string= delete-p "yes")
              (delete-post id)
              (save-blog))
            (redirect uri)))))

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer)
     title
     body
     action
     (uri :init-form (referer)))
  (w/auth
   (cond ((string= action "delete") (del-conf :id id :uri uri))
         ((string= action "add") (add-post id title body uri))
         (t
          (cond ((string= action "view")
                 (setf title (post-parameter "title")
                       body (post-parameter "body")))
                (id
                 (let ((p (find-post id)))
                   (when p
                     (setf title (out-fmt (title p))
                           body (out-fmt (body p)))))))
          (new-form :id id :title title :body body :uri uri)))))
