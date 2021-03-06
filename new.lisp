(in-package :blog)

(defun add-post (id title body uri)
  "Add a new post or edit an existing one."
  (when (string= title "")
    (setq title "No title"))
  (if id
      (edit-post id title body)
      (ins-post title body))
  (save-blog)
  (redirect (if id uri "/blog")))

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
     (:tr (:td "foo -- bar") (:td "foo &mdash; bar"))
     (:tr (:td "entity &amp;#248;") (:td "entity &#248;"))
     (:tr (:td "inline code `a &lt; 10`")
          (:td "inline code <code>a &lt; 10</code>"))
     (:tr
      (:td "Lines starting with four spaces <br/>are treated like code:<br/>"
           (:pre
            "&nbsp;&nbsp;&nbsp;&nbsp;if i &lt; 10:"
            (:br "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;print \"Hello world!\"")))
      (:td "Lines starting with four spaces <br/>are treated like code:<br/>"
           (:pre "if i &lt; 10:<br/>&nbsp;&nbsp;print \"Hello world!\""))))))

(defun new-form (&key id title body uri)
  "Form to add or edit a post."
  (w/html ()
    (:form :class "config" :method "post" :action "new"
	   (when id
	     (htm (:input :type "hidden" :name "id" :value id)))
           (:input :type "hidden" :name "uri" :value uri)
	   (:div :class "post-title" (esc (or title "")))
	   (:div :class "post-body" (str (in-fmt (or body ""))))
	   (:table
	    (:tr
	     (:td "title")
	     (:td (:input :type "text" :name "title"
			  :value (esc title))))
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

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer)
     title
     body
     action
     (uri :init-form (referer)))
  (w/auth
   (cond ((string= action "add")
          (add-post id title body uri))
         ((string= action "view")
          (new-form :id id
                    :title (or title (post-parameter "title"))
                    :body (or body (post-parameter "body"))
                    :uri uri))
         ((string= action "edit")
          (aif (find-post id)
               (new-form :id id
                         :title (unesc-str (title it))
                         :body (out-fmt (body it))
                         :uri uri)
               (blog-error)))
         (t (new-form)))))
