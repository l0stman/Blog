(in-package :blog)

(defun new () 
  (with-auth
      (with-html
	(:html
	 (:head (:title (str *title*)))
	 (:body
	  (:form :method "post" :action "add"
		 (:table :border 0
			 (:tr
			  (:td "title")
			  (:td (:input :type "text" :name "title" :size 50)))
			 (:tr
			  (:td "body")
			  (:td (:textarea :name "body" :rows 10 :cols 50)))
			 (:tr
			  (:td)
			  (:td
			   (:input :type "submit" :value "add post"))))))))))

(push (create-prefix-dispatcher "/new" 'new)
      *dispatch-table*)

(define-easy-handler (add-post :uri "/add"
			       :default-request-type :post)
    (title body)
  (with-auth
      (when (string= title "")
	(setq title "No title"))
    (ins-post title body)
    (redirect "blog")))