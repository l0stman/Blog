(in-package :blog)

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(deffmt in-fmt (s)
  ("(^(\\r\\n)?|(\\r\\n){2,})>((.|\\s)*?)((\\r\\n){2,}|$)"
   (list "<blockquote>"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all ">" (fourth regs) ""))
	 "</blockquote>")
   :simple-calls t)
  ("(\\r\\n){2,}" "<p>")
  ("\\*([^*]*)\\*" "<strong>\\1</strong>")
  ("_([^_]*)_" "<em>\\1</em>")
  ("\\[([^]]+)\\]\\(([^)]+)\\)" "<a href=\"\\2\">\\1</a>"))

(deffmt out-fmt (s)
  ("<p>" *ret*)
  ("<blockquote>((.|\\s)*?)</blockquote>" 
   (list *ret* ">"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "(?<=\\r\\n)" (first regs) ">"))
	 *ret*)
   :simple-calls t)
  ("</?strong>" "*")
  ("</?em>" "_")
  ("<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)"))

(defun add-post (id title body) 
  (when (string= title "")
    (setq title "No title"))
  (if id
      (edit-post id title (in-fmt body))
      (ins-post title (in-fmt body)))
  (redirect (redir-url "blog")))

(define-easy-handler (new-post :uri "/new"
			       :default-request-type :post)
    ((id :parameter-type 'integer) title body action)
  (with-auth
      (cond ((string= action "delete")
	     (delete-post id)
	     (redirect (redir-url "blog")))
	    ((string= action "add") (add-post id title body)) 
	    (t
	     (cond ((string= action "view")
		    (setf title (post-parameter "title")
			  body (post-parameter "body")))
		   (id
		    (let ((p (find-post id)))
		      (when p
			(setf title (title p)
			      body (body p)))))) 
	     (with-html ()	  
	       (:form :method "post" :action "new" 
		      (when id
			(htm (:input :type "hidden" :name "id" :value id))) 
		      (:div :class "post-title" (str title))
		      (:div :class "post-body" (str (in-fmt body)))
		      (:table
		       (:tr
			(:td "title")
			(:td (:input :type "text" :name "title" :value  title)))
		       (:tr
			(:td "body")
			(:td (:textarea :name "body" (str (out-fmt body)))))
		       (:tr
			(:td)
			(:td
			 (:input :type "submit" :name "action" :value "add")
			 (:span :class "separator" " ")
			 (:input :type "submit"  :name "action"
				 :value "view"))))))))))


