(in-package :blog)

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(deffmt in-fmt (s)
  ("(\\r\\n){2,}>((.|\\s)*?)(\\r\\n){2,}"
   (list "<blockquote>"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all ">" (second regs) ""))
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
		       (:td (:textarea :name "body" (str (out-fmt body)))))
		      (:tr
		       (:td)
		       (:td
			(:input :type "submit" :value "add post"))))))))))

(define-easy-handler (add-post :uri "/add"
			       :default-request-type :post)
    (title body (id :parameter-type 'integer))
  (with-auth
      (when (string= title "")
	(setq title "No title"))
    (if id
	(edit-post id title (in-fmt body))
	(ins-post title (in-fmt body)))
    (redirect (redir-url "blog"))))