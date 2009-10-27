(in-package :blog)

(defvar *css*
  (make-pathname
   :name "blog"
   :type "css"
   :defaults (load-time-value
	      (or #.*compile-file-pathname* *load-pathname*))))

(push (create-static-file-dispatcher-and-handler "/blog.css" *css* "text/css")
      *dispatch-table*)

(defmacro with-html ((&key title) &body body)  
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head (:title (str (or ,title *title*))))
      (:link :rel "stylesheet" :type "text/css" :href "blog.css")
      (:body ,@body))))

(defmacro with-html-str (&body body)
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))

(defmacro with-auth (&rest body)
  `(multiple-value-bind
	 (user pass) (authorization)
     (cond ((and (string= user *user*)
		 (equalp (hash pass) *hash*))
	    ,@body)
	   (t (require-authorization *title*)))))

(defmacro deffmt (name (s) &body body)
  `(defun ,name (,s)
     (let*
	 ,(loop for p in body collect
	       `(,s (regex-replace-all ,(first p) ,s ,(second p))))
       s)))

(deffmt in-fmt (s)
  ("(\\r\\n){2,}" "<p>")
  ("\\*([^*]*)\\*" "<strong>\\1</strong>")
  ("_([^_]*)_" "<em>\\1</em>")
  ("\\[([^]]+)\\]\\(([^)]+)\\)" "<a href=\"\\2\">\\1</a>"))

(deffmt out-fmt (s)
  ("<p>" (format nil "~a~a" #\return #\return))
  ("</?strong>" "*")
  ("</?em>" "_")
  ("<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)"))
