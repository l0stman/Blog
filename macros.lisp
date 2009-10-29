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

(defmacro deffmt (name (s &key start end) &body body)
  (flet ((call (fn) `(,s ,(if fn `(,fn ,s) s))))
   `(defun ,name (,s)
      (let*
	  (,(call start)
	   ,@(loop for p in body collect
		  `(,s (regex-replace-all ,(first p) ,s ,@(cdr p))))
	    ,(call end)) 
	s))))
