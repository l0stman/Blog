(in-package :blog)

(defmacro with-html ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head (:title (str ,(or title *title*))))
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