(in-package :blog)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defmacro with-html-str (&body body)
  `(with-html-output-to-string (*standard-output* nil :indent t)
     ,@body))

(defmacro with-auth (&rest body)
  `(multiple-value-bind
	 (user pass) (authorization)
     (cond ((and (string= user *user*)
		 (equalp (hash pass) *hash*))
	    ,@body)
	   (t (require-authorization *title*)))))