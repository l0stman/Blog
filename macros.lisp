(in-package :blog)

(defvar *css*
  (make-pathname
   :name "blog"
   :type "css"
   :defaults (load-time-value
	      (or #.*compile-file-pathname* *load-pathname*))))

(push (create-static-file-dispatcher-and-handler "/blog.css" *css* "text/css")
      *dispatch-table*)

(defmacro w/html ((&key title) &body body)
  "Write html code to the standard output with a header."
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head (:title (str (or ,title *title*))))
      (:link :rel "stylesheet" :type "text/css" :href "blog.css")
      (:body ,@body))))

(defmacro html/s (&body body)
  "Write the html strings to the standard output."
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))

(defmacro defhand ((name uri &rest args) &body body)
  "Define the function name as a handler for the given uri."
  `(progn
     (defun ,name ,args ,@body)
     (push (create-prefix-dispatcher ,uri ',name)
	   *dispatch-table*)))

(defmacro w/syms (args &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (gensym)))
		 args)
     ,@body))

(defmacro aif (pred &body body)
  `(let ((it ,pred))
     (if it ,@body)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))