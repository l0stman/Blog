(in-package :blog)

(defvar *css*
  (make-pathname
   :name "blog"
   :type "css"
   :defaults (load-time-value
	      (or #.*compile-file-pathname* *load-pathname*))))

(push (create-static-file-dispatcher-and-handler "/blog.css" *css* "text/css")
      *dispatch-table*)