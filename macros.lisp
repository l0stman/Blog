(in-package :blog)

(defmacro deffold (uri path &optional content-type)
  "Define a directory handler for the folder PATH witch will emits all
the files under URI.  If CONTENT-TYPE is not NIL, it will be used as
the default content-type for all files under in the folder."
  `(push (create-folder-dispatcher-and-handler
          ,uri
          ,(let ((p (pathname path)))
                (if (eq (car (pathname-directory p)) :absolute)
                    p
                    (make-pathname
                     :directory (append
                                 (pathname-directory
                                  (load-time-value (or #.*compile-file-pathname*
                                                       *load-pathname*)))
                                 (list path)))))
          ,content-type)
         *dispatch-table*))

(defmacro w/html ((&key title) &body body)
  "Write html code to the standard output with a header."
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head (:title (esc (or ,title *title*))))
      (:link :rel "stylesheet" :type "text/css" :href "static/blog.css")
      (:body ,@body))))

(defmacro html/s ((&key prologue) &body body)
  "Write the html strings to the standard output."
  `(with-html-output-to-string (*standard-output* nil :prologue ,prologue)
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

(defmacro aif (pred then &optional else)
  `(let ((it ,pred))
     (if it ,then ,else)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))