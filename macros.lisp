(in-package :blog)

(defmacro deffold (uri path &optional content-type)
  "Define a directory handler for the folder PATH witch will emits all
the files under URI.  If CONTENT-TYPE is not NIL, it will be used as
the default content-type for all files in the folder."
  `(push (create-folder-dispatcher-and-handler
          ,uri
          ,(let ((p (pathname path)))
                (if (eq (car (pathname-directory p)) :absolute)
                    p
                    (let ((dir (or #.*compile-file-truename* *load-truename*)))
                      (make-pathname
                       :directory (append (pathname-directory dir)
                                          (list path))))))
          ,content-type)
         *dispatch-table*))

(defmacro html/s ((&key prologue) &body body)
  "Write the html strings to the standard output."
  `(with-html-output-to-string (*standard-output* nil :prologue ,prologue)
     ,@body))

(defmacro w/html ((&key title (css "static/blog.css")) &body body)
  "Write html code to the standard output with a header."
  `(html/s (:prologue t)
     (:html
      (:head (:title (esc (or ,title *title*))))
      (:link :rel "stylesheet" :type "text/css" :href ,css)
      (:body ,@body))))

(defmacro defhand ((name uri &key case-p) args &body body)
  "Define the function NAME with arguments ARGS as a handler for the
given URI.  If CASE-P is true, the generated HTML is case-sensitive.
In the latter case, beware of the side-effect of setting
*DOWNCASE-TOKENS-P* to NIL when expanding this macro.  This won't be
restored until the code is compiled."
  (let ((downcase-p *downcase-tokens-p*))
    (when case-p
      (setq *downcase-tokens-p* nil))
    `(progn
       (defun ,name ,args ,@body)
       (push (create-prefix-dispatcher ,uri ',name)
             *dispatch-table*)
       ,@(when case-p `((setq *downcase-tokens-p* ,downcase-p))))))

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