(in-package :blog)

(defmacro defconstant (sym val &optional doc)
  "Make sure that VAL is only evaluated once."
  `(cl:defconstant ,sym (if (boundp ',sym) (symbol-value ',sym) ,val)
     ,@(when doc (list doc))))

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

(defmacro w/xml ((&key (prologue "<?xml version=\"1.0\"?>")) &body body)
  "Write the xml strings to the standard output."
  `(with-output-to-string (*standard-output*)
     ,(let (*downcase-tokens-p*)
        (macroexpand
         `(with-html-output (*standard-output* nil :prologue ,prologue)
            ,@body)))))

(defmacro defhand ((name uri) args &body body)
  "Define the function NAME with arguments ARGS as a handler for the
given URI."
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