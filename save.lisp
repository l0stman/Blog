(in-package :blog)

(defvar *db* (merge-pathnames "blog/blog.db" (user-homedir-pathname)))

(defmethod print-object ((p post) s)
  (if *print-readably*
      (with-slots (title body id date) p
	(format s
		"#.(make-instance 'post :title ~S :body ~S :id ~d :date ~S)"
		title
		body
		id
		date))
      (call-next-method)))

(defparameter *params*
  '(*title* *id* *maxchar* *maxpost* *user* *salt* *hash*))

(defmacro save-conf (stream)
  `(format ,stream "(~@{~s ~s ~})"
	   ,@(loop for s in *params*
		append `(',s ,s))))

(defun save-blog ()
  (with-open-file (out (ensure-directories-exist *db*)
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package :blog)))
	(let ((*print-readably* nil))
	  (save-conf out))
	(let ((*print-readably* t))
	  (print *blog* out))))))

(defun load-blog ()
  (with-open-file (in *db*)
    (with-standard-io-syntax
      (let* ((*package* (find-package :blog))
	    (conf (read in)))
	(loop for sym in *params*
	     do (setq sym (getf conf sym)))
	(setq *blog* (read in))))))