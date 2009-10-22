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
  '(*title* *id* *maxchar* *maxpost* *user* *salt* *hash* *proxy-uri*))

(defun save-blog ()
  (with-open-file (out (ensure-directories-exist *db*)
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package :blog)))
	(let ((*print-readably* nil))
	  (format out "#.(progn ~{(setq ~s ~s)~^ ~})"
		  (loop for sym in *params*
		     append `(,sym ,(symbol-value sym)))))
	(let ((*print-readably* t))
	  (print *blog* out))))))

(defun load-blog ()
  (with-open-file (in *db*)
    (with-standard-io-syntax
      (let* ((*package* (find-package :blog)))
	(read in)
	(setq *blog* (read in))))))