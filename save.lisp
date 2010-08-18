(in-package :blog)

(defvar *db* (merge-pathnames "blog/blog.db" (user-homedir-pathname)))

(defmethod print-object ((p post) s)
  (if *print-readably*
      (with-slots (title body id date stub) p
	(format
	 s
	 "#.(make-instance 'post :title ~S :body ~S :id ~d :date ~S :stub ~S)"
	 title
	 body
	 id
	 date
	 stub))
      (call-next-method)))

(defparameter *params*
  '(*title* *id* *maxchar* *maxpost* *user* *salt* *hash* *htoot-url*
    *maxfeed* *rss-description*))

(defun save-blog ()
  (with-open-file (out (ensure-directories-exist *db*)
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (let* ((*package* (find-package :blog))
             (*print-readably* t))
	(princ "#.(setf " out)
        (dolist (sym *params*)
          (format out "~S ~S "
                  sym
                  (let ((val (symbol-value sym)))
                    (if (typep val 'octet-array)
                        `(coerce ,(coerce val 'simple-vector) 'octet-array)
                        val))))
        (format out "*blog* ~S "
                `(make-hash-table
                  :test ',(hash-table-test *blog*)
                  :size ,(hash-table-size *blog*)
                  :rehash-size ,(hash-table-rehash-size *blog*)
                  :rehash-threshold ,(hash-table-rehash-threshold *blog*)))
        (maphash #'(lambda (id post)
                     (format out "(gethash ~S *blog*) ~S " id post))
                 *blog*)
        (princ #\) out)))))

(defun load-blog ()
  (with-open-file (in *db*)
    (with-standard-io-syntax
      (let* ((*package* (find-package :blog)))
	(read in)))))


(when (probe-file *db*)
  (load-blog))
