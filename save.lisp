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

(defmethod print-object ((table hash-table) s)
  (if *print-readably*
      (format s "#.~S"
              `(let ((table (make-hash-table)))
                 ,(aif (loop for k being the hash-keys in table
                          using (hash-value v) nconc `((gethash ,k table) ,v))
                       `(setf ,@it))
                 table))
      (call-next-method)))

(defparameter *params*
  '(*title* *id* *maxchar* *maxpost* *user* *salt* *hash* *htoot-url*
    *maxfeed* *rss-description*))

(defun save-blog ()
  (with-open-file (out (ensure-directories-exist *db*)
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package :blog)))
	(let ((*print-readably* nil))
          (flet ((to-octets (obj)
                   `(coerce ,obj '(simple-array (unsigned-byte 8) (*)))))
            (format out "#.~S"
                    `(setq
                      ,@(loop for sym in *params* append
                             `(,sym ,(if (eq sym '*salt*)
                                         (to-octets (symbol-value sym))
                                         (symbol-value sym))))))))
	(let ((*print-readably* t))
          (print *blog* out))))))

(defun load-blog ()
  (with-open-file (in *db*)
    (with-standard-io-syntax
      (let* ((*package* (find-package :blog)))
	(read in)
	(setq *blog* (read in))))))


(when (probe-file *db*)
  (load-blog))