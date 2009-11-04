(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 320)
(defvar *maxpost* 10)

(defun salt ()
  (let* ((size 5)
	 (s (make-array size
			:fill-pointer 0
			:element-type 'character)))
    (loop repeat size
       do (vector-push (code-char (random 128)) s)
       finally (return s))))

(defvar *salt* (salt))

(defun hash (pass)
  (md5sum-sequence (concatenate 'string *salt* pass)))

(defvar *user* "admin")
(defvar *hash* (hash "admin"))

(defun sys-time ()
    (multiple-value-bind
	  (second minute hour date month year) (get-decoded-time)
      (format nil
	      "~2,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	      date
	      month
	      year
	      hour
	      minute
	      second)))

(defclass post ()
  ((title
    :initarg :title
    :reader title
    :writer (setf title))
   (body
    :initarg :body
    :reader body
    :writer (setf body))
   (stub
    :initarg :stub
    :reader stub
    :writer (setf stub))
   (id
    :initarg :id
    :reader id
    :initform (incf *id*))
   (date
    :initarg :date
    :reader date
    :initform (sys-time))))

(defun make-post (title body)
  (make-instance 'post
		 :title title
		 :body (in-fmt body)
		 :stub (in-fmt (excerpt body))))

(defun excerpt (text)
  (if (array-in-bounds-p text (1- *maxchar*))
      (format nil "~a..." (subseq text 0 *maxchar*))
      text))

(defun blog-error ()
  (setf (return-code*) +http-not-found+)
  nil)

(defun ins-post (title body)
  (push (make-post title body) *blog*))

(defun find-post (id)
  (find-if #'(lambda (p) (= (id p) id)) *blog*))

(defun edit-post (id title body)
  (let ((p (find-post id)))
    (cond (p  
	   (setf (title p) title
		 (body p) (in-fmt body)
		 (stub p) (in-fmt (excerpt body))))
	  (t (blog-error)))))

(defun delete-post (id)
  (flet ((id= (p) (= id (id p))))
    (labels ((drop (posts)
	       (cond ((null (cdr posts)) nil)
		     ((id= (cadr posts))
		      (setf (cdr posts) (cddr posts)))
		     (t (drop (cdr posts))))))
      (cond ((null *blog*) nil)
	    ((id= (car *blog*)) (setq *blog* (cdr *blog*)))
	    (t (drop *blog*))))))
