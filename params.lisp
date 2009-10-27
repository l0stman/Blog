(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 320)
(defvar *maxpost* 10)
(defvar *proxy-uri* "/")

(defun redir-url (s)
  (concatenate 'string *proxy-uri* s))

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

(defclass post ()
  ((title
    :initarg :title
    :reader title
    :writer (setf title))
   (body
    :initarg :body
    :reader body
    :writer (setf body))
   (id
    :initarg :id
    :reader id
    :initform (incf *id*))
   (date
    :initarg :date
    :reader date
    :initform (sys-time))))

(defun ins-post (title body)
  (push (make-instance 'post :title title :body body)
	*blog*))

(defun find-post (id)
  (find-if #'(lambda (p) (= (id p) id)) *blog*))

(defun edit-post (id title body)
  (let ((p (find-post id)))
    (cond (p  
	   (setf (title p) title
		 (body p) body))
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
