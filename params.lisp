(in-package :blog)

(defvar *blog* (make-hash-table))
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 320)
(defvar *maxpost* 10)

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
    :accessor title)
   (body
    :initarg :body
    :accessor body)
   (stub
    :initarg :stub
    :accessor stub)
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
  (let ((p (make-post title body)))
    (setf (gethash (id p) *blog*) p)))

(defun find-post (id) (gethash id *blog*))

(defun find-from (n)
  (and (> n 0)
       (aif (find-post n) it (find-from (1- n)))))

(defun edit-post (id title body)
  (aif (find-post id)
       (setf (title it) title
             (body it) (in-fmt body)
             (stub it) (in-fmt (excerpt body)))
       (blog-error)))

(defun delete-post (id) (remhash id *blog*))
