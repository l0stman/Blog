(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 140)

(defclass post ()
  ((title
    :initarg :title
    :reader title)
   (body
    :initarg :body
    :reader body)
   (id
    :initarg :id
    :reader id)
   (date
    :reader date
    :initform (sys-time))))

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

(defun ins-post (title body)
  (incf *id*)
  (push (make-instance 'post :title title :body body :id *id*)
	*blog*))

(defun excerpt (post &optional maxchar)
  (with-html-str
    (:div :class "post"
	  (:a :href
	      (conc "view?id="
		    (write-to-string (id post)))
	      (:div :class "post-title" (str (title post))))
	  (:div :class "post-date" (str (date post)))
	  (:div :class "post-body"
		(let ((b (body post)))
		  (str (handler-case
			   (subseq b 0 maxchar)
			 (error () b))))))))

(defun blog ()
  (with-html
    (:html
     (:head (:title (str *title*)))
     (:body
      (loop for p in *blog*
	 do (str (excerpt p *maxchar*)))))))

(push (create-prefix-dispatcher "/blog" 'blog)
      *dispatch-table*)

(defun find-post (id)
  (find-if #'(lambda (p)
	       (= (id p) id))
	   *blog*))

(define-easy-handler (view-post :uri "/view"
				:default-request-type :get)
    ((id :parameter-type 'integer))
  (let ((p (find-post id)))
    (cond ((not p)
	   (setf (return-code*) +http-not-found+)
	   nil)
	  (t (with-html
	       (:html
		(:head (:title (str (title p))))
		(:body (str (excerpt p)))))))))