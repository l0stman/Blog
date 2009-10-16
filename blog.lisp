(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 140)

(defun salt ()
  (with-output-to-string (s)
    (loop repeat 5
       do (princ (code-char (random 128)) s))))

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
    :reader id
    :initform (incf *id*))
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
  (push (make-instance 'post :title title :body body)
	*blog*))

(defun excerpt (post &key limitp)
  (with-html-str
    (:div :class "post"
	  (:a :href
	      (conc "view?id="
		    (write-to-string (id post)))
	      (:span :class "post-title" (str (title post))))
	  (:div :class "post-date" (str (date post)))
	  (:div :class "post-body"
		(let ((b (body post)))
		  (esc (if limitp
			   (handler-case
			       (conc (subseq b 0 *maxchar*) "...")
			     (error () b))
			   b))))
	  (:div :class "post-edit"
		(:form :method "post" :action "new"
		       (:input :type "hidden" :name "id" :value (id post))
		       (:input :type "submit" :value "edit post"))))))

(defun blog ()
  (with-html
    (:html
     (:head (:title (str *title*)))
     (:body
      (loop for p in *blog*
	 do (str (excerpt p :limitp t)))))))

(push (create-prefix-dispatcher "/blog" 'blog)
      *dispatch-table*)

(defun find-post (id)
  (find-if #'(lambda (p)
	       (= (id p) id))
	   *blog*))

(defun blog-error ()
  (setf (return-code*) +http-not-found+)
  nil)

(define-easy-handler (view-post :uri "/view"
				:default-request-type :get)
    ((id :parameter-type 'integer))
  (let ((p (find-post id)))
    (cond ((not p) (blog-error))
	  (t (with-html
	       (:html
		(:head (:title (str (title p))))
		(:body (str (excerpt p)))))))))