(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 140)
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
(defun header ()
  (with-html-str
    (:div :id "header"
	  (:span (:a :href "new" "new"))
	  (:span :class "separator" "|")
	  (:span (:a :href "admin" "admin")))
    (:div :id "title" (:a :href "blog"(str *title*)))))

(define-easy-handler (blog :uri "/blog"
			   :default-request-type :get)
    ((page :parameter-type 'integer))
  (let* ((page (or page 1))
	 (blog (loop repeat (1+ (* (1- page) *maxpost*))
		  for b on *blog*
		  finally (return b))))
    (with-html
      (:html
       (:head (:title (str *title*)))
       (:body
	(str (header))
	(loop repeat *maxpost*
	   for post in blog
	   for rest on blog
	   do (str (excerpt post :limitp t))
	   finally (when (and rest (cdr rest))
		     (htm
		      (:a :href
			  (conc "blog?page="
				(write-to-string (1+ page)))
			  "Next >>")))))))))

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
		(:body (str (header))
		       (str (excerpt p)))))))))