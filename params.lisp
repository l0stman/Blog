(in-package :blog)

(deffold "/static/" "static")

(defvar *blog* (make-hash-table))
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 320)
(defvar *maxpost* 10)

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
    :initform (get-universal-time))))

(defun sys-time (ut)
  (multiple-value-bind
        (second minute hour date month year) (decode-universal-time ut)
    (format nil
            "~2,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            month
            date
            year
            hour
            minute
            second)))

(defun univ-time (ut)
  (multiple-value-bind
        (sec min hour date mon year day daylight-p tz)
      (decode-universal-time ut)
    (declare (ignore daylight-p))
    (format nil
            "~A, ~2,'0d ~A ~A ~2,'0d:~2,'0d:~2,'0d ~A~2,'0D00"
            (aref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
            date
            (aref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                  (1- mon))
            year
            hour
            min
            sec
            (if (minusp tz) '+ '-)
            (abs tz))))

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
  (loop for id from n downto 1
     when (find-post id) return it))

(defun edit-post (id title body)
  (aif (find-post id)
       (setf (title it) title
             (body it) (in-fmt body)
             (stub it) (in-fmt (excerpt body)))
       (blog-error)))

(defun delete-post (id) (remhash id *blog*))
