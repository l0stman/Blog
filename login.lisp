(in-package :blog)

(defmacro w/octets (size i val)
  "Return an array of octets of the given size whose i-th element is val."
  (w/syms (a len) 
    `(let* ((,len ,size) 
	    (,a (make-array ,len :element-type '(unsigned-byte 8))))
       (dotimes (,i ,len)
	 (setf (aref ,a ,i) ,val))
       ,a)))

(defun sto (s)
  "Transform a string to an array of octets."
  (w/octets (length s) i (char-code (aref s i))))

(defun random-octets (size)
  "Return random octets of the given size"
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    (w/octets size i (read-byte in))))

(defvar *secret-key* (random-octets 32))

(proclaim '(inline digest trustedp loggedp expiredp))

(defun digest (data)
  "Compute the digest of the string data."
  (let ((hmac (ironclad:make-hmac *secret-key* :sha256))) 
    (ironclad:update-hmac hmac (sto data))
    (ironclad:hmac-digest hmac)))

(defun trustedp (data digest)
  (equalp (digest data) digest))

(defvar *session-max-time* 3600
  "The time in seconds after which the cookie expires if unused.")

(defun expiredp (x)
  (< (+ x *session-max-time*) (get-universal-time)))

(defun encode-cookie ()
  "Return a string containing the current time and its MAC separated by &."
  (let ((time (write-to-string (get-universal-time))))
    (with-output-to-string (s)
      (format s "~a&" time)
      (loop for ch across (digest time)
	   do (princ (code-char ch) s)))))

(defun decode-cookie (c)
  "Return the values of the creation time of the cookie and its MAC."
  (let ((p (position #\& c :test #'char=)))
    (and p
	 (values (subseq c 0 p)
		 (sto (subseq c (1+ p)))))))

(defun loggedp ()
  "Verify if the client is logged in."
  (aif (cookie-in "t")
    (multiple-value-bind (time digest) (decode-cookie it) 
      (and time
	   (trustedp time digest)))))

(define-easy-handler (login :uri "/login"
			    :default-request-type :post)
    (user msg uri)
  (w/html ()
    (if msg (htm (:div :class "message" (str msg))))
    (:form :method "post" :action "/verify"
	   (:input :type "hidden" :name "uri" :value (or uri (request-uri*)))
	   (:table
	    (:tr
	     (:td "username")
	     (:td (:input :type "text" :name "user" :value user)))
	    (:tr
	     (:td "password")
	     (:td (:input :type "password" :name "pass")))
	    (:tr
	     (:td)
	     (:td (:input :type "submit" :value "login")))))))