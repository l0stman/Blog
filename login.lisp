(in-package :blog)

(deftype octet-array () `(simple-array (unsigned-byte 8) (*)))

(defun sto (s)
  "Transform a string to an array of octets."
  (map 'octet-array #'char-code s))

(defun random-octets (size)
  "Return random octets of the given size"
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((a (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (i size a)
        (setf (aref a i) (read-byte in))))))

(defvar *secret-key* (random-octets 32))

(declaim (inline digest trustedp loggedp))

(defun digest (data)
  "Compute the digest of the string data."
  (let ((hmac (make-hmac *secret-key* :sha256)))
    (update-hmac hmac (sto data))
    (hmac-digest hmac)))

(defun trustedp (data digest)
  (let ((digest1 (digest data))
        (res 0))
    (and (= (length digest1) (length digest))
         (dotimes (i (length digest) (zerop res))
           (setq res
                 (logior res
                         (logxor (aref digest i) (aref digest1 i))))))))

(defvar *session-max-time* 3600
  "The time in seconds after which the cookie expires if unused.")

(defun encode-cookie ()
  "Return a string containing the expiration time and its MAC separated by &."
  (let ((data (format nil "exp=~D"
                      (+ (get-universal-time) *session-max-time*))))
    (with-output-to-string (s)
      (format s "~A&digest=" data)
      (loop for ch across (digest data)
           do (princ (code-char ch) s)))))

(defun decode-cookie (c)
  "Return \(VALUES DATA MAC EXPIRATION-TIME)."
  (multiple-value-bind (match regs)     ; digest could contain newlines
      (scan-to-strings "(?s)^(exp=(\\d+))&digest=(.+)" c)
    (when match
      (values (aref regs 0)
              (aref regs 2)
              (parse-integer (aref regs 1))))))

(defvar *ck-name* "auth"
  "Name of the cookie for authentication.")

(declaim (inline update-cookie))
(defun update-cookie ()
  (set-cookie *ck-name* :value (encode-cookie)))

(defhand (logout "/logout") ()
  (set-cookie *ck-name* :expires (1- (get-universal-time)))
  (redirect (or (referer) "/blog")))

(defun loggedp ()
  "Verify if the client is logged in and update its cookie."
  (aif (cookie-in *ck-name*)
       (multiple-value-bind (data digest exp-time) (decode-cookie it)
         (and data
              (trustedp data (sto digest))
              (cond ((< exp-time (get-universal-time))
                     (logout) nil)
                    (t (update-cookie) t))))))

(declaim (inline salt hash))
(defun salt () (random-octets 8))

(defvar *salt* (salt))
(defvar *kdf* (make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256))

(defun hash (pass)
  "Compute the password digest using PBKDF2."
  (derive-key *kdf* (sto pass) *salt* 10000 20))

(defvar *user* "admin")
(defvar *hash* (hash "admin"))

(defmacro w/auth (&rest body)
  "Assure that the access to the resource needs authentication."
  `(if (loggedp)
       (progn ,@body)
       (redirect "/login")))

(define-easy-handler (login :uri "/login"
			    :default-request-type :post)
    (msg uri)
  (w/html ()
    (:form :class "config" :method "post" :action "verify"
	   (:input :type "hidden" :name "uri" :value (or uri (referer)))
	   (:table
	    (if msg (htm (:div :class "message" (str msg))))
	    (:tr
	     (:td "username")
	     (:td (:input :type "text" :name "user")))
	    (:tr
	     (:td "password")
	     (:td (:input :type "password" :name "pass"))))
	   (:div :class "submit" (:input :type "submit" :value "login")))))