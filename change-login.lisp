(in-package :blog)

(defhand (reset "/reset" &key (user *user*) (msg "Enter the new values:"))
  (w/auth
   (w/html ()
     (if msg (htm (:div :class "message" (str msg))))
     (:form :method "post" :action "/verify-login"
	    (:table
	     (:tr
	      (:td "Username")
	      (:td (:input :type "text" :name "user" :value user)))
	     (:tr
	      (:td "Enter the new password")
	      (:td (:input :type "password" :name "pass")))
	     (:tr
	      (:td "Re-enter the password")
	      (:td (:input :type "password" :name "pass2")))
	     (:tr
	      (:td)
	      (:td (:input :type "submit" :value "update"))))))))

(define-easy-handler (verify-login :uri "/verify-login"
				:default-request-type :post)
    (user pass pass2)
  (w/auth
   (cond ((string= pass "")
	  (reset :user user :msg "The password couldn't be empty:"))
	 ((not (string= pass pass2))
	  (reset :user user :msg "Passwords mismatch:"))
	 (t
	  (setf *user* user
		*salt* (salt)
		*hash* (hash pass))
	  (save-blog)
	  (redirect "/blog")))))