(in-package :blog)

(define-easy-handler (verify :uri "/verify"
			     :default-request-type :post)
    (user pass uri)
  (cond ((string/= user *user*)
	 (login :msg "Invalid username." :uri uri))
	((not (equalp (hash pass) *hash*))
	 (login :user user :msg "Invalid password." :uri uri))
	(t
	 (update-cookie)
	 (redirect uri))))
