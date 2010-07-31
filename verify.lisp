(in-package :blog)

(define-easy-handler (verify :uri "/verify"
			     :default-request-type :post)
    (user pass uri)
  (cond ((or (string/= user *user*)
             (not (equalp (hash pass) *hash*)))
	 (login :msg "Invalid username or password." :uri uri))
	(t
	 (update-cookie)
	 (redirect uri))))
