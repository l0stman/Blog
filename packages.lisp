(in-package :cl-user)

(defpackage :blog
  (:use hunchentoot :cl-who :cl :md5 :cl-ppcre)
  (:import-from :ironclad
		:make-hmac
		:update-hmac
		:hmac-digest
		:digest-sequence)
  (:export
   :save-blog
   :load-blog))