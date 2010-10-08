(in-package :cl-user)

(defpackage :blog
  (:use :hunchentoot :cl-who :cl :cl-ppcre)
  (:shadow :defconstant
           :*session-max-time*)
  (:import-from :ironclad
                :make-hmac
                :update-hmac
                :hmac-digest
                :derive-key
                :make-kdf)
  (:export :save-blog
           :load-blog))