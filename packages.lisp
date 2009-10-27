(in-package :cl-user)

(defpackage :blog
  (:use hunchentoot :cl-who :cl :md5 :cl-ppcre)
  (:export
   :save-blog
   :load-blog))