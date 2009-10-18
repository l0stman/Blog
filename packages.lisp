(in-package :cl-user)

(defpackage :blog
  (:use hunchentoot :cl-who :cl :md5)
  (:export
   :save-blog
   :load-blog))