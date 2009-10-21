(defpackage blog-system
  (:use :cl :asdf))

(in-package :blog-system)

(defsystem "blog"
    :description "a simple blog engine."
    :author "rrl <endian.sign@gmail.com>"
    :components ((:file "packages")
		 (:file "macros" :depends-on ("packages"))
		 (:file "blog" :depends-on ("packages" "macros"))
		 (:file "new" :depends-on ("packages" "macros" "blog"))
		 (:file "save" :depends-on ("packages" "blog"))
		 (:file "admin" :depends-on ("packages" "macros" "blog" "save")))
    :depends-on (:hunchentoot :cl-who :md5))