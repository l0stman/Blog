(defpackage blog-system
  (:use :cl :asdf))

(in-package :blog-system)

(defsystem "blog"
    :description "a simple blog engine."
    :author "rrl <endian.sign@gmail.com>"
    :components ((:file "packages")
		 (:file "params" :depends-on ("packages"))
		 (:file "format" :depends-on ("packages"))
		 (:file "macros" :depends-on ("packages" "params"))
		 (:file "save" :depends-on ("packages" "params"))
		 (:file "css" :depends-on ("packages"))
		 (:file "blog" :depends-on ("packages" "macros" "css" "format"))
		 (:file "new" :depends-on ("packages" "macros" "blog" "format")) 
		 (:file "admin" :depends-on ("packages" "macros" "blog" "save")))
    :depends-on (:hunchentoot :cl-who :md5 :cl-ppcre))