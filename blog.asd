(defpackage blog-system
  (:use :cl :asdf))

(in-package :blog-system)

(defsystem "blog"
    :description "a simple blog engine."
    :author "rrl <endian.sign@gmail.com>"
    :components ((:file "packages")
                 (:file "macros" :depends-on ("packages"))
		 (:file "fmt" :depends-on ("packages"))
		 (:file "params" :depends-on ("fmt" "macros"))
		 (:file "login" :depends-on ("macros"))
		 (:file "verify" :depends-on ("login"))
		 (:file "feed" :depends-on ("params"))
		 (:file "save" :depends-on ("login" "params" "feed"))
                 (:file "blog" :depends-on ("login" "params"))
		 (:file "new" :depends-on ("save"))
		 (:file "admin" :depends-on ("save"))
		 (:file "change-login" :depends-on ("save")))
    :depends-on (:hunchentoot :cl-who :cl-ppcre :ironclad))
