(defpackage blog-system
  (:use :cl :asdf))

(in-package :blog-system)

(defsystem "blog"
    :description "a simple blog engine."
    :author "rrl <endian.sign@gmail.com>"
    :components ((:file "packages")
		 (:file "fmt" :depends-on ("packages"))
		 (:file "login" :depends-on ("macros"))
		 (:file "verify" :depends-on ("login")) 
		 (:file "params" :depends-on ("fmt")) 
		 (:file "save" :depends-on ("params"))
		 (:file "macros" :depends-on ("save")) 
		 (:file "blog" :depends-on ("macros"))
		 (:file "new" :depends-on ("blog")) 
		 (:file "admin" :depends-on ("save"))
		 (:file "change-login" :depends-on ("save" "login"))) 
    :depends-on (:hunchentoot :cl-who :cl-ppcre :ironclad))