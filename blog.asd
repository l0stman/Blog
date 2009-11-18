(defpackage blog-system
  (:use :cl :asdf))

(in-package :blog-system)

(defsystem "blog"
    :description "a simple blog engine."
    :author "rrl <endian.sign@gmail.com>"
    :components ((:file "packages") 
		 (:file "fmt" :depends-on ("packages")) 
		 (:file "verify" :depends-on ("login")) 
		 (:file "params" :depends-on ("fmt"))
		 (:file "macros" :depends-on ("params"))
		 (:file "login" :depends-on ("macros"))
		 (:file "save" :depends-on ("params" "login")) 
		 (:file "blog" :depends-on ("login" "params"))
		 (:file "new" :depends-on ("blog")) 
		 (:file "admin" :depends-on ("save"))
		 (:file "change-login" :depends-on ("save"))) 
    :depends-on (:hunchentoot :cl-who :cl-ppcre :ironclad))