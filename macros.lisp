(in-package :blog)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defmacro with-html-str (&body body)
  `(with-html-output-to-string (*standard-output* nil :indent t)
     ,@body))