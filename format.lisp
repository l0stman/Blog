(in-package :blog)

(defmacro deffmt (name (s) &body body)
  `(defun ,name (,s)
     (let*
	 ,(loop for p in body collect
	       `(,s (regex-replace-all ,(first p) ,s ,(second p))))
       s)))

(deffmt in-fmt (s)
  ("(\\r\\n){2,}" "<p>")
  ("\\*([^*]*)\\*" "<strong>\\1</strong>")
  ("_([^_]*)_" "<em>\\1</em>")
  ("\\[([^]]+)\\]\\(([^)]+)\\)" "<a href=\"\\2\">\\1</a>"))

(deffmt out-fmt (s)
  ("<p>" (format nil "~a~a" #\return #\return))
  ("</?strong>" "*")
  ("</?em>" "_")
  ("<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)"))
