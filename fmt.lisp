(in-package :blog)

(defmacro deffmt (name (s &key start end) &body body)
  (flet ((call (fn) `(,s ,(if fn `(,fn ,s) s))))
   `(defun ,name (,s)
      (let*
	  (,(call start)
	   ,@(loop for p in body collect
		  `(,s (regex-replace-all ,(first p) ,s ,@(cdr p))))
	    ,(call end)) 
	s))))

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(deffmt in-fmt (s :start escape-string)
  ("(^(\\r\\n)?|(\\r\\n){2,})&gt;((.|\\s)*?)((\\r\\n){2,}|$)"
   (list "<blockquote>"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "&gt;" (fourth regs) ""))
	 "</blockquote>")
   :simple-calls t)
  ("(\\r\\n){2,}" "<p>")
  ("\\*([^*]*)(\\*|$)" "<strong>\\1</strong>")
  ("_([^_]*)(_|$)" "<em>\\1</em>")
  ("\\[([^]]+)\\]\\(([^)]+)(\\)|$)" "<a href=\"\\2\">\\1</a>")
  ("--" "&mdash;"))

(deffmt unesc (s)
  ("&lt;" "<") ("&gt;" ">") ("&quot;" "\"")
  ("&(#x?[\\da-fA-F]+);"
   #'(lambda (m r1)
       (declare (ignore m))
       (let ((n (if (char= (elt r1 1) #\x) 0 1)))
	(format nil "~a" (code-char (read-from-string (subseq r1 n))))))
   :simple-calls t))

(deffmt out-fmt (s :end unesc)
  ("<p>" *ret*)
  ("<blockquote>((.|\\s)*?)</blockquote>" 
   (list *ret* ">"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "(?<=\\r\\n)" (first regs) ">"))
	 *ret*)
   :simple-calls t)
  ("</?strong>" "*")
  ("</?em>" "_")
  ("<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)")
  ("&mdash;" "--"))
