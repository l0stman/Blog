(in-package :blog)

(defmacro deffmt (name (s &key start end) &body body)
  "Apply a regex based transformation to the string."
  (flet ((call (fn) (if fn `((,s (,fn ,s))))))
    `(defun ,name (,s)
       (let*
	   (,@(call start)
	    ,@(loop for p in body collect
		   `(,s (regex-replace-all ,(first p) ,s ,@(cdr p))))
	      ,@(call end))
	 s))))

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

;; Transform the ascii string to html by escaping characters.
(deffmt in-fmt (s :start escape-string)
  ("(?s)^\\s*(.*)\\s*$" "\\1")
  ("(?s)(?:^|(?:\\r\\n){2,})&gt;(.*?)((\\r\\n){2,}|$)"
   (list "<blockquote>"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "\\r\\n&gt;" (first regs) "<br>"))
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

;; Transform back the html string to ascii and unescape special characters.
(deffmt out-fmt (s :end unesc)
  ("<p>" *ret*)
  ("(?s)<blockquote>(.*?)</blockquote>"
   (list *ret* ">"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "<br>"
				(first regs)
				(coerce '(#\return #\newline #\>) 'string)))
	 *ret*)
   :simple-calls t)
  ("</?strong>" "*")
  ("</?em>" "_")
  ("<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)")
  ("&mdash;" "--"))
