(in-package :blog)

(defmacro deffmt (name (s &key start end) docstring &body plist)
  "Define the function NAME that applies a regex based transformation
to the string S. PLIST is a property list of regex and replacement
string passed successively to `regex-replace-all'.  If supplied, the
function START and the function END are applied respectively before
and after the above transformations."
  (flet ((call (fn) (if fn `((,s (,fn ,s)))))
         (mklist (o) (if (listp o) o (list o))))
    `(defun ,name (,s)
       ,docstring
       (let*
	   (,@(call start)
	    ,@(loop for (re repmt) on plist by #'cddr collect
		   `(,s (regex-replace-all ,re ,s ,@(mklist repmt))))
	      ,@(call end))
	 s))))

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(deffmt in-fmt (s :start escape-string)
    "Transform the ASCII string to HTML by escaping characters."
  "(?s)(?:^|(?:\\r\\n){2,})&gt;(.*?)((\\r\\n){2,}|$)"
  ((list "<blockquote>"
         #'(lambda (m &rest regs)
             (declare (ignore m))
             (regex-replace-all "\\r\\n&gt;" (first regs) "<br>"))
         "</blockquote>")
   :simple-calls t)
  "(\\r\\n){2,}" "<p>"
  "\\*([^*]*)(\\*|$)" "<strong>\\1</strong>"
  "_([^_]*)(_|$)" "<em>\\1</em>"
  "\\[([^]]+)\\]\\(([^)]+)(\\)|$)" "<a href=\"\\2\">\\1</a>"
  "--" "&mdash;")

(deffmt unesc (s)
    "Reverse the result of escape-string."
  "&lt;" "<"
  "&gt;" ">"
  "&quot;" "\""
  "&(#x?[\\da-fA-F]+);"
  (#'(lambda (m r1)
       (declare (ignore m))
       (let ((n (if (char= (elt r1 1) #\x) 0 1)))
         (format nil "~a" (code-char (read-from-string (subseq r1 n))))))
     :simple-calls t))

(deffmt out-fmt (s :end unesc)
    "Transform back the HTML string to ASCII and unescape special characters."
  "<p>" *ret*
  "(?s)<blockquote>(.*?)</blockquote>"
  ((list *ret* ">"
	 #'(lambda (m &rest regs)
	     (declare (ignore m))
	     (regex-replace-all "<br>"
				(first regs)
				(coerce '(#\return #\newline #\>) 'string)))
	 *ret*)
   :simple-calls t)
  "</?strong>" "*"
  "</?em>" "_"
  "<a +href=\"([^\"]*)\">([\\w ]+)</a>" "[\\2](\\1)"
  "&mdash;" "--")
