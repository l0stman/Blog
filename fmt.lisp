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

(defun in-fmt (s)
    "Transform the ASCII string to HTML by escaping characters."
    (escape-string s))

(defun esc-html (src)
  "Escape all special HTML characters in the string SRC."
  (with-output-to-string (dst)
    (loop
       with i = 0
       while (< i (length src))
       do (let ((delta 1))
            (case (aref src i)
              ((#\<) (princ "&lt;" dst))
              ((#\>) (princ "&gt;" dst))
              ((#\') (princ "&#039;" dst))
              ((#\&)
               (multiple-value-bind (mstart mend)
                   (scan "^#\\d{3};" src :start (1+ i))
                 (if mstart             ; character entity?
                     (progn
                       (write-sequence src dst :start i :end mend)
                       (setq delta (- mend i)))
                     (princ "&amp;" dst))))
              ((#\") (princ "&quot;" dst))
              (otherwise (princ (aref src i) dst)))
            (incf i delta)))))

(deffmt out-fmt (s)
    "Transform back the HTML string to ASCII and unescape special characters."
  "&lt;" "<"
  "&gt;" ">"
  "&quot;" "\""
  "&(#x?[\\da-fA-F]+);"
  (#'(lambda (m r1)
       (declare (ignore m))
       (let ((n (if (char= (elt r1 1) #\x) 0 1)))
         (format nil "~a" (code-char (read-from-string (subseq r1 n))))))
     :simple-calls t))

