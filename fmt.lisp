(in-package :blog)

(defconstant +eol+ (coerce '(#\return #\newline) 'string)
  "String representing an end of line.")
(defconstant +emptyl+ (concatenate 'string +eol+ +eol+)
  "String representing an empty line.")
(defconstant +spc+ "    "
  "Blanks prefixing a code in a text.")

(defmacro case-match ((src start end) &body clauses)
  "CASE-MATCH is a dispatcher like CASE that executes the forms in a
clause if a condition is met.

Each clause has the form:
  \(RE STATEMENT*).

STATEMENT* is executed sequentially if RE is equal to T or if the
regex RE matches the string SRC by scanning between the positions
START and END.  In this case the variables MATCH-START, MATCH-END,
REG-STARTS and REG-ENDS are bound respectively to the positions of the
start of the match and the end of the match, the arrays containing the
start and end positions of the registers in RE."
  (labels ((iter (clauses)
             (when-bind  (cl (car clauses))
               (if (eq (car cl) t)
                   `(progn ,@(cdr cl))  ; default statement
                   `(multiple-value-bind (match-start
                                          match-end
                                          reg-starts
                                          reg-ends)
                        (scan ,(car cl) ,src :start ,start :end ,end)
                      (declare (ignorable match-end reg-starts reg-ends))
                      (if match-start
                          (progn ,@(cdr cl))
                          ,(iter (cdr clauses))))))))
    (iter clauses)))

(defmacro case-prefix ((src start end) &body clauses)
  "CASE-PREFIX is like CASE-MATCH but instead of matching a regex, it
tries to find if a string literal is a prefix of SRC at position
START.  MATCH-END is bound to the position after the prefix in SRC."
  (labels ((iter (clauses)
             (when-bind (cl (car clauses))
               (if (eq (car cl) t)
                   `(progn ,@(cdr cl))
                   `(let ((match-end (+ ,start
                                       ,(if (stringp (car cl))
                                            (length (car cl))
                                            `(length ,(car cl))))))
                      (if (and (<= match-end ,end)
                               (string= ,(car cl) ,src :start2 ,start
                                        :end2 match-end))
                          (progn ,@(cdr cl))
                          ,(iter (cdr clauses))))))))
    (iter clauses)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *syntax-table* (make-array 255 :initial-element NIL)
    "Table containing syntax handler functions for input text."))

(defun sfunc (ch)
  "Return the syntax handler function corresponding to the character
CH."
  (let ((c (char-code ch)))
    (when (< c (length *syntax-table*))
      (aref *syntax-table* c))))

(defmacro defsyn (char &body body)
  "Define a procedure of four arguments as the syntax handler
function associated with the character CHAR.  That procedure should be
called when we encounter CHAR and the position after the last
processed character should be returned.

If BODY is a singleton, it's considered to be the procedure itself.
Otherwise, it should have the form: \(SRC DST START END) STATEMENT*,
where STATEMENT* is the body of the procedure.  The input text is the
substring of SRC between the positions START and END \(CHAR is at the
position START) and the result is written into the stream DST."
  (let ((code (char-code char)))
    (unless (< code (length *syntax-table*))
      (error "couldn't associate a syntax function with ~C" char))
    `(setf (aref *syntax-table* ,code)
           ,(if (cdr body)
                (destructuring-bind (src dst start end) (car body)
                  `(lambda (,src ,dst ,start ,end)
                     (declare (ignorable ,src ,dst ,start ,end))
                     ,@(cdr body)))
                (car body)))))

(defun text->html (src dst start end)
  "Transform the input text string SRC between the positions START and
END to HTML and write it to DST."
  (while (< start end)
    (aif (sfunc (char src start))
         (setq start (funcall it src dst start end))
         (progn
           (princ (char src start) dst)
           (incf start)))))

(defun esc (s d &optional (start 0) (end (length s)))
  "Transform all the special characters in S into HTML entities and
write the result to D."
  (for (i start end)
    (let ((ch (char s i)))
      (if (and (sfunc ch)
               (char/= ch #\return))
          (format d "&#~3,'0d;" (char-code ch))
          (princ ch d)))))

(defun unesc (s d &optional (start 0) (end (length s)))
  "Transform all the HTML entities in S into characters and write the
result to D."
  (while (< start end)
    (multiple-value-bind (mstart mend)
        (scan "&#\\d{3};" s :start start :end end)
      (unless mstart
        (write-sequence s d :start start :end end)
        (return))
      (write-sequence s d :start start :end mstart)
      (princ (code-char
              (parse-integer s :start (+ mstart 2) :end (1- mend)))
             d)
      (setq start mend))))

(declaim (inline esc-str unesc-str))
(defun esc-str (s)
  (with-output-to-string (d)
    (esc s d)))
(defun unesc-str (s)
  (with-output-to-string (d)
    (unesc s d)))

(defun pgraph->html (src dst start end)
  "Transform the paragraphs from the input text string SRC between the
positions START and END to HTML and write the result to DST."
  (if (< start end)
      (multiple-value-bind (match-start match-end)
          (scan "(\\r\\n){2,}" src :start start :end end)
        (flet ((strip (pref pos)
                 (regex-replace-all (format nil "\\r\\n~A" pref) src +eol+
                                    :start pos
                                    :end (or match-start end))))
         (let ((end2 (+ start (length +spc+))))
           (cond ((char= #\> (char src start)) ; blockquote ?
                  (princ "<blockquote>" dst)
                  (let ((q (strip #\> (1+ start))))
                    (write-sequence
                     (regex-replace-all "\\r\\n"
                                        (with-output-to-string (s)
                                          (pgraph->html q s 0 (length q)))
                                        "<br/>")
                     dst))
                  (princ "</blockquote>" dst))
                 ((and (<= end2 end)
                       (string= +spc+ src :start2 start :end2 end2)) ; code?
                  (princ "<pre><code>" dst)
                  (esc (strip +spc+ end2) dst)
                  (princ "</code></pre>" dst))
                 (t
                  (princ "<p>" dst)     ; new paragraph
                  (text->html src dst start (or match-start end))
                  (princ "</p>" dst)))))
        (if match-end
            (pgraph->html src dst match-end end)
            end))
      end))

(defun in-fmt (s)
  "Transform the input text string to HTML."
  (with-output-to-string (d)
    (pgraph->html s d 0 (length s))))

(defsyn #\< (src dst start end)
  (princ "&lt;" dst)
  (1+ start))

(defsyn #\> (src dst start end)
  (princ "&gt;" dst)
  (1+ start))

(defsyn #\' (src dst start end)
  (princ "&#039;" dst)
  (1+ start))

(defsyn #\& (src dst start end)
  (case-match (src (1+ start) end)
    ("^#\\d{3};"                        ; character entity?
     (write-sequence src dst :start start :end match-end)
     match-end)
    (t
     (princ "&amp;" dst)
     (1+ start))))

(defsyn #\" (src dst start end)
  (princ "&quot;" dst)
  (1+ start))

(defun del->html (c tag fn)
  "Return a syntax handler such that CTEXTC is transformed into
<TAG>HTML</TAG>. TEXT should only contain one paragraph. FN is a
handler function that transforms TEXT to HTML."
  (lambda (src dst start end)
    (let (pos next)
      (do ((i (1+ start)))
          ((or (>= i end) (char= c (char src i)))
           (setq pos i
                 next (if (>= i end) i (1+ i))))
        (case-prefix (src i end)
          (+emptyl+                     ; end of paragraph
           (setq next i
                 pos i)
           (return))
          (t (incf i))))
      (format dst "<~A>" tag)
      (funcall fn src dst (1+ start) pos)
      (format dst "</~A>" tag)
      next)))

(defsyn #\_ (del->html #\_ "em" #'text->html))
(defsyn #\* (del->html #\* "strong" #'text->html))
(defsyn #\` (del->html #\` "code" #'esc))

(defsyn #\\ (src dst start end)
  (let ((i (1+ start)))
    (cond ((and (< i end) (sfunc (char src i))) ; special character?
           (princ (char src i) dst)
           (1+ i))
          (t (princ #\\ dst) i))))

(defsyn #\return (src dst start end)
  (case-match (src (1+ start) end)
    ("^\\n(\\r\\n)+"                    ; empty line?
     (princ "</p>" dst)
     (pgraph->html src dst match-end end))
    (t
     (princ #\return dst)
     (1+ start))))

(defsyn #\[ (src dst start end)
  (case-match (src (1+ start) end)
    ("^(.+)\\]\\((.+)\\)"         ; [link](url)?
     (princ "<a href=\"" dst)
     (write-sequence src
                     dst
                     :start (aref reg-starts 1)
                     :end (aref reg-ends 1))
     (princ "\">" dst)
     (text->html src dst (aref reg-starts 0) (aref reg-ends 0))
     (princ "</a>" dst)
     match-end)
    (t
     (princ #\[ dst)
     (1+ start))))

(defsyn #\- (src dst start end)
  (let ((pos (1+ start)))
    (cond ((and (< pos end) (char= #\- (char src pos)))
           (princ "&mdash;" dst)
           (1+ pos))
          (t
           (princ #\- dst)
           pos))))

(defun scan-rtag (tag src start end)
  "TAG is a string containing the name of tags separated by
blanks. For example if TAG is equal to \"a b c\", SCAN-RTAG would
return the positions of the end and beginning of the string
\"</c></b></a>\" in SRC between the positions START and END.  This is
done while maintaining balanced tags.  If these conditions are not
met, NIL is returned."
  (let* ((tags (split " " tag))
         (ltag (format nil "<~{~A>~^<~}" tags))
         (rtag (format nil "</~{~A>~^</~}" (nreverse tags))))
    (declare (string ltag rtag))
    (do ((i start)                      ; position in src
         (ntag 1))                      ; number of opening tags
        ((or (>= i end) (zerop ntag))
         (when (zerop ntag)
           (values i (- i (length rtag)))))
      (case-prefix (src i end)
        (ltag
         (incf ntag)
         (setq i match-end))
        (rtag
         (decf ntag)
         (setq i match-end))
        (t (incf i))))))

(defun amp->text (src dst start end)
  "Transform the HTML string SRC between START and END after an
ampersand and write the result to DST. Return the position
immediately after the HTML entity."
  (case-prefix (src start end)
    ("lt;" (princ #\< dst) match-end)
    ("gt;" (princ #\> dst) match-end)
    ("quot;" (princ #\" dst) match-end)
    ("amp;" (princ #\& dst) match-end)
    ("mdash;" (princ "--" dst) match-end)
    (t
     (case-match (src start end)
       ("^#\\d{3};"                     ; character entity?
        (princ (code-char
                (parse-integer src :start (1+ start) :end (1- match-end)))
               dst)
        match-end)
       (t (princ "\\&" dst) start)))))

(defun lt->text (src dst start end)
  "Transform the HTML string SRC between START and END after a left
angle bracket and write the result to DST.  Return the position
immediately after the closing tag or the position after the bracket if
it doesn't exist."
  (case-prefix (src start end)
    ("em>"
     (multiple-value-bind (after before)
         (scan-rtag "em" src match-end end)
       (princ #\_ dst)
       (html->text src dst match-end (or before end))
       (princ #\_ dst)
       (or after end)))
    ("strong>"
     (multiple-value-bind (after before)
         (scan-rtag "strong" src match-end end)
       (princ #\* dst)
       (html->text src dst match-end (or before end))
       (princ #\* dst)
       (or after end)))
    ("code>"
     (multiple-value-bind (after before)
         (scan-rtag "code" src match-end end)
       (princ #\` dst)
       (unesc src dst match-end (or before end))
       (princ #\` dst)
       (or after end)))
    ("blockquote>"
     (multiple-value-bind (after before)
         (scan-rtag "blockquote" src match-end end)
       (princ #\> dst)
       (write-sequence
        (regex-replace-all "\\r\\n|\\\\<br/\\\\>"
                           (with-output-to-string (q)
                             (html->text src q match-end (or before end)))
                           (format nil "~A>" +eol+))
        dst)
       (if after
           (progn (princ +emptyl+ dst) after)
           end)))
    ("pre><code>"
     (multiple-value-bind (after before)
         (scan-rtag "pre code" src match-end end)
       (princ +spc+ dst)
       (write-sequence
        (regex-replace-all "(?<=\\r\\n)"
                           (with-output-to-string (d)
                             (unesc src d match-end (or before end)))
                           +spc+)
        dst)
       (if after
           (progn (princ +emptyl+ dst) after)
           end)))
    ("p>"
     (multiple-value-bind (after before)
         (scan-rtag "p" src match-end end)
       (html->text src dst match-end (or before end))
       (cond ((and after (< after end))
              (princ +emptyl+ dst)
              after)
             (t end))))
    (t
     (case-match (src start end)
       ("^a href=\"([^\"]+)\">"         ; link?
        (let ((pos (search "</a>" src :start2 match-end :end2 end)))
          (princ #\[ dst)
          (html->text src dst match-end (or pos end))
          (princ "](" dst)
          (write-sequence src
                          dst
                          :start (aref reg-starts 0)
                          :end (aref reg-ends 0))
          (princ #\) dst)
          (if pos (+ pos 4) end)))
       (t
        (princ "\\<" dst)
        start)))))

(defun html->text (src dst start end)
  "Transform the input HTML string SRC between the positions START and
END to ASCII text and write it to DST."
  (while (< start end)
    (let ((c (char src start)))
      (case c
        (#\& (setq start (amp->text src dst (1+ start) end)))
        (#\< (setq start (lt->text src dst (1+ start) end)))
        (otherwise
         (if (and (sfunc c)             ; special character?
                  (char/= c #\return))
             (format dst "\\~C" c)
             (princ c dst))
         (incf start))))))

(defun out-fmt (s)
  "Transform the input HTML string to ASCII."
  (with-output-to-string (d)
    (html->text s d 0 (length s))))
