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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *syntax-table* (make-array 255 :initial-element NIL)
    "Table containing syntax handler functions for input text."))

(defun sfunction (ch)
  "Return the syntax handler function corresponding to the character
CH."
  (let ((c (char-code ch)))
    (when (< c (length *syntax-table*))
      (aref *syntax-table* c))))

(defmacro defsyn (char &body body)
  "Associate a procedure of four arguments as the syntax handler
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
  (loop
     with i = start
     while (< i end)
     do (aif (sfunction (char src i))
             (setq i (funcall it src dst i end))
             (progn
               (princ (char src i) dst)
               (incf i)))))

(defun esc (s d &optional (start 0) (end (length s)))
  "Transform all the special characters in S into HTML entities and
write the result to D."
  (loop
     for i = start then (1+ i)
     while (< i end)
     do (let ((ch (char s i)))
          (if (and (sfunction ch)
                   (char/= ch #\return))
              (format d "&#~3,'0d;" (char-code ch))
              (princ ch d)))))

(defun unesc (s d &optional (start 0) (end (length s)))
  "Transform all the HTML entities in S into characters and write the
result to D."
  (loop
     with i = start
     while (< i end)
     do (multiple-value-bind (mstart mend)
            (scan "&#\\d{3};" s :start i :end end)
          (unless mstart
            (write-sequence s d :start i :end end)
            (return))
          (write-sequence s d :start i :end mstart)
          (princ (code-char
                  (parse-integer s :start (+ mstart 2) :end (1- mend)))
                 d)
          (setq i mend))))

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
<TAG>HTML</TAG>. TEXT should only contain one paragraph. FN is handler
function that transform TEXT to HTML."
  (lambda (src dst start end)
    (let (pos next)
      (do ((i (1+ start)))
          ((or (>= i end) (char= c (char src i)))
           (setq pos i
                 next (if (>= i end) i (1+ i))))
        (let ((end1 (+ i (length +emptyl+))))
          (cond ((and (<= end1 end )
                      (string= src +emptyl+ :start1 i :end1 end1))
                 ;; end of paragraph
                 (setq next i
                       pos  i)
                 (return))
                (t (incf i)))))
      (format dst "<~A>" tag)
      (funcall fn src dst (1+ start) pos)
      (format dst "</~A>" tag)
      next)))

(defsyn #\_ (del->html #\_ "em" #'text->html))
(defsyn #\* (del->html #\* "strong" #'text->html))
(defsyn #\` (del->html #\` "code" #'esc))

(defsyn #\\ (src dst start end)
  (let ((i (1+ start)))
    (cond ((and (< i end) (sfunction (char src i))) ; special character?
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

(defun scan-tag (tag src start end)
  "TAG is a string containing the name of tags separated by
blanks. For example if TAG is equal to \"a b c\", SCAN-TAG would
return the positions just before and after the string \"</c></b></a>\"
in SRC between the positions START and END.  This is done while
maintaining balanced tags.  If these conditions are not met, NIL is
returned."
  (let* ((tags (split " " tag))
         (lanchor (format nil "^<~{~A>~^<~}" tags))
         (ranchor (format nil "^</~{~A>~^</~}" (nreverse tags))))
    (do ((i start)                      ; position in src
         (ntag 1))                      ; number opening tags
        ((or (>= i end) (zerop ntag))
         (when (zerop ntag)
           (values i (- i (length ranchor) -1))))
      (case-match (src i end)
        (lanchor
         (incf ntag)
         (setq i match-end))
        (ranchor
         (decf ntag)
         (setq i match-end))
        (t (incf i))))))

(defun amp->text (src dst start end)
  "Transform the HTML string SRC between START and END after an
ampersand and write the result to DST. Return the position
immediately after the HTML entity."
  (case-match (src start end)
    ("^lt;" (princ #\< dst) match-end)
    ("^gt;" (princ #\> dst) match-end)
    ("^quot;" (princ #\" dst) match-end)
    ("^amp;" (princ #\& dst) match-end)
    ("^mdash;" (princ "--" dst) match-end)
    ("^#\\d{3};"                        ; character entity?
     (princ (code-char
             (parse-integer src :start (1+ start) :end (1- match-end)))
            dst)
     match-end)
    (t (princ #\& dst) start)))

(defun lt->text (src dst start end)
  "Transform the HTML string SRC between START and END after a left
angle bracket and write the result to DST.  Return the position
immediately after the closing tag in any or after the bracket."
  (case-match (src start end)
    ("^em>"
     (multiple-value-bind (after before)
         (scan-tag "em" src match-end end)
       (princ #\_ dst)
       (html->text src dst match-end (or before end))
       (princ #\_ dst)
       (or after end)))
    ("^strong>"
     (multiple-value-bind (after before)
         (scan-tag "strong" src match-end end)
       (princ #\* dst)
       (html->text src dst match-end (or before end))
       (princ #\* dst)
       (or after end)))
    ("^code>"
     (multiple-value-bind (after before)
         (scan-tag "code" src match-end end)
       (princ #\` dst)
       (unesc src dst match-end (or before end))
       (princ #\` dst)
       (or after end)))
    ("^blockquote>"
     (multiple-value-bind (after before)
         (scan-tag "blockquote" src match-end end)
       (princ #\> dst)
       (with-output-to-string (q)
         (html->text src q match-end (or before end))
         (write-sequence
          (regex-replace-all "\\r\\n|\\\\<br/\\\\>"
                             (get-output-stream-string q)
                             (format nil "~A>" +eol+))
          dst))
       (if after
           (progn (princ +emptyl+ dst) after)
           end)))
    ("^pre><code>"
     (multiple-value-bind (after before)
         (scan-tag "pre code" src match-end end)
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
    ("^a href=\"([^\"]+)\">"
     (multiple-value-bind (after before)
         (scan-tag "a" src match-end end)
       (princ #\[ dst)
       (html->text src dst match-end (or before end))
       (princ "](" dst)
       (write-sequence src
                       dst
                       :start (aref reg-starts 0)
                       :end (aref reg-ends 0))
       (princ #\) dst)
       (or after end)))
    ("^p>"
     (multiple-value-bind (after before)
         (scan-tag "p" src match-end end)
       (html->text src dst match-end (or before end))
       (cond ((and after (< after end))
              (princ +emptyl+ dst)
              after)
             (t end))))
    (t
     (princ "\\<" dst)
     start)))

(defun html->text (src dst start end)
  "Transform the input HTML string SRC between the positions START and
END to ASCII text and write it to DST."
  (loop
     with i = start
     while (< i end)
     do (let ((c (char src i)))
          (case c
            (#\& (setq i (amp->text src dst (1+ i) end)))
            (#\< (setq i (lt->text src dst (1+ i) end)))
            (otherwise
             (if (and (sfunction c)     ; special character?
                      (char/= c #\return))
                 (format dst "\\~C" c)
                 (princ c dst))
             (incf i))))))

(defun out-fmt (s)
  "Transform the input HTML string to ASCII."
  (with-output-to-string (d)
    (html->text s d 0 (length s))))
