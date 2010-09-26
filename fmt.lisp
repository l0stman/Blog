(in-package :blog)

(defvar *eol* (coerce '(#\return #\newline) 'string))
(defvar *emptyl* (concatenate 'string *eol* *eol*))

(defmacro case-match ((src start end) &body clauses)
  (labels ((iter (clauses)
             (when clauses
               (let ((cl (car clauses)))
                 (if (eq (car cl) t)
                     `(progn ,@(cdr cl)) ; default statement
                     `(multiple-value-bind (match-start
                                            match-end
                                            reg-starts
                                            reg-ends)
                          (scan ,(car cl) ,src :start ,start :end ,end)
                        (declare (ignorable match-end reg-starts reg-ends))
                        (if match-start
                            (progn ,@(cdr cl))
                            ,(iter (cdr clauses)))))))))
    (iter clauses)))

(defvar *syntax-table* (make-array 255 :initial-element NIL)
  "Table containing syntax handler functions for input text.")

(defun sfunction (ch)
  "Return the syntax handler function corresponding to the character
CH."
  (let ((c (char-code ch)))
    (when (< c (length *syntax-table*))
      (aref *syntax-table* c))))

(defmacro defsyn (char (src dst start end) &body body)
  "Define the procedure of four arguments SRC, DST, START and END and
whose body is BODY as the syntax handler function associated with
CHAR.  That procedure should be called when we encounter the character
CHAR and the position after the last processed character should be
returned.  The input text is the substring of SRC between the
positions START and END (CHAR is at the position START.)  The result
is written in the stream DST."
  (let ((code (char-code char)))
    (unless (< code (length *syntax-table*))
      (error "couldn't associate a syntax function with ~C" char))
    `(setf (aref *syntax-table* ,code)
           (lambda (,src ,dst ,start ,end) ,@body))))

(defun text->html (src dst start end)
  "Transform the input text string SRC between the positions START and
END to HTML and write it to DST."
  (loop
     with i = start
     while (< i end)
     do (aif (sfunction (aref src i))
             (setq i (funcall it src dst i end))
             (progn
               (princ (aref src i) dst)
               (incf i)))))

(defun in-fmt (s)
  "Transform the input text string to HTML."
  (with-output-to-string (d)
    (text->html s d 0 (length s))))

(defsyn #\< (src dst start end)
  (declare (ignore src end))
  (princ "&lt;" dst)
  (1+ start))

(defsyn #\' (src dst start end)
  (declare (ignore src end))
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
  (declare (ignore src end))
  (princ "&quot;" dst)
  (1+ start))

(defun del->html (c ltag rtag)
  "Return a syntax handler such that C<text>C is transformed into
LTAG<text>RTAG. <text> should be contained in one paragraph."
  (lambda (src dst start end)
    (let (pos next)
      (do ((i (1+ start)))
          ((or (>= i end) (char= c (aref src i)))
           (setq pos i
                 next (if (>= i end) i (1+ i))))
        (case-match (src i end)
          ("^(\\r\\n){2,}"              ; end of paragraph?
           (setq next i
                 pos  i)
           (return))
          (t (incf i))))
      (princ ltag dst)
      (text->html src dst (1+ start) pos)
      (princ rtag dst)
      next)))

(defsyn #\_ (src dst start end)
  (funcall (del->html #\_ "<em>" "</em>") src dst start end))

(defsyn #\* (src dst start end)
  (funcall (del->html #\* "<strong>" "</strong>") src dst start end))

(defsyn #\\ (src dst start end)
  (let ((i (1+ start)))
    (cond ((and (< i end) (sfunction (aref src i))) ; special character?
           (princ (aref src i) dst)
           (1+ i))
          (t (princ #\\ dst) i))))

(defsyn #\> (src dst start end)
  (if (or (zerop start)                 ; beginning of string?
          (and (> start 1)              ; after any empty line?
               (string= *emptyl*
                        src
                        :start2 (- start (length *emptyl*))
                        :end2 start)))
      (multiple-value-bind (match-start match-end)
          (scan "(\\r\\n){2,}" src :start (1+ start) :end end) ; empty line?
        (princ "<blockquote>" dst)
        (text->html src dst (1+ start) (or match-start end))
        (princ "</blockquote>" dst)
        (or match-end end))
      (progn
        (princ "&gt;" dst)
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

(defun scan-tag (tag src start end)
  "Return the positions in the string SRC immediately after and before
the closing tag corresponding to TAG while maintaining balanced
tags or NIL if these conditions are not met."
  (do ((i start)                        ; position in src
       (ntag 1)                         ; number opening tags
       (lanchor (format nil "^<~A>" tag))
       (ranchor (format nil "^</~A>" tag)))
      ((or (>= i end) (zerop ntag))
       (when (zerop ntag)
         (values i (- i (length tag) 3))))
    (case-match (src i end)
      (lanchor
       (incf ntag)
       (setq i match-end))
      (ranchor
       (decf ntag)
       (setq i match-end))
      (t (incf i)))))

(defun amp->text (src dst start end)
  "Transform the HTML string SRC between START and END after an
ampersand and write the result to DST. Return the position
immediately after the HTML entity."
  (case-match (src start end)
    ("^lt;" (princ #\< dst) match-end)
    ("^gt;" (princ #\> dst) match-end)
    ("^quot;" (princ #\" dst) match-end)
    ("^amp;" (princ #\& dst) match-end)
    ("^#039;" (princ #\' dst) match-end)
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
    ("^blockquote>"
     (multiple-value-bind (after before)
         (scan-tag "blockquote" src match-end end)
       (princ #\> dst)
       (html->text src dst match-end (or before end))
       (if after
           (progn (princ *emptyl* dst) after)
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
    (t
     (princ #\< dst)
     start)))

(defun html->text (src dst start end)
  "Transform the input HTML string SRC between the positions START and
END to ASCII text and write it to DST."
  (loop
     with i = start
     while (< i end)
     do (let ((c (aref src i)))
          (case c
            (#\& (setq i (amp->text src dst (1+ i) end)))
            (#\< (setq i (lt->text src dst (1+ i) end)))
            (otherwise
             (if (sfunction c)          ; special character?
                 (format dst "\\~C" c)
                 (princ c dst))
             (incf i))))))

(defun out-fmt (s)
  "Transform the input HTML string to ASCII."
  (with-output-to-string (d)
    (html->text s d 0 (length s))))
