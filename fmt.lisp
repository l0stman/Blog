(in-package :blog)

(defvar *eol* (coerce '(#\return #\newline) 'string))
(defvar *emptyl* (concatenate 'string *eol* *eol*))

(declaim (inline specialp))
(defun specialp (ch) (find ch "*_\\>"))

(defmacro case-match ((src start end) &body clauses)
  (labels ((iter (clauses)
             (when clauses
               (let ((cl (car clauses)))
                 (if (eq (car cl) t)
                     `(progn ,@(cdr cl)) ; default statement
                     `(multiple-value-bind (match-start match-end)
                          (scan ,(car cl) ,src :start ,start :end ,end)
                        (declare (ignorable match-end))
                        (if match-start
                            (progn ,@(cdr cl))
                            ,(iter (cdr clauses)))))))))
    (iter clauses)))

(defvar *esc-table* (make-array 255 :initial-element NIL))

(defun esc-function (ch)
  "Return the escape function corresponding to the character CH."
  (let ((c (char-code ch)))
    (when (< c (length *esc-table*))
      (aref *esc-table* c))))

(defmacro defesc (char (src dst start end) &body body)
  "Define the procedure of four arguments SRC, DST, START and END and
whose body is BODY as the escape function associated with CHAR.  That
procedure should be called when we encounter the character CHAR and
the position after the last processed character should be returned.
The input text is the substring of SRC between the positions START and
END (CHAR is at the position START.)  The result is written in the
stream DST."
  (let ((code (char-code char)))
    (unless (< code (length *esc-table*))
      (error "couldn't associate a function escape with ~C" char))
    `(setf (aref *esc-table* ,code)
           (lambda (,src ,dst ,start ,end) ,@body))))

(defesc #\< (src dst start end)
  (declare (ignore src end))
  (princ "&lt;" dst)
  (1+ start))

(defesc #\' (src dst start end)
  (declare (ignore src end))
  (princ "&#039;" dst)
  (1+ start))

(defesc #\& (src dst start end)
  (case-match (src (1+ start) end)
    ("^#\\d{3};"                        ; character entity?
     (write-sequence src dst :start start :end match-end)
     match-end)
    (t
     (princ "&amp;" dst)
     (1+ start))))

(defesc #\" (src dst start end)
  (declare (ignore src end))
  (princ "&quot;" dst)
  (1+ start))

(defesc #\_ (src dst start end)
  (let ((pos (or (position #\_ src :start (1+ start) :end end)
                 end)))
    (princ "<em>" dst)
    (esc-html src dst (1+ start) pos)
    (princ "</em>" dst)
    (1+ pos)))

(defesc #\* (src dst start end)
  (let ((pos (or (position #\* src :start (1+ start) :end end)
                 end)))
    (princ "<strong>" dst)
    (esc-html src dst (1+ start) pos)
    (princ "</strong>" dst)
    (1+ pos)))

(defesc #\\ (src dst start end)
  (let ((i (1+ start)))
    (cond ((and (< i end) (specialp (aref src i))) ; escape a special character?
           (princ (aref src i) dst)
           (1+ i))
          (t (princ #\\ dst) i))))

(defesc #\> (src dst start end)
  (cond ((or (zerop start)              ; beginning of string?
             (and (> start 1)           ; after any empty line?
                  (string= *emptyl*
                           src
                           :start2 (- start (length *emptyl*))
                           :end2 start)))
         (princ "<blockquote>" dst)
         (case-match (src (1+ start) end)
           ("(\\r\\n){2,}"              ; empty line?
            (esc-html src dst (1+ start) (1- match-start))
            (princ "</blockquote>" dst)
            match-end)
           (t
            (write-sequence src dst :start (1+ start) :end end)
            (princ "</blockquote>" dst)
            end)))
        (t
         (princ "&gt;" dst)
         (1+ start))))

(defun esc-html (src dst start end)
  "Escape all special HTML characters in the string SRC between the
positions START and END and write it to DST."
  (loop
     with i = start
     while (< i end)
     do (aif (esc-function (aref src i))
             (setq i (funcall it src dst i end))
             (progn
               (princ (aref src i) dst)
               (incf i)))))

(defun in-fmt (s)
  "Transform the ASCII string to HTML by escaping characters."
  (with-output-to-string (d)
    (esc-html s d 0 (length s))))

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

(defun unesc-amp (src dst start end)
  "Unescape the HTML string SRC after an ampersand and write the
  result to DST. Return the position immediately after the HTML
  entity."
  (case-match (src start end)
    ("^lt;" (princ #\< dst) match-end)
    ("^gt;" (princ #\> dst) match-end)
    ("^quot;" (princ #\" dst) match-end)
    ("^amp;" (princ #\& dst) match-end)
    ("^#039;" (princ #\' dst) match-end)
    (t (princ #\& dst) start)))

(defun unesc-lt (src dst start end)
  "Unescape the HTML string SRC after a left angle bracket and write
the result to DST.  Return the position immediately after the closing
tag in any or after the bracket."
  (case-match (src start end)
    ("^em>"
     (multiple-value-bind (after before)
         (scan-tag "em" src match-end end)
       (princ #\_ dst)
       (unesc-html src dst match-end (or before end))
       (princ #\_ dst)
       (or after end)))
    ("^strong>"
     (multiple-value-bind (after before)
         (scan-tag "strong" src match-end end)
       (princ #\* dst)
       (unesc-html src dst match-end (or before end))
       (princ #\* dst)
       (or after end)))
    ("^blockquote>"
     (multiple-value-bind (after before)
         (scan-tag "blockquote" src match-end end)
       (princ #\> dst)
       (unesc-html src dst match-end (or before end))
       (if after
           (progn (princ *emptyl* dst) after)
           end)))
    (t
     (princ #\< dst)
     start)))

(defun unesc-html (src dst start end)
  "Transform back all special HTML characters to ASCII in the string SRC."
  (loop
     with i = start
     while (< i end)
     do (case (aref src i)
          (#\& (setq i (unesc-amp src dst (1+ i) end)))
          (#\< (setq i (unesc-lt src dst (1+ i) end)))
          ((#\_ #\* #\\ #\>)
           (format dst "\\~C" (aref src i))
           (incf i))
          (otherwise
           (princ (aref src i) dst)
           (incf i)))))

(defun out-fmt (s)
  "Transform back the HTML string to ASCII and unescape special characters."
  (with-output-to-string (d)
    (unesc-html s d 0 (length s))))
