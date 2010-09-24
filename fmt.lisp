(in-package :blog)

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(defun in-fmt (s)
  "Transform the ASCII string to HTML by escaping characters."
  (with-output-to-string (d)
    (esc-html s d)))

(declaim (inline specialp))
(defun specialp (ch)
  (or (char= #\* ch) (char= #\_ ch) (char= #\\ ch)))

(defun esc-html (src dst &key (start 0) (end (length src)))
  "Escape all special HTML characters in the string SRC and write it
to DST."
  (loop
     with i = start
     while (< i end)
     do (let ((delta 1))
          (case (aref src i)
            ((#\<) (princ "&lt;" dst))
            ((#\>) (princ "&gt;" dst))
            ((#\') (princ "&#039;" dst))
            ((#\&)
             (multiple-value-bind (mstart mend)
                 (scan "^#\\d{3};" src :start (1+ i))
               (if mstart               ; character entity?
                   (progn
                     (write-sequence src dst :start i :end mend)
                     (setq delta (- mend i)))
                   (princ "&amp;" dst))))
            ((#\") (princ "&quot;" dst))
            ((#\_)
             (let ((pos (or (position #\_ src :start (1+ i) :end end) end)))
               (princ "<em>" dst)
               (esc-html src dst :start (1+ i) :end pos)
               (princ "</em>" dst)
               (setq delta (- (1+ pos) i))))
            ((#\*)
             (let ((pos (or (position #\* src :start (1+ i) :end end) end)))
               (princ "<strong>" dst)
               (esc-html src dst :start (1+ i) :end pos)
               (princ "</strong>" dst)
               (setq delta (- (1+ pos) i))))
            ((#\\)
             (cond ((and (< (1+ i) end) (specialp (aref src (1+ i))))
                    (princ (aref src (1+ i)) dst)
                    (setq delta 2))
                   (t (princ #\\ dst))))
            (otherwise (princ (aref src i) dst)))
          (incf i delta))))

(defmacro case-match ((src &key (start 0)) &body clauses)
  (labels ((iter (clauses)
             (when clauses
               (let ((cl (car clauses)))
                 (if (eq (car cl) t)
                     `(progn ,@(cdr cl)) ; default statement
                     `(multiple-value-bind (match-start match-end)
                          (scan ,(car cl) ,src :start ,start)
                        (declare (ignorable match-end))
                        (if match-start
                            (progn ,@(cdr cl))
                            ,(iter (cdr clauses)))))))))
    (iter clauses)))

(defun scan-tag (tag src start end)
  "Return the position in the string SRC immediately after the closing
tag corresponding to TAG while maintaining balanced tags or END if
these conditions are not met."
  (do ((i start)                        ; position in src
       (ntag 1)                         ; number opening tags
       (ltag (format nil "^<~A>" tag))
       (rtag (format nil "^</~A>" tag)))
      ((or (>= i end) (zerop ntag)) i)
    (case-match (src :start i)
      (ltag
       (incf ntag)
       (setq i match-end))
      (rtag
       (decf ntag)
       (setq i match-end))
      (t (incf i)))))

(defun unesc-amp (src dst start)
  "Unescape the HTML string SRC after an ampersand and write the
  result to DST. Return the position immediately after the HTML
  entity."
  (case-match (src :start start)
    ("^lt;" (princ #\< dst) match-end)
    ("^gt;" (princ #\> dst) match-end)
    ("^quot;" (princ #\" dst) match-end)
    ("^amp;" (princ #\& dst) match-end)
    ("^#039;" (princ #\' dst) match-end)
    (t (princ #\& dst) start)))

(defun unesc-lt (src dst start end)
  "Unescape the HTML string SRC after a left angle bracket and write
the result to DST.  Return the position immediately after the bracket
or the closing tag `</em>' or `</strong>'."
  (case-match (src :start start)
    ("^em>"
     (let ((pos (scan-tag "em" src (+ start 3) end)))
       (princ #\_ dst)
       (unesc-html src
                   dst
                   :start (+ start 3)
                   :end (if (< pos end) (- pos 5) end))
       (princ #\_ dst)
       pos))
    ("^strong>"
     (let ((pos (scan-tag "strong" src (+ start 7) end)))
       (princ #\* dst)
       (unesc-html src
                   dst
                   :start (+ start 7)
                   :end (if (< pos end) (- pos 9) end))
       (princ #\* dst)
       pos))
    (t
     (princ #\< dst)
     start)))

(defun unesc-html (src dst &key (start 0) (end (length src)))
  "Transform back all special HTML characters to ASCII in the string SRC."
  (loop
     with i = start
     while (< i end)
     do (case (aref src i)
          (#\& (setq i (unesc-amp src dst (1+ i))))
          (#\< (setq i (unesc-lt src dst (1+ i) end)))
          ((#\_ #\*)
           (format dst "\\~C" (aref src i))
           (incf i))
          (otherwise
           (princ (aref src i) dst)
           (incf i)))))

(defun out-fmt (s)
  "Transform back the HTML string to ASCII and unescape special characters."
  (with-output-to-string (d)
    (unesc-html s d)))
