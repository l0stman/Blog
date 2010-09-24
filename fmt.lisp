(in-package :blog)

(defvar *ret* (coerce '(#\return #\newline #\return #\newline) 'string))

(defun in-fmt (s)
    "Transform the ASCII string to HTML by escaping characters."
    (esc-html s))

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

(defmacro case-match ((src &key (start 0)) &body clauses)
  (labels ((iter (clauses)
             (when clauses
               (let ((cl (car clauses)))
                 (if (eq (car cl) t)
                     `(progn ,@(cdr cl)) ; default statement
                     `(multiple-value-bind (match-start match-end)
                          (scan ,(car cl) ,src :start ,start)
                        (if match-start
                            (progn ,@(cdr cl))
                            ,(iter (cdr clauses)))))))))
    (iter clauses)))

(defun unesc-html (src)
  "Transform back all special HTML characters to ASCII in the string SRC."
  (with-output-to-string (dst)
    (loop
       with i = 0
       while (< i (length src))
       do (if (char= (aref src i) #\&)
              (case-match (src :start (1+ i))
                ("^lt;"
                 (princ #\< dst)
                 (setq i match-end))
                ("^gt;"
                 (princ #\> dst)
                 (setq i match-end))
                ("^quot;"
                 (princ #\" dst)
                 (setq i match-end))
                ("^amp;"
                 (princ #\& dst)
                 (setq i match-end))
                ("^#039;"
                 (princ #\' dst)
                 (setq i match-end))
                (t
                 (princ #\& dst)
                 (incf i)))
              (progn
               (princ (aref src i) dst)
               (incf i))))))

(defun out-fmt (s)
  "Transform back the HTML string to ASCII and unescape special characters."
  (unesc-html s))

