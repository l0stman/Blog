(in-package :blog)

(defvar *blog* nil)
(defvar *title* "A Blog")
(defvar *id* 0)
(defvar *maxchar* 320)
(defvar *maxpost* 10)
(defvar *proxy-uri* "/")

(defun redir-url (s)
  (concatenate 'string *proxy-uri* s))

(defun salt ()
  (let* ((size 5)
	 (s (make-array size
			:fill-pointer 0
			:element-type 'character)))
    (loop repeat size
       do (vector-push (code-char (random 128)) s)
       finally (return s))))

(defvar *salt* (salt))

(defun hash (pass)
  (md5sum-sequence (concatenate 'string *salt* pass)))

(defvar *user* "admin")
(defvar *hash* (hash "admin"))