(in-package :blog)

(defvar *htoot-url* nil
  "Set this in case Hunchentoot is behind a proxy and is not
directly accessible to the outside world.")

(defvar *maxfeed* 10
  "Maximum number of items in the rss feed.")

(defvar *rss-description* "RSS feed")

(defhand (feed "/feed")
  (let ((url (or *htoot-url* (format nil "http://~A" (host)))))
    (html/s (:prologue "<?xml version=\"1.0\"?>")
      (:rss :version "2.0"
            (:channel
             (:title (str *title*))
             (:link (conc url "/blog"))
             (:description (str *rss-description*))
             (:docs "http://blogs.law.harvard.edu/tech/press")
             (loop repeat *maxfeed*
                for post = (find-from *id*) then (find-from (1- (id post)))
                while post
                do (htm
                    (:item
                     (:title (str (title post)))
                     (:link (fmt "~A/view?id=~D" url (id post)))
                     (:description (esc (stub post)))))))))))