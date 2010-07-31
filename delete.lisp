(in-package :blog)

(define-easy-handler (del-conf :uri "/delete"
                               :default-request-type :post)
    ((id :parameter-type 'integer) delete-p uri)
  "Ask confirmation before deleting a post."
  (w/auth
   (cond ((not delete-p)
          (w/html ()
            (:form :method "post" :action "delete"
                   (:div :class "message"
                         (fmt "Do you really want to delete post ~D?" id))
                   (:input :type "hidden" :name "id" :value id)
                   (:input :type "hidden" :name "uri" :value uri)
                   (:div :class "submit"
                         (:input :type "submit" :name "delete-p" :value "yes")
                         (:span :class "separator" " ")
                         (:input :type "submit" :name "delete-p"
                                 :value "no")))))
         (t (when (string= delete-p "yes")
              (delete-post id)
              (save-blog))
            (redirect uri)))))