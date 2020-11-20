
;;;;;; session.asd

(asdf:defsystem :session
  :description "Simple HTTP session library (old)"
  :author "David Ritchie"
  :serial t
  :depends-on (#:ironclad #:cl-ppcre #:aserve #:vip-utils)
  :components ((:file "package")
               (:file "session")))
