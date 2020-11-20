
(in-package :cl-user)


(defpackage :session
  (:use :cl :cl-ppcre :vip-utils)
  (:export :add-session-to-cookie
           :get-session
           :make-session
           :get-request-session
           :set-session-datum
           :get-session-datum))

