
(in-package :session)

(defun to-string (x)
  (format nil "~A" x))

;; stuff to implement sessions. They are stored in memory.

;; This is where sessions are stored.

;; *** Note that I've changed defparameter to defvar so I can reload
;; this file without clobering any running sessions. I want to be able
;; to resync and then reload all the source code without interrupting
;; normal running of the server (if possible). I need to come up with
;; good procedures for handling these sorts of updates.

(defvar *sessions* (make-hash-table :test #'equal))
(defvar *session-timeout* 3600) ;seconds

;; create and return a new session token
;; !!! using (random-string 40) is excessive really - just being paranoid
(defun make-session (site)
  "Create a session and return its id"
  (let ((session-id (random-string 40)))
    (loop while (gethash session-id
			 *sessions*)
       do (setq session-id (random-string 40)))
    (setf (gethash session-id
		   *sessions*)
	  (cons (get-universal-time)
		(hash (list :session-id session-id
			    :site site))))
    (clean-sessions *session-timeout*)
    session-id))

;; If passed the session it just returns that. It doesn't check for its existance though
;; that's a bit bas ackward really
(defmethod get-session (session-id)
  (if (consp session-id)
      session-id
      (if (gethash session-id *sessions*)
	  (progn (setf (car (gethash session-id *sessions*))
		       (get-universal-time))
		 (gethash session-id *sessions*)))))

(defun get-session-data (session-id)
  ;; This will always reset the session access time...
  (if (get-session session-id)
      (progn (setf (car (get-session session-id))
		   (get-universal-time))
	     (cdr (get-session session-id)))))

(defun set-session-data (session-id data)
  (if (get-session session-id)
      (progn (setf (car (get-session session-id))
		   (get-universal-time))
	     (setf (cdr (get-session session-id))
		   data))))

;; *** Use these 2 to do stuff with sessions. You can associate arbirtary keys with sessions
(defun get-session-datum (session-id datum)
  (let ((s (get-session session-id)))
    (if s
	(gethash datum (cdr s)))))

(defun set-session-datum (session-id datum value)
  (let ((s (get-session session-id)))
    (if s
	(setf (gethash datum (cdr s))
	      value)))
  value)

;; expunge expired sessions
;; cleans all sessions if no timeout is given
(defun clean-sessions (&optional (timeout 0))
  (maphash #'(lambda (x y)
	       (if (< (car y)
		      (- (get-universal-time)
			 timeout))
		   (delete-session x)))
	   *sessions*))

(defun delete-session (session-id)
  (if (consp session-id)
      (remhash (get-session-datum session-id :session-id) *sessions*)
      (remhash session-id *sessions*)))

(defun get-request-session (site req &optional session-id)
  ;; (princ (net.aserve::get-cookie-values req))
  ;; (format t ";; get-request-session: ~S~%" (request-query req))
  (cond ( session-id
	 (get-session session-id))

        ;; we can't look in posted data since we often use that to post back non form encoded stuff
        ;; do we ever need these? We would if we were crossing domains, eg for SSL
        ((net.aserve::request-query-value "sessionId" req :post nil)
         (get-session (net.aserve::request-query-value "sessionId" req :post nil)))

        ((net.aserve::request-query-value "s" req :post nil)
         (get-session (net.aserve::request-query-value req "s" :post nil)))
	
	;; cookies are allowed to contain multiple sessions separated by periods
	( (assoc "s" (net.aserve::get-cookie-values req)
		 :test #'equal)
	 ;;(format t ";; sessions: ~S~%" (assoc "s" (net.aserve::get-cookie-values req)
	;;					      :test #'equal))
	 (loop for session-token in (split "\\." (cdr (assoc "s" (net.aserve::get-cookie-values req)
							     :test #'equal)))
	       do
	       ;;(format t ";; tok: ~S~%" session-token)
	       (when (equal (get-session-datum session-token
					       :site)
			    site)
		 ;;(format t ";; SESSION = ~S~%" session-token)
		 (return-from get-request-session
		   (get-session session-token))))
	  nil)))

;; We need to handle multiple sessions or we'll have big problems if people try to look at more than one of our sites!!!
;; I've improved this so it strips out dead sessions
(defun add-session-to-cookie (req session-id)
  (let ((s ""))
    (loop for a in (split "\\." (cdr (assoc "s" (net.aserve::get-cookie-values req)
					    :test #'equal)))
       when (gethash a
		     *sessions*)
       do (setq s (concatenate 'string s "." a)))
    (setq s (concatenate 'string s "." (to-string session-id)))
    (net.aserve::set-cookie-header req
		       :name "s"
		       :value s)))

;; Retrieve all current sessions
(defun list-sessions ()
  (loop for x being the hash-values in *sessions*
	collect
	(list (car x)
	      ;; this gives the sessions age
	      (- (get-universal-time)
		 (car x))
	      (loop for y being the hash-keys in (cdr x)
		    for z being the hash-values in (cdr x)
		    collect (list y z)))))

(defun empty-sessionp (session-id)
  (not (loop for a being the hash-keys
	  in (cdr (get-session session-id))
	  ;; these 2 keys are always created. We need to ascertain if more info has been added to the session
	  when (not (or (eq a :session-id)
			(eq a :site)))
	  collect a)))
