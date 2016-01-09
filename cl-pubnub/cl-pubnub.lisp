;;;; cl-pubnub.lisp

(in-package #:cl-pubnub)

(eval-when (:execute)
  (defconstant +host+ "http://pubsub.pubnub.com"))

(defun pubnub (url) (jsown:parse
		     (drakma:http-request
		      (format nil "~{~A~^/~}"
			      (list* +host+ url)))))
(defclass pubnub-client ()
  ((time
    :initarg  :time
    :initform 0
    :reader   pn-time
    :writer (setf pn-time)))
  (:documentation "Representation of a pubnub client."))
(defvar *client* (make-instance 'pubnub-client))

(defun cmd (url extract)
  #'(lambda(state) (funcall extract (funcall (state-client) (funcall url state)))))
	    
(defun publish (&key (channel "channel") (message  "\"message\""))
  (destructuring-bind (res descr time) (pubnub (publish-url channel message))
		       (setf (pn-time *client*) time)
		       descr))

(defun subscribe (&key (channel "history") (timestamp (pn-time *client*)))
   (destructuring-bind (obj time) (pubnub (subscribe-url channel timestamp))
		       (setf (pn-time *client*) time)
		       obj))

(defun history(&key(channel "history") (count 10) (start nil)
		   (end nil)
		   (string-token "true")
		   (reverse "true")
		   (auth "auth")
		   (include-token "true"))
  (pubnub (history-url channel
		       :count count
		       :start start
		       :end end
		       :include-token include-token
		       :auth auth
		       :reverse reverse
		       :include-token include-token)))



(defun history (channel &optional (limit 100))
  (pubnub (history-url channel)))



(defun test-publish()
  (loop for ch from 1 to 10 do
	(loop for message from 1 to 10 do
	      (publish :channel ch :message message))))
