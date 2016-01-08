;;;; cl-pubnub.lisp

(in-package #:cl-pubnub)
(defconstant +host+ "http://pubsub.pubnub.com")

(defun pubnub (url) (jsown:parse
		     (drakma:http-request
		      (format nil "~{~A~^/~}"
			      (list* +host+ url)))))

(defvar *timestamp* 0)
(defun cmd (url extract)
  #'(lambda(state) (funcall extract (funcall (state-client) (funcall url state)))))
	    
(defun publish (&key (channel "channel") (message  "\"message\""))
  (destructuring-bind (res descr time) (pubnub (publish-url channel message))
		       (setf *timestamp* time)
		       descr))

(defun subscribe (&key (channel "history") (timestamp *timestamp*))
   (destructuring-bind (obj time) (pubnub (subscribe-url channel timestamp))
		       (setf *timestamp* time)
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
		       :include-token include-token)) )



(defun history (channel &optional (limit 100))
  (pubnub (history-url channel)))

(defun pub-peek ()
  "looks like a bug in the api, but it is funny to peek what is going on."
  (let ((timestamp 0)
	(shown '()))
    (loop for res = (pubnub (peek-url "hello_world" 0)) do
	  
	(when (member "body" (jsown:keywords res) :test #'equal)
	  (let ((stamp (format nil "~&~A:~A" (jsown:val res "displayName") (jsown:val res "body") )))
	    (unless (member stamp shown :test #'equal)
	      (format t "~&~A" stamp (jsown:val res "body") )
	      (push stamp shown) )
	    )
	  
	  ;(break "~A" res)
	  ))))

(defun test-publish()
  (loop for ch from 1 to 10 do
	(loop for message from 1 to 10 do
	      (publish :channel ch :message message))))
