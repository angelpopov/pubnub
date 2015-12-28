;;;; cl-pubnub.lisp

(in-package #:cl-pubnub)
(defconstant +pub-key+ "demo")
(defconstant +sub-key+ "demo")
(defconstant +host+ "http://pubsub.pubnub.com")

(defun peek-url (channel message)
  (format nil "~{~A~^/~}" (list +host+ "publish" +pub-key+
				0 ;; signature
				channel
				0 ;; callback
				message)))

(defun publish-url (channel message)
  (format nil "~{~A~^/~}" (list +host+ "publish" +pub-key+ +sub-key+
				0 ;; signature
				channel
				0 ;; callback
				(do-urlencode:urlencode message))))

(defun subscribe-url(channel timetoken)
  (format nil "~{~A~^/~}" (list +host+ "subscribe" +sub-key+
				channel
				0 ;; callback
				timetoken)))

(defun history-url(channel limit &key (callback 0))
  (format nil "~{~A~^/~}" (list +host+ "history" +sub-key+
				channel
				callback
				limit)))

(defun pubnub (url) (jsown:parse (drakma:http-request url)) )

(defvar *timestamp* 0)
(defun publish (channel message)
  (destructuring-bind (res descr time) (pubnub (publish-url channel message))
		       (setf *timestamp* time)
		       descr))

(defun subscribe (channel &key (timestamp *timestamp*))
   (destructuring-bind (obj time) (pubnub (subscribe-url channel timestamp))
		       (setf *timestamp* time)
		       obj))

(defun history (channel &optional (limit 100))
  (pubnub (history-url channel limit)))

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

