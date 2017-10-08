;;;; cl-pubnub.lisp
(in-package #:cl-pubnub)
(defvar +host+ "http://pubsub.pubnub.com")

(defun to-url(list)
  (format nil "~{~A~^/~}" (list* +host+ list)) )

(defun pubnub (list)
  (jsown:parse
   (drakma:http-request
     (print (to-url list)) )))

(defclass pubnub-client ()
  ((time
    :initarg  :time
    :initform 0
    :accessor pn-time)
   (items
    :initarg  :items
    :initform '()
    :accessor pn-items))
  (:documentation "Representation of a pubnub client."))

(defgeneric channels(client)
  (:documentation "List of available channels"))

(defmethod channels ((pn pubnub-client))
  (sort (jsown:keywords (pn-items pn)) #'> :key #'(lambda (i) (jsown:filter (pn-items pn) i "occupancy"))))

(defmethod channel ((pn pubnub-client) channel)
  (with-slots (items)
	      (if (member channel items :test #'equal) 
		  (jsown:val items channel)
		(format t "~A does not contain key ~A" items))))

(defvar *client* (make-instance 'pubnub-client))

(defun cmd (url extract)
  #'(lambda(state) (funcall extract (funcall (state-client) (funcall url state)))))
	    
(defun publish (&key (channel "channel") (message  "\"message\""))
  (destructuring-bind (res descr time) (pubnub (publish-url channel message))
		       (setf (pn-time *client*) time)
		       descr))

(defun now()
  (destructuring-bind
   (now) (pubnub (time-url))
   (setf (pn-time *client*) now)))

(defun errorp(item)
  (or (null item) (member (jsown:keywords item) '("error") :test #'equal)) )

(defun here-now ()
  (let((items (pubnub (here-now-url))))
    (if (errorp items)
	(format t "~A~%" items)
      (setf (pn-items *client*)
	    (jsown:filter items "payload" "channels")))))

(defun subscribe (&key (channel (first (pn-channels *client*))) (timestamp (pn-time *client*)))
  (destructuring-bind
   (obj time . channels) (pubnub (subscribe-url channel timestamp)) 
   (setf (pn-time *client*) time)
   (when obj
     (if channels
	 (progn
	   (assert (= 1 (length channels)))
	   (let ((channels (split-sequence #\, (first channels) ))) 
	     (assert (= (length channels) (length obj)))
	     (mapcar #'cons channels obj)))
       obj))))

(defun history(&key(channel (first (channels *client*))) (count 10) (start nil)
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

(defun watch-all ()
  "Shows activity on all channels"
  (here-now)
  (loop do
	(loop for (channel . message) in
	      (subscribe :channel (format nil "~{~A~^,~}"(channels *client*))) do
	      (format t "~&~A:~A~%" channel message))))

(defun test-publish()
  (loop for message from 1 to 10 do
	(publish :channel (format nil "multi~A" message)
		 :message message)))

(defun test-publish()
  (loop for ch from 1 to 10 do
	(loop for message from 1 to 10 do
	      (publish :channel ch :message message))))
;;(pubnub (here-now-url :sub-key (make-string 32300 :initial-element #\1))) hang?
