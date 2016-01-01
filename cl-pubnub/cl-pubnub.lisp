;;;; cl-pubnub.lisp

(in-package #:cl-pubnub)
(defconstant +pub-key+ "demo")
(defconstant +sub-key+ "demo")
(defconstant +host+ "http://pubsub.pubnub.com")
(defun to-url(&rest params)
  (format nil "~{~A~^/~}" (list* +host+ params)))

(defun peek-url (channel message)
  (to-url "publish" +pub-key+
	  0 ;; signature
	  channel
	  0 ;; callback
	  message))

(defun time-url()(to-url "time" 0))

(defun leave-url(channel uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  "leave" (do-urlencode:urlencode (format nil "{'uuid':~S,'auth':''}" uuid))))
(defun leave-group-url(channel uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  "leave" (do-urlencode:urlencode (format nil "{'uuid':~S,'auth':''}" uuid))))

(defun history-url(channel)
  (to-url "v2" "history" "sub-key" +sub-key+ "channel"
	  (do-urlencode:urlencode channel)
	  ;;(do-urlencode:urlencode "{'string_message_token':'true'}") TBA if uncommented, it shows 0 items
	  ) )
(defun replay-url(source-channel dest-channel)
  (to-url "v1" "replay" +pub-key+ +sub-key+
	  (do-urlencode:urlencode source-channel)
	  (do-urlencode:urlencode dest-channel)
	  ;;(do-urlencode:urlencode "{'count':1}") TBA - result: not-found?!
	  ))
(defun here-now-url ()
  (to-url "v2" "presence" "sub_key" +sub-key+) ;; TBA - should we restrict to max 100 items?
  )

(defun where-now-url (uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+ "uuid" uuid))

(defun state-url (channel uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+ "channel" (do-urlencode:urlencode channel)
	  "uuid" uuid))

(defun grant-url(channel)
  (to-url "v1" "auth" "sub_key" +sub-key+ (do-urlencode:urlencode "{
                channel  : 'my_chat',
                callback : fun,
                error    : fun,
                ttl      : 24 * 60, // Minutes
                read     : true,
                write    : true,
                auth_key : '3y8uiajdklytowsj'
            }") ))

(defun mobile-gw-prowision-url (device-id
			    op ;; add | remove
			    gw-type ;; apns' | 'gcm'
			    channel)
  (to-url
   "v1/push/sub-key" +sub-key+
   "devices" device-id
   (do-urlencode:urlencode (format nil "{
         device_id: '~A',
         op       : '~A',
         gw_type  : '~A',
         channel  : '~A',
         callback : fun,
         error    : fun,
         }" device-id op gw-type channel ))))

(defun audit-url ()
  (to-url
   "v1/auth/audit/sub-key" +sub-key+
   (do-urlencode:urlencode (format nil "{
         timestamp : 0}" ))))

(defun publish-url (channel message)
  (to-url "publish" +pub-key+ +sub-key+
	  0 ;; signature
	  channel
	  0 ;; callback
	  (do-urlencode:urlencode message)))

(defun subscribe-url(channel timetoken)
  (to-url "subscribe" +sub-key+
	  channel
	  0 ;; callback
	  timetoken))

(defun history-old-url(channel limit &key (callback 0))
  (to-url "history" +sub-key+	  channel	  callback	  limit))

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

