;;;; cl-pubnub.lisp

(in-package #:cl-pubnub)
(defconstant +pub-key+ "pub-c-0ffbb83a-b74a-4a3c-be08-d40532fce60f")
(defconstant +sub-key+ "sub-c-783927a0-a48c-11e5-9196-02ee2ddab7fe")
(defconstant +host+ "http://pubsub.pubnub.com")
(defun to-url(&rest params)
  (format nil "~{~A~^/~}" (list* +host+ params)))

(defun time-url()(to-url "time" 0))

(defun leave-url(channel uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  "leave" (do-urlencode:urlencode (format nil "{'uuid':~S,'auth':''}" uuid))))
(defun leave-group-url(channel uuid)
  (to-url "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  "leave" (do-urlencode:urlencode (format nil "{'uuid':~S,'auth':''}" uuid))))

(defun history-url (channel &key (count 10)
			    (start nil)
			    (end nil)
			    (reverse "false")
			    (string-message-token "true")
			    (include-token "true")
			    (params "")
			    (auth "auth"))

  (to-url "v2" "history" "sub-key" +sub-key+ "channel"
	  (format nil "~A~A" (do-urlencode:urlencode channel)
		  (format nil "?~{~{~A=~A~}~^&~}"
			  (remove-if-not #'second
					 `(("string_message_token"  ,string-message-token)
					   ("include_token" ,include-token)
					   ("reverse" ,reverse)
					   ("count" ,count)
					   ("start" ,start)
					   ("end" ,end))))
	  ) ))

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
   (do-urlencode:urlencode (format nil "{'timestamp' : 0}" ))))

(defun presence-heartbeat-url (channel uuid auth)
    (to-url
   "v2/presence/sub-key" +sub-key+ "channel" (do-urlencode:urlencode channel)  "heartbeat"
   (do-urlencode:urlencode (format nil "{'uuid':'~A', 'auth':'~A'}"uuid auth )))
  )

(defun publish-url (channel message)
  (to-url "publish" +pub-key+ +sub-key+
	  0 ;; signature
	  channel
	  0 ;; callback
	  (do-urlencode:urlencode (format nil "~A" message))))

(defun subscribe-url(channel timetoken)
  (to-url "subscribe" +sub-key+
	  channel
	  0 ;; callback
	  timetoken))

(defun history-old-url(channel limit &key (callback 0))
  (to-url "history" +sub-key+	  channel	  callback	  limit))

(defun pubnub (url) (jsown:parse (drakma:http-request url)) )

(defvar *timestamp* 0)
(defun publish (&key (channel "channel") (message  "\"message\""))
  (destructuring-bind (res descr time) (pubnub (publish-url channel message))
		       (setf *timestamp* time)
		       descr))
(defun history(&key(channel "channel") (count 4) (start 0) (reverse "true")
		   (with-token "true"))
  (pubnub (history-url channel :params
  
  )
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

(defun test-publish()
  (loop for ch from 1 to 10 do
	(loop for message from 1 to 10 do
	      (publish :channel ch :message message))))
