(in-package #:cl-pubnub)

(defun time-url()(list "time" 0))

(defun leave-url(channel uuid)
  (list "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  (format nil "leave?uuid=~A"uuid)))

(defun leave-group-url(channel uuid)
  (list "v2" "presence" "sub_key" +sub-key+
	  "channel" (do-urlencode:urlencode channel)
	  "leave" (do-urlencode:urlencode (format nil "{'uuid':~S,'auth':''}" uuid))))

(defun history-url (channel &key (count 10)
			    (start nil)
			    (end nil)
			    (reverse "false")
			    (string-message-token "true")
			    (include-token "true")
			    (auth "auth"))

  (list "v2" "history" "sub-key" +sub-key+ "channel"
	(format nil "~A?~{~{~A=~A~}~^&~}"
			(do-urlencode:urlencode channel)
			(remove-if-not #'second
				       `(("string_message_token"  ,string-message-token)
					 ("include_token" ,include-token)
					 ("reverse" ,reverse)
					 ("count" ,count)
					 ("start" ,start)
					 ("end" ,end)))) ))

(defun replay-url(source-channel dest-channel)
  (list "v1" "replay" +pub-key+ +sub-key+
	  (do-urlencode:urlencode source-channel)
	  (do-urlencode:urlencode dest-channel)
	  ;;(do-urlencode:urlencode "{'count':1}") TBA - result: not-found?!
	  ))

(defun here-now-url (&key (sub-key +sub-key+))
  (list "v2" "presence" "sub_key" sub-key))

(defun where-now-url (uuid)
  (list "v2" "presence" "sub_key" +sub-key+ "uuid" uuid))

(defun state-url (channel uuid)
  (list "v2" "presence" "sub_key" +sub-key+ "channel" (do-urlencode:urlencode channel)
	  "uuid" uuid))

(defun grant-url(channel)
  (list "v1" "auth" "sub_key" +sub-key+ (do-urlencode:urlencode "{
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
  (list
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
  (list
   "v1/auth/audit/sub-key" +sub-key+
   (do-urlencode:urlencode (format nil "{'timestamp' : 0}" ))))

(defun presence-heartbeat-url (channel uuid auth)
    (list
   "v2/presence/sub-key" +sub-key+ "channel" (do-urlencode:urlencode channel)  "heartbeat"
   (do-urlencode:urlencode (format nil "{'uuid':'~A', 'auth':'~A'}"uuid auth )))
  )

(defun publish-url (channel message)
  (list "publish" +pub-key+ +sub-key+
	  0 ;; signature
	  channel
	  0 ;; callback
	  (do-urlencode:urlencode (format nil "~A" message))))

(defun subscribe-url(channel timetoken)
  (list "subscribe" +sub-key+
	  (do-urlencode:urlencode (format nil "~A" channel))
	  0 ;; callback
	  timetoken))

(defun history-old-url(channel limit &key (callback 0))
  (list "history" +sub-key+	  channel	  callback	  limit))
