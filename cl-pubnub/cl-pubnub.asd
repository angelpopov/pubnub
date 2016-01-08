;;;; cl-pubnub.asd

(asdf:defsystem #:cl-pubnub
  :serial t
  :description "cl interface to www.pubnub.com"
  :author "Angel Popov"
  :license "Apache 2"
  :depends-on (#:drakma
               #:jsown
	       #:do-urlencode)
  :components ((:file "package")
	       (:file "keys")
	       (:file "urls")
               (:file "cl-pubnub") ))

