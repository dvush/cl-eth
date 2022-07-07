(uiop:define-package #:eth/provider/providers
  (:use #:cl #:eth/provider/base)
  (:import-from #:alexandria
		#:required-argument)
  (:import-from #:dexador
		#:post)
  (:import-from #:iolib
		#:send-to
		#:with-open-socket)
  (:import-from #:flexi-streams
		#:string-to-octets)
  (:export #:make-http-provider
	   #:make-ipc-provider))
(in-package #:eth/provider/providers)

(defclass http-provider ()
  ((url
    :initform (required-argument 'url)
    :initarg :url
    :accessor http-provider-url)))

(defun make-http-provider (url)
  "Make http-provider from url, e.g. http://127.0.0.1:8545"
  (declare (type string url))
  (make-instance 'http-provider :url url))

(defmethod rpc-call ((provider http-provider) method &optional params)
  (let* ((request          (jsonrpc-encode-call method params))
         (response         (post (http-provider-url provider)
				 :content request
				 :headers (list (cons "Content-type" "application/json"))))
	 (extracted-result (jsonrpc-extract-result response)))
    extracted-result))

 
(defclass ipc-provider ()
  ((path
    :initform (required-argument 'path)
    :initarg :path
    :accessor ipc-provider-path))) 

(defun make-ipc-provider (path)
  "Make ipc-provider from path to socket, e.g. /path-to-geth-dir/geth.ipc"
  (make-instance 'ipc-provider :path path))

(defmethod rpc-call ((provider ipc-provider) method &optional params)
  (let* ((request (jsonrpc-encode-call method params))
         (response (with-open-socket (socket :address-family :local
					     :remote-filename (ipc-provider-path provider))
		     (send-to socket (string-to-octets request))
		     (read-line socket)))
	 (extracted-result (jsonrpc-extract-result response)))
    extracted-result))
