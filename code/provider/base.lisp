(uiop:define-package #:eth/provider/base
  (:use #:cl)
  (:import-from #:shasht
		#:read-json
		#:write-json)
  (:export #:*default-provider*
	   #:rpc-call
	   #:jsonrpc-error
	   #:jsonrpc-encode-call
	   #:jsonrpc-extract-result))
(in-package #:eth/provider/base)

(defvar *default-provider* nil
  "Provider used for all RPC calls by default.")

(defgeneric rpc-call (provider method &optional params)
  (:documentation "Make rpc call with provider and return sesult.
- method is string name
- params is list of aruments.
  (hash-table :test #'equalp) with string keys is used for json ojects"))

(define-condition jsonrpc-error (error)
  ((code :initarg :code)
   (message :initarg :message)
   (data :initarg :data))
  (:report (lambda (condition stream)
	     (with-slots (code message) condition
	       (format stream "JSONRPC error, code: ~a, message: ~a" code message)))))


(defun jsonrpc-encode-call (method &optional params (id 1))
  (write-json `(:object-alist
  			   ("jsonrpc" . "2.0")
  			   ("id" . ,id)
  			   ("method" . ,(string method))
  			   ,@(when params
  			       `(("params" . ,`(:array ,@params)))))
  		      nil))

(defun jsonrpc-extract-result (response)
  (let ((response (read-json response)))
    (multiple-value-bind (result result-p) (gethash "result" response)
      (if result-p
	  result
	  (multiple-value-bind (err err-p) (gethash "error" response)
	    (unless err-p (error "Invalid jsonrpc response: no result or error field."))
	    (let ((err-code (gethash "code" err))
		  (err-message (gethash "message" err))
		  (err-data (gethash "data" err)))
	      (error 'jsonrpc-error :code err-code
				    :message err-message
				    :data err-data)))))))
