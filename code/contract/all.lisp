(uiop:define-package #:eth/contract/all
  (:nicknames #:eth/contract)
  (:use #:cl #:eth/abi/all)
  (:import-from #:serapeum
		#:@
		#:defconstructor
		#:octet-vector
		#:fmt)
  (:import-from #:eth/utils
		#:oct<-
		#:keccak256)
  (:import-from #:babel
		#:string-to-octets)
  (:import-from #:alexandria
		#:when-let))
(in-package #:eth/contract/all)

(defclass abi-fun ()
  ((name :initarg :name :accessor abi-fun-name)
   (inputs :initarg :inputs :accessor abi-fun-inputs)
   (outputs :initarg :outputs :accessor abi-fun-outputs)
   (selector :initarg :selector :accessor abi-fun-selector)
   (state-mutability :initarg :state-mutability :accessor abi-fun-state)))

(defun abi-function-selector (name args)
  (let* ((fun-name (fmt "~a(~{~a~^,~})" name
			(map 'list #'abi-cannonical-name args))))
    (subseq
     (keccak256 (string-to-octets fun-name))
     0 4)))

(defun make-abi-fun (name inputs outputs state-mutability)
  (make-instance 'abi-fun
		   :name name
		   :inputs (abi-tuple (coerce inputs 'vector))
		   :outputs (if (typep outputs 'sequence)
				(abi-tuple (coerce outputs 'vector))
				outputs)
		   :selector (abi-function-selector name inputs)
		   :state-mutability state-mutability))

(defun encode-call (fun &rest args)
  (declare (type abi-fun fun))
  (with-slots (selector inputs) fun
    (concatenate 'octet-vector
		 selector
		 (abi-encode inputs (coerce args 'vector)))))

(defun decode-result (fun result)
  (declare (type abi-fun fun)
	   (type octet-vector result))
  (with-slots (outputs) fun
    (abi-decode result outputs)))

(defparameter *xfun*
  (make-abi-fun "balanceOf" (list abi-address) abi-uint :view))   


(defclass event ()
  ((name :initarg :name :accessor event-name)
   (args :initarg :args :accessor event-args)
   (anonymous :initarg :anonymous :accessor event-anonymous)
   (hash :initarg :hash :accessor event-hash)))

(defconstructor event-arg
  (name (or string null))
  (type <abi-type>)
  (indexed boolean))

(defun event-name-hash (name args)
  (let* ((event-str (fmt "~a(~{~a~^,~})" name
			 (map 'list
			      (lambda (a) (abi-cannonical-name (event-arg-type a)))
			      args))))
    (keccak256 (string-to-octets event-str))))

(defun make-event (name anonymous &rest args)
  (make-instance 'event
		 :name name
		 :args args
		 :anonymous anonymous
		 :hash (event-name-hash name args)))


(defparameter *xev* (make-event "Transfer" nil
				 (event-arg "from" abi-address t)
				 (event-arg "to" abi-address t)
				 (event-arg "value" abi-uint nil))) 

(defparameter *xrec* (eth:eth/get-tx-receipt (eth:oct<- "0x71748ac5d1a2f01e5a7d90bc07089f06eb2ee7dc92709a27606f038191f2a9f8")))

(defparameter *xlog* (aref (serapeum:@ *xrec* "logs") 0))

(defun parse-log-args (event log)
  (with-slots (anonymous args hash) event
    (when-let ((topics (map 'list #'oct<- (@ log "topics")))
	       (data (oct<- (@ log "data"))))
      (multiple-value-bind (indexed not-indexed) (loop for arg in args
						       if (event-arg-indexed arg) collect arg into indexed
						       else collect arg into not-indexed
						       finally (return (values indexed not-indexed)))
	;; (when-let ((parsed-args
	;; 	    (when (or anonymous (and (not (emptyp topics))
	;; 				     (= (length indexed) (1- (length topics)))
	;; 				     (equalp (elt topics 0) hash)))
	;; 	      (append
	;; 	       (loop for idx-arg in indexed
	;; 		     for topic in (if anonymous topics (subseq topics 1))
	;; 		     collect (abi-decode-indexed-event-arg idx-arg (hex->octets topic)))
	;; 	       (when not-indexed
	;; 		 (abi-decode (hex->octets data) (abi-tuple (map 'vector #'event-arg-type not-indexed)))))))))
	(values indexed not-indexed)))))

;; (parse-log-args *xev* *xlog*)

;; (eth:hex<-
;;  (event-hash *xev*))



;; '(abi-function "Name" ("name-a1" abi-uint) (arg2) arg3)
;; '(abi-event "Name" ("name-a1" abi-uint :indexed t) (arg2) arg3)

;; '(contract :functions '() :errors '())


;; '(call erc20/transfer args)
;; '(call (abi-fun address "haha") args)

;; ; need gas, nonce,

;; '(call address (abi-fun ) 10 11)

;; '(sign-tx address (abi-fun ))
;; '(sign-tx address (abi-fun ))

;; '(uni/swap a b c)

;; '(uni/swap a b c)
