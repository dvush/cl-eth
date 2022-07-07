(uiop:define-package #:eth/abi/encoding
  (:use #:cl #:eth/abi/base)
  (:import-from #:serapeum
		#:mvlet*
		#:partial
		#:op
		#:scan
		#:~>>
		#:pad-start
		#:->
		#:octet-vector
		#:make-octet-vector
		#:match-of)
  (:import-from #:eth/utils/convert
		#:int->oct)
  (:import-from #:eth/utils/types
		#:u256 #:address)
  (:import-from #:alexandria
		#:appendf
		#:length=)
  (:export #:abi-encode))
(in-package #:eth/abi/encoding)

(-> encode-uint (u256) (octet-vector 32))
(defun encode-uint (uint)
  (let ((res (int->oct uint 32)))
    res))

(-> encode-address (address) (octet-vector 32))
(defun encode-address (address)
  (let ((res (pad-start address 32 0)))
    res))

(-> encode-bool (boolean) (octet-vector 32))
(defun encode-bool (bool)
  (if bool
      (encode-uint 1)
      (encode-uint 0)))

(-> encode-octs (octet-vector &optional (integer 1 32))
    octet-vector)
(defun encode-octs (octets &optional fixed-size)
  (let ((octets-size (length octets)))
    (when fixed-size
      (assert (<= 1 fixed-size 32))
      (assert (= octets-size fixed-size)))

    (let ((padding (- (nth-value 1 (ceiling octets-size 32)))))
      (concatenate 'octet-vector
		   (if fixed-size
		       (make-octet-vector 0)
		       (encode-uint octets-size))
		   octets
		   (make-octet-vector padding)))))


(-> encode-array (<abi-type> vector &key (:fixed-size (integer 1 32)))
    octet-vector)
(defun encode-array (type values &key fixed-size)
  (let ((size (length values))
	(static? (abi-static-type-p type)))
    (when fixed-size
      (assert (= size fixed-size)))

    (let* ((encoded-size    (unless fixed-size
			      (list (encode-uint size))))
	   (encoded-values  (map 'list (partial #'abi-encode type) values))
	   (encoded-offsets (unless static?
			      (~>> (mapcar #'length encoded-values)
				   (scan #'+ _ :initial-value (* 32 size) :end (1- size))
				   (mapcar #'encode-uint)))))
      (apply #'concatenate 'octet-vector
	     (append encoded-size encoded-offsets encoded-values)))))

(defun tuple-value/hash-table->vector (hash-table abi-tuple-types)
  (loop for type across abi-tuple-types
	collect
	   (progn
	     (assert (and (consp type)
			  (stringp (car type))) ()
			  "To encode hash-table as tuple, abi-tuple-types should have form of (NAME . TYPE)")
	     (let* ((name (car type))
		    (value (gethash name hash-table)))
	       (unless value
		 (error "hash-table tuple value missing: ~a" name))
	       value))
	into result
	finally (return (apply #'vector result))))


(-> encode-tuple (vector (or vector hash-table)) octet-vector)
(defun encode-tuple (types values)
  (let ((values (etypecase values
		  (hash-table (tuple-value/hash-table->vector values types))
		  (vector values))))
    (assert (length= types values))

    (let ((heads (list))
	  (tails (list))
	  (tail-offset (loop for type across types summing (nth-value 1 (abi-static-type-p type)))))

      (labels ((head-and-tail (head tail)
		 (push head heads)
		 (push tail tails)
		 (incf tail-offset (length tail)))
	       (result ()
		 (apply #'concatenate 'octet-vector
			(append (reverse heads) (reverse tails)))))
	
	(loop for type across types
	      for value across values
	      do (let* ((static? (abi-static-type-p type))
			(enc-value (abi-encode type value)))

		   (cond (static?
			  (head-and-tail enc-value (octet-vector)))
			 (t
			  (head-and-tail (encode-uint tail-offset) enc-value)))))
	(result)))))

;; (eth:debug-print-oct-chunked
;;  (encode-tuple (vector abi-bytes abi-uint abi-bytes)
;; 	       (vector (eth:oct<- "ffffffffffffffffffffffffffffffffffffffff" 20)
;; 		       (eth:int<- "cccccccccccccccccccccccccccccccccccccccc")
;; 		       (eth:oct<- "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" 20)
;; 		       )))

(-> abi-encode (<abi-type> t) octet-vector)
(defun abi-encode (type value)
  "Encodes given ABI value as octet-vector"
  (match-of <abi-type> type
    ((abi-uint _) (encode-uint value))
    (abi-address (encode-address value))
    (abi-bool (encode-bool value))
    (abi-bytes (encode-octs value))
    ((abi-bytes-fixed size) (encode-octs value size))
    ((abi-array-fixed size type) (encode-array type value :fixed-size size))
    ((abi-array type) (encode-array type value))
    ((abi-tuple types) (encode-tuple types value))
    ;; 	 (abi-int _)
    ;; 	 abi-string
    (_ (error "ABI encoding unimplemented"))))
