(uiop:define-package #:eth/abi/decoding
  (:use #:cl #:eth/abi/base)
  (:import-from #:serapeum
		#:match-of
		#:true
		#:op
		#:length<
		#:mvlet*
		#:defsubst
		#:~>>
		#:octet-vector
		#:->)
  (:import-from #:eth/utils/convert
		#:oct->int)
  (:import-from #:eth/utils/types
		#:u256 #:address)
  (:import-from #:alexandria
		#:when-let
		#:ensure-cons
		#:if-let
		#:maxf
		#:array-index)
  (:export #:*abi-decode-strict*
	   #:*abi-decode-tuple-as-hash-table*
	   #:abi-decode))
(in-package #:eth/abi/decoding)

(defvar *abi-decode-strict* nil
  "If t values are validated when decoded
e.g for address
- if strict - first 12 bytes should be 0
- if not strict - first 12 bytes are ignored ")

(defvar *abi-decode-tuple-as-hash-table* t
  "Controls tuple decoding
- if t tries to decode tuple as hashtable (if names are present in type declaration)
- if nil decodes tuple as vector")

(defsubst advance-size (oct offset size)
  (values
   (subseq oct offset (+ offset size))
   (+ offset size)))

(defsubst oct-zero (oct)
  (every #'zerop oct))

(-> decode-uint (octet-vector array-index &optional (integer 8 256))
    (values u256 array-index))
(defun decode-uint (oct offset &optional (uint-size 256))
  (mvlet* ((uint-bytes (floor uint-size 8))
	   (prefix offset (advance-size oct offset (- 32 uint-bytes)))
	   (oct-val offset (advance-size oct offset uint-bytes))
	   (value (oct->int oct-val)))
    (when *abi-decode-strict*
      (assert (oct-zero prefix)))
    (values value offset)))

(-> decode-address (octet-vector array-index)
    (values address array-index))
(defun decode-address (oct offset)
  (mvlet* ((prefix offset (advance-size oct offset 12))
	   (value offset (advance-size oct offset 20)))
    (when *abi-decode-strict*
      (assert (oct-zero prefix)))
    (values value offset)))

(-> decode-bool (octet-vector array-index)
    (values boolean array-index))
(defun decode-bool (oct offset)
  (mvlet* ((num-value offset (decode-uint oct offset))
	   (value (not (zerop num-value))))
    (values value offset)))

(-> decode-octs (octet-vector array-index &optional (integer 1 32))
    (values octet-vector array-index))
(defun decode-octs (oct offset &optional fixed-size)
  (mvlet* ((size offset (if fixed-size
			    (values fixed-size offset)
			    (decode-uint oct offset)))
	   (value offset (advance-size oct offset size))
	   (end-padding-size (- (* (ceiling size 32) 32) size))
	   (end-padding offset (advance-size oct offset end-padding-size)))
    (when *abi-decode-strict*
      (assert (oct-zero end-padding)))
    (values value offset)))


(defun decode-from-heads (oct offset next-type)
  (mvlet* ((head-offset offset)
	   (max-read-offset offset)
	   (values (list)))
    (labels ((read-value-at (offset type)
	       (mvlet* ((value offset (decode oct offset type)))
		 (push value values)
		 (maxf max-read-offset offset)
		 offset))
	     (decode-next-static (type)
	       (let ((new-head-offset (read-value-at head-offset type)))
		 (setf head-offset new-head-offset)))
	     (decode-next-dynamic (type)
	       (mvlet* ((tail-offset new-head-offset (decode-uint oct head-offset)))
		 (setf head-offset new-head-offset)
		 (read-value-at (+ offset tail-offset) type)))
	     (decode-next (type)
	       (if (abi-static-type-p type)
		   (decode-next-static type)
		   (decode-next-dynamic type)))
	     (result ()
	       (values (apply #'vector (reverse values))
		       max-read-offset)))

      (loop while (when-let ((type (funcall next-type)))
		    (decode-next type)))
      (result))))

(-> decode-array (octet-vector array-index <abi-type> &optional (integer 1))
    (values vector array-index))
(defun decode-array (oct offset type &optional fixed-size)
  (mvlet* ((size offset (if fixed-size
			    (values fixed-size offset)
			    (decode-uint oct offset)))
	   (next-type (lambda ()
			(decf size)
			(when (>= size 0)
			  type)))
	   (values offset (decode-from-heads oct offset next-type)))
    (values values offset)))

(defsubst tuple-types-have-names (types)
  (every (lambda (type) (and (consp type)
			     (stringp (car type))))
	 types))

(defsubst tuple-values-as-hash-table (types values)
  (let ((result (make-hash-table :test #'equal)))
    (loop for (name . type) across types
	  for value across values
	  do (setf (gethash name result) value))
    result))

(-> decode-tuple (octet-vector array-index vector)
    (values (or vector hash-table) array-index))
(defun decode-tuple (oct offset types)
  (mvlet* ((next-type (let ((idx 0))
			  (lambda ()
			    (when (length< idx types)
			      (let* ((var (aref types idx))
				     (var (if (consp var)
					      (cdr var)
					      var)))
				(incf idx)
				var)))))
	   (values offset (decode-from-heads oct offset next-type))
	   (values (if (and *abi-decode-tuple-as-hash-table*
			    (tuple-types-have-names types))
		       (tuple-values-as-hash-table types values)
		       values)))
    (values values offset)))

(-> decode (octet-vector array-index <abi-type>)
    (values t array-index))
(defun decode (oct offset type)
  (match-of <abi-type> type
    ((abi-uint size) (decode-uint oct offset size))
    (abi-address (decode-address oct offset))
    (abi-bool (decode-bool oct offset))
    (abi-bytes (decode-octs oct offset))
    ((abi-bytes-fixed size) (decode-octs oct offset size))
    ((abi-array-fixed size type) (decode-array oct offset type size))
    ((abi-array type) (decode-array oct offset type))
    ((abi-tuple types) (decode-tuple oct offset types))
    ;; 	 (abi-int _)
    ;; 	 abi-string
    (_ (error "ABI decoding unimplemented"))))


(-> abi-decode (octet-vector <abi-type> &key (:start array-index))
    (values t array-index))
(defun abi-decode (oct type &key (start 0))
  "Reads ABI value from octet-vector 'oct starting as :start.
returns:
- Decoded value
- Index of the first byte untouched by decoding
  (i.e. Index - start = bytes consumed))"
  (decode oct start type))
