(uiop:define-package #:eth/utils/rlp
  (:use #:cl)
  (:import-from #:serapeum
		#:octet-vector
		#:->)
  (:import-from #:eth/utils/convert
		#:int->oct)
  (:export #:rlp-encode))
(in-package #:eth/utils/rlp)


(-> rlp-encode ((or octet-vector sequence)) octet-vector)
(defun rlp-encode (value)
  "Returns RLP encoding of value. 
- values is either:
  - 'ocetet-vector
  - sequence of 'octet-vector
- empty 'octet-vector represents nothing (i.e. null in other impls)
- empty list, nil - represents empty list (i.e. [] in other impls)"
  (labels ((encode-length (length offset)
	     (if (< length 56)
		 (octet-vector (+ length offset))
		 (let* ((binary-length (int->oct length))
			(binary-length-size (length binary-length))
			(result (concatenate 'octet-vector
					     (octet-vector (+ binary-length-size offset 55))
					     binary-length)))
		   result))))
  (etypecase value
    (octet-vector
     (let ((value-length (length value)))
       (if (and (= value-length 1) (< (elt value 0) #x80))
	   value
	   (concatenate 'octet-vector
			(encode-length value-length #x80)
			value))))
    (sequence (let* ((encoded-elements (mapcar #'rlp-encode value))
		 (elements-length  (reduce #'+ (mapcar #'length encoded-elements))))
	    (apply #'concatenate 'octet-vector
		   (encode-length elements-length #xc0)
		   encoded-elements))))))
