(uiop:define-package #:eth/utils/types
  (:use #:cl)
  (:import-from #:serapeum
		#:octet-vector)
  (:export #:u256
	   #:address
	   #:oct32))
(in-package #:eth/utils/types)

(deftype u256 () '(unsigned-byte 256))
(deftype address () '(octet-vector 20))
(deftype oct32 () '(octet-vector 32))
