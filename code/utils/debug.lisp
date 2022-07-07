(uiop:define-package #:eth/utils/debug
  (:use #:cl #:eth/abi/base)
  (:import-from #:serapeum
		#:octet-vector
		#:make-octet-vector
		#:match-of)
  (:import-from #:eth/utils/convert
		#:oct->hex)
  (:export #:debug-print-oct-chunked))
(in-package #:eth/utils/debug)

(defun debug-print-oct-chunked (octets &optional (chunk-size 32))
  (loop for chunk-start below (length octets) by chunk-size
	do (let* ((chunk-end (min (+ chunk-start chunk-size)
			    	   (length octets)))
		   (chunk (subseq octets chunk-start chunk-end)))
	      (print (oct->hex chunk nil)))))
