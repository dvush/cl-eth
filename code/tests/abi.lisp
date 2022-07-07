(uiop:define-package #:eth/tests/abi
  (:use #:cl #:eth/abi/all #:eth/utils/convert)
  (:import-from #:parachute
		#:define-test
		#:define-test+run
		#:is)
  (:import-from #:serapeum
		#:repeat-sequence
		#:octet-vector
		#:octet-vector=)
  (:export #:abi-tests))
(in-package #:eth/tests/abi)


(define-test abi-tests)

(defun abi-enc-test (type value octs)
  ;; test encode
  (is #'octet-vector=
      octs
      (abi-encode type value))
  ;; test decode
  (is #'equalp
      value
      (abi-decode octs type))
  ;; test decode from non-zero offset
  (let ((padded-octs (concatenate 'octet-vector
				  ;; add 17 bytes before and 30 byte after octs 
				  (oct<- (repeat-sequence "fe" 17))
				  octs
				  (oct<- (repeat-sequence "fc" 30)))))
    (is #'equalp
	value
	(abi-decode padded-octs type :start 17))))


(define-test+run abi-encode
  :parent abi-tests
  (abi-enc-test abi-uint
		(int<- "fafeaabbfafeaabbfafeaabbfafeaabb")
		(oct<- "fafeaabbfafeaabbfafeaabbfafeaabb" 32))
  
  (abi-enc-test (abi-uint 256)
		(int<- "fafeaabbfafeaabbfafeaabbfafeaabb")
		(oct<- "fafeaabbfafeaabbfafeaabbfafeaabb" 32))
  (abi-enc-test (abi-uint 8)
		#xfe
		(oct<- #xfe 32))
  (abi-enc-test abi-address
		(oct<- "fafbfefefefefefefefefefefefefefefefefefe" 20)
		(oct<- "fafbfefefefefefefefefefefefefefefefefefe" 32))
  (abi-enc-test abi-bool
		t
		(oct<- 1 32))
  (abi-enc-test abi-bool
		nil
		(oct<- 0 32))
  (abi-enc-test (abi-bytes-fixed 4)
		(oct<- "aabbccdd")
		(concatenate 'octet-vector
			     (oct<- "aabbccdd")
			     (oct<- 0 (- 32 4))))
  (abi-enc-test abi-bytes
		(oct<- "aabbccdd")
		(concatenate 'octet-vector
			     (oct<- 4 32)
			     (oct<- "aabbccdd")
			     (oct<- 0 (- 32 4))))
  (abi-enc-test (abi-array-fixed 2 abi-uint)
		(vector #xaabbcc #xddeeff)
		(concatenate 'octet-vector
			     (oct<- #xaabbcc 32)
			     (oct<- #xddeeff 32)))
  (abi-enc-test (abi-array abi-uint)
		(vector #xaabbcc #xddeeff)
		(concatenate 'octet-vector
			     (oct<- 2 32)
			     (oct<- #xaabbcc 32)
			     (oct<- #xddeeff 32)))
  (abi-enc-test (abi-array-fixed 2 abi-bytes)
		(vector (oct<- #xaabbcc) (oct<- #xddeeff))
		(concatenate 'octet-vector
			     (oct<- (* 2 32) 32) ;; offset to first byte arr
			     (oct<- (* 4 32) 32) ;; offset to second byte arr
			     (oct<- 3 32)
			     (oct<- #xaabbcc) (oct<- 0 (- 32 3))
			     (oct<- 3 32)
			     (oct<- #xddeeff) (oct<- 0 (- 32 3))))
  (abi-enc-test (abi-array abi-bytes)
		(vector (oct<- #xaabbcc) (oct<- #xddeeff))
		(concatenate 'octet-vector
			     (oct<- 2 32)
			     (oct<- (* 2 32) 32) ;; offset to first byte arr
			     (oct<- (* 4 32) 32) ;; offset to second byte arr
			     (oct<- 3 32)
			     (oct<- #xaabbcc) (oct<- 0 (- 32 3))
			     (oct<- 3 32)
			     (oct<- #xddeeff) (oct<- 0 (- 32 3))))
  (abi-enc-test (abi-tuple (vector abi-address abi-bytes abi-uint))
		(vector (oct<- #xaabbcc 20) (oct<- #xddeeff) #x101112)
		(concatenate 'octet-vector
			     ;; headers
			     (oct<- 0 (- 32 3)) (oct<- #xaabbcc 3) ;; address
			     (oct<- (* 3 32) 32)  ;; bytes offset
			     (oct<- #x101112 32) ;; uint
			     (oct<- 3 32) ;; bytes
			     (oct<- #xddeeff) (oct<- 0 (- 32 3)))))

