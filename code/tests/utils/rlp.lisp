(uiop:define-package #:eth/tests/utils/rlp
  (:use #:cl #:eth/utils/rlp)
  (:import-from #:parachute
		#:define-test
		#:define-test+run
		#:is)
  (:import-from #:serapeum
		#:octet-vector
		#:octet-vector=)
  (:import-from #:eth/utils/convert
		#:oct<-)
  (:export #:utils/rlp-tests))
(in-package #:eth/tests/utils/rlp)


(define-test utils/rlp-tests)

(define-test+run rlp-encode
  :parent utils/rlp-tests
  (is #'octet-vector=
      (octet-vector #x83 1 2 3)
      (rlp-encode (octet-vector 1 2 3)))
  (is #'octet-vector=
      (octet-vector #xc8 #x83 1 2 3 #x83 4 5 6) 
      (rlp-encode (list (octet-vector 1 2 3) (octet-vector 4 5 6))))
  (is #'octet-vector=
      (octet-vector #x80) 
      (rlp-encode (octet-vector)))
  (is #'octet-vector=
      (octet-vector #xc0) 
      (rlp-encode (list)))
  (is #'octet-vector=
      (octet-vector #x80) 
      (rlp-encode (oct<- 0)))
  (is #'octet-vector=
      (octet-vector #x82 #x04 #x00) 
      (rlp-encode (oct<- 1024)))
  (is #'octet-vector=
      (octet-vector #xc7 #xc0 #xc1 #xc0 #xc3 #xc0 #xc1 #xc0) 
      (rlp-encode '(() (()) (() (()))))))

