(uiop:define-package #:eth/tests/utils/hash
  (:use #:cl #:eth/utils/hash)
  (:import-from #:parachute
		#:define-test
		#:define-test+run
		#:is)
  (:import-from #:serapeum
		#:octet-vector=
		#:octet-vector)
  (:import-from #:evm/utils
		#:hex->oct)
  (:export #:utils/hash-tests))
(in-package #:eth/tests/utils/hash)


(define-test utils/hash-tests)

(define-test+run keccak256
  :parent utils/hash-tests
  (is #'octet-vector=
      (hex->oct "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
      (keccak256 (octet-vector)))
  (is #'octet-vector=
      (hex->oct "f1885eda54b7a053318cd41e2093220dab15d65381b1157a3633a83bfd5c9239")
      (keccak256 (octet-vector 1 2 3))))

(define-test+run sha256
  :parent utils/hash-tests
  (is #'octet-vector=
      (hex->oct "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      (sha256 (octet-vector)))
  (is #'octet-vector=
      (hex->oct "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81")
      (sha256 (octet-vector 1 2 3))))
