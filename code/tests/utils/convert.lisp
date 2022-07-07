(uiop:define-package #:eth/tests/utils/convert
  (:use #:cl #:eth/utils/convert)
  (:import-from #:parachute
		#:is
		#:define-test)
  (:import-from #:serapeum
		#:octet-vector=
		#:octet-vector)
  (:export #:utils/convert-tests))
(in-package #:eth/tests/utils/convert)


(define-test utils/convert-tests)


(define-test to-oct
  :parent utils/convert-tests
  (is #'octet-vector= (octet-vector)       (hex->oct "0x"))
  (is #'octet-vector= (octet-vector 0)     (hex->oct "0x0"))
  (is #'octet-vector= (octet-vector 1)     (hex->oct "0x1"))
  (is #'octet-vector= (octet-vector 1)     (hex->oct "0x01"))
  (is #'octet-vector= (octet-vector 1 2 3) (hex->oct "0x010203"))

  (is #'octet-vector= (octet-vector 0 0 0) (hex->oct "0x" 3))
  (is #'octet-vector= (octet-vector 0 0 0) (hex->oct "0x0" 3))
  (is #'octet-vector= (octet-vector 0 0 1) (hex->oct "0x1" 3))
  (is #'octet-vector= (octet-vector 0 0 1) (hex->oct "0x01" 3))
  (is #'octet-vector= (octet-vector 1 2 3) (hex->oct "0x010203" 3))

  (is #'octet-vector= (octet-vector)       (int->oct 0))
  (is #'octet-vector= (octet-vector 10)    (int->oct 10))
  (is #'octet-vector= (octet-vector 3 232) (int->oct 1000))


  (is #'octet-vector= (octet-vector 0 0)   (int->oct 0 2))
  (is #'octet-vector= (octet-vector 0 10)  (int->oct 10 2))
  (is #'octet-vector= (octet-vector 3 232) (int->oct 1000 2))

  (is #'octet-vector= (octet-vector 0 3 232)     (oct<- 1000 3))
  (is #'octet-vector= (octet-vector 3 232)       (oct<- 1000))
  (is #'octet-vector= (octet-vector #xfe #xfa)   (oct<- "0xfefa"))
  (is #'octet-vector= (octet-vector 0 #xfe #xfa) (oct<- "fefa" 3))
  (is #'octet-vector= (octet-vector #xfe #xfa)   (oct<- (octet-vector #xfe #xfa)))
  (is #'octet-vector= (octet-vector 0 #xfe #xfa) (oct<- (octet-vector #xfe #xfa) 3)))



(define-test to-int
  :parent utils/convert-tests

  (is #'= 0        (hex->int "0x"))
  (is #'= 0        (hex->int "0x0"))
  (is #'= #x09     (hex->int "0x09"))
  (is #'= 16710395 (hex->int "0xfefafb"))

  (is #'= 0        (oct->int (octet-vector)))
  (is #'= 0        (oct->int (octet-vector #x00)))
  (is #'= #x09     (oct->int (octet-vector #x09)))
  (is #'= 16710395 (oct->int (octet-vector #xfe #xfa #xfb)))

  (is #'= 0        (int<- ""))
  (is #'= 10       (int<- "a"))
  (is #'= 16710395 (int<- (octet-vector #xfe #xfa #xfb)))
  (is #'= 0        (int<- 0))
  (is #'= 1012     (int<- 1012)))


(define-test to-hex
  :parent utils/convert-tests
  (is #'string= "0x"   (oct->hex (octet-vector)))
  (is #'string= ""     (oct->hex (octet-vector) nil))
  (is #'string= "0xaa" (oct->hex (octet-vector #xaa)))
  (is #'string= "0x0a" (oct->hex (octet-vector #xa)))

  (is #'string= "0x0"      (int->hex 0))
  (is #'string= "0xa"      (int->hex 10))
  (is #'string= "0xfefafb" (int->hex 16710395))
  (is #'string= "a"        (int->hex 10 nil))

  (is #'string= "0x"   (hex<- (octet-vector)))
  (is #'string= "0x0a" (hex<- (octet-vector 10)))
  (is #'string= "0xa"  (hex<- 10))
  (is #'string= "0x0"  (hex<- 0)))
