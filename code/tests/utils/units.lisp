(uiop:define-package #:eth/tests/utils/units
  (:use #:cl #:eth/utils/units)
  (:import-from #:parachute
		#:define-test
		#:define-test+run
		#:is)
  (:export #:utils/units-tests))
(in-package #:eth/tests/utils/units)


(define-test utils/units-tests)


(define-test+run unit-as-int
  :parent utils/units-tests
  (is #'= (expt 10 18)             (unit-as-int 1 :eth))
  (is #'= (expt 10 17)             (unit-as-int 1/10 :eth))
  (is #'= (* 22/10 (expt 10 18))   (unit-as-int "2.2" :eth))
  (is #'= (unit-as-int 1000 :wei)  (unit-as-int 1 :kwei))
  (is #'= (unit-as-int 1000 :kwei) (unit-as-int 1 :mwei))
  (is #'= (unit-as-int 1000 :mwei) (unit-as-int 1 :gwei))
  (is #'= (unit-as-int 123 :eth)   (unit-as-int 123 18))
  (is #'= (unit-as-int 123 :gwei)  (unit-as-int 123 9)))


(define-test+run int-as-unit
  :parent utils/units-tests
  (is #'= 1     (int-as-unit (expt 10 18):eth))
  (is #'= 1/10  (int-as-unit (expt 10 17) :eth))
  (is #'= 22/10 (int-as-unit (* 22/10 (expt 10 18)) :eth))
  (is #'= 1     (int-as-unit 1000 :kwei))
  (is #'= 1     (int-as-unit (expt 10 6) :mwei))
  (is #'= 1     (int-as-unit (expt 10 9) :gwei))
  (is #'= 1     (int-as-unit (expt 10 7) 7)))

(define-test+run int-as-fmt-unit
  :parent utils/units-tests
  (is #'string= "1"   (int-as-fmt-unit (expt 10 18) :eth))
  (is #'string= "0.1" (int-as-fmt-unit (expt 10 17) :eth))
  (is #'string= "2.2" (int-as-fmt-unit (* 22/10 (expt 10 18)) :eth)))

