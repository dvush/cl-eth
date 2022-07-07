(uiop:define-package #:eth/utils/hash
  (:use #:cl)
  (:import-from #:serapeum
		#:octet-vector
		#:->)
  (:import-from #:ironclad)
  (:export #:keccak256
	   #:sha256))
(in-package #:eth/utils/hash)



(-> keccak256 (octet-vector) (octet-vector 32))
(defun keccak256 (octs)
  (let ((hash (ironclad:digest-sequence :keccak/256 octs)))
    hash))


(-> sha256 (octet-vector) (octet-vector 32))
(defun sha256 (octs)
  (let ((hash (ironclad:digest-sequence :sha256 octs)))
    hash))
