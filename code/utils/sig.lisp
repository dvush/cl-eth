(uiop:define-package #:eth/utils/sig
  (:use #:cl #:eth/utils/hash)
  (:import-from #:serapeum
		#:octet-vector
		#:->)
  (:import-from #:ironclad)
  (:import-from #:secp256k1
		#:public-key-serialize
		#:recov-signature-recover
		#:recov-signature-from-components)
  (:export #:ecrecover))
(in-package #:eth/utils/sig)

;(defun ecrecover* (hash rawsig))

(defun ecrecover (hash r s v)
  (let* ((sig (recov-signature-from-components :r r :s s :v v))
	 (pkey (recov-signature-recover sig hash))
	 (pkey-bytes (public-key-serialize pkey)))
    (subseq 
     (keccak256 (subseq pkey-bytes 1))
     #.(- 32 20))))

;(serapeum:example
;  (serapeum:mvlet* ((signer (eth:oct<- "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 20))
;		    (msghash (eth:oct<- "0xd9eba16ed0ecae432b71fe008c98cc872bb4cc214d3220a36f365326cf807d68" 32 ))
;		    (sig (eth:oct<- "0xa461f509887bd19e312c0c58467ce8ff8e300d3c1a90b608a760c5b80318eaf15fe57c96f9175d6cd4daad4663763baa7e78836e067d0163e9a2ccf2ff753f5b1b"))
;		    (r s v (values (subseq sig 0 32) (subseq sig 32 64) (aref sig 64)))
;		    (rec-id (- v 27)))
;    (equalp (ecrecover msghash r s rec-id) signer)))
