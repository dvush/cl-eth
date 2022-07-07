(uiop:define-package #:eth/signer
  (:use #:cl #:eth/utils)
  (:import-from #:alexandria
		#:required-argument)
  (:import-from #:serapeum
		#:octet-vector
		#:mvlet*
		#:op
		#:do-hash-table
		#:assure
		#:make
		#:->)
  (:import-from #:secp256k1
		#:recov-signature-destructure*
		#:recov-signature-destructure
		#:recov-signature-sign
		#:public-key-serialize
		#:public-key-create
		#:make-secret-key)
  (:export #:secret-key-signer
	   #:secret-key-secp256k1-key
	   #:signer-address
	   #:make-secret-key-signer
	   #:sign-transaction))
(in-package #:eth/signer)


(defclass secret-key-signer ()
  ((secret-key
    :initform (required-argument 'key)
    :initarg :key
    :reader secret-key-secp256k1-key)
   (address
    :initform (required-argument 'address)
    :initarg :address
    :reader signer-address)))

(-> make-secret-key-signer (oct32) secret-key-signer)
(defun make-secret-key-signer (secret-key)
  (let* ((secret-key    (make-secret-key secret-key))
	 (pub-key       (public-key-create secret-key))
	 (pub-key-bytes (public-key-serialize pub-key))
	 (pub-key-hash  (keccak256 (subseq pub-key-bytes 1)))
	 (address       (subseq pub-key-hash (- 32 20))))
    (make 'secret-key-signer :key secret-key :address address)))


(defun access-list-to-rlp-input (access-list)
  (when access-list
    (loop for address being the hash-keys in access-list using (hash-value slots)
	  collect
	     (let ((address (assure address address))
		   (slots (map 'list (op (assure oct32 _)) slots)))
	       (list address slots)))))

(serapeum:example
  (access-list-to-rlp-input (serapeum:dict #'equalp
					   (oct<- 1 20) (list (oct<- 0 32)
							      (oct<- 1 32)
							      (oct<- 2 32))
					   (oct<- 2 20) '())))


(defmethod sign-transaction ((sig secret-key-signer)
			     &key (nonce (required-argument 'nonce))
				  (data (octet-vector))
				  (to (required-argument 'to))
				  (value 0)
				  gas-price max-priority-fee-per-gas max-fee-per-gas
				  (gas-limit (required-argument 'gas-limit))
				  (access-list nil access-list-provided)
				  (chain-id (required-argument 'chain-id)))
  "Sings transaction and returns encoded raw transaction.
- either gas-price or (max-priority-fee-per-gas and max-fee-per-gas) must be set
- access-list is either nil or (hash-table address => (seqence slots))"
  (assert (if gas-price
	      (not (or max-priority-fee-per-gas max-fee-per-gas))
	      (and max-priority-fee-per-gas max-fee-per-gas))
	  (gas-price max-priority-fee-per-gas max-fee-per-gas)
	  "Either gas-price should be set or both of max-priority-fee-per-gas and max-fee-per-gas.")
  (when (and max-priority-fee-per-gas max-fee-per-gas)
    (assert (>= max-fee-per-gas max-priority-fee-per-gas)
	    (max-priority-fee-per-gas max-fee-per-gas)
	    "max-fee-per-gas can't be less then max-priority-fee-per-gas
becaues is is defined as max-fee-per-gas = max-priority-fee-per-gas + base-fee"))
  (with-slots (secret-key) sig
    (cond ((and gas-price (not access-list-provided)) ;; legacy tx signed with EIP-155
	   (mvlet* ((signed-data (rlp-encode (list (int->oct nonce)
						 (int->oct gas-price)
						 (int->oct gas-limit)
						  to
						 (int->oct value)
						 data
						 (int->oct chain-id)
						 (int->oct 0)
						 (int->oct 0))))
		  (signed-hash32 (keccak256 signed-data))
		  (signature (recov-signature-sign signed-hash32 secret-key))
		  (r s v (recov-signature-destructure* signature))
		  (r s (values (oct->int r) (oct->int s)))
		  (v-eip155 (+ 35 (* chain-id 2) v))
		  (raw-transaction (rlp-encode (list (int->oct nonce)
						     (int->oct gas-price)
						     (int->oct gas-limit)
						     to
						     (int->oct value)
						     data
						     (int->oct v-eip155)
						     (int->oct r)
						     (int->oct s)))))
	     raw-transaction))
	  ((and gas-price access-list-provided) ;; EIP-2930
	   (mvlet* ((signed-data
	 	    (concatenate 'octet-vector
				 (octet-vector #x01)
	 			 (rlp-encode (list (int->oct chain-id)
	 					   (int->oct nonce)
	 					   (int->oct gas-price)
	 					   (int->oct gas-limit)
						   to
	 					   (int->oct value)
	 					   data
	 					   (access-list-to-rlp-input access-list)))))
	 	  (signed-hash32 (keccak256 signed-data))
		    (signature (recov-signature-sign signed-hash32 secret-key))
	 	  (r s v (recov-signature-destructure* signature))
		  (r s (values (oct->int r) (oct->int s)))
	 	  (raw-transaction
	 	    (concatenate 'octet-vector
	 			 (octet-vector #x01)
	 			 (rlp-encode (list (int->oct chain-id)
	 					   (int->oct nonce)
	 					   (int->oct gas-price)
	 					   (int->oct gas-limit)
	 					   to
	 					   (int->oct value)
	 					   data
	 					   (access-list-to-rlp-input access-list)
	 					   (int->oct v)
						   (int->oct r)
						   (int->oct s))))))
	     raw-transaction))
	  (t ;; EIP-1559
	   (mvlet* ((signed-data
		    (concatenate 'octet-vector
				 (octet-vector #x02)
				 (rlp-encode (list (int->oct chain-id)
						   (int->oct nonce)
						   (int->oct max-priority-fee-per-gas)
						   (int->oct max-fee-per-gas)
						   (int->oct gas-limit)
						   to
						   (int->oct value)
						   data
						   (access-list-to-rlp-input access-list)))))
		  (signed-hash32 (keccak256 signed-data))
		  (signature (recov-signature-sign signed-hash32 secret-key))
		  (r s v (recov-signature-destructure* signature))
		  (r s (values (oct->int r) (oct->int s)))
		  (raw-transaction
		    (concatenate 'octet-vector
				 (octet-vector #x02)
				 (rlp-encode (list (int->oct chain-id)
						   (int->oct nonce)
						   (int->oct max-priority-fee-per-gas)
						   (int->oct max-fee-per-gas)
						   (int->oct gas-limit)
						   to
						   (int->oct value)
						   data
						   (access-list-to-rlp-input access-list)
						   (int->oct v)
						   (int->oct r)
						   (int->oct s))))))
	     raw-transaction)))))
