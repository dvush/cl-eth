(uiop:define-package #:eth/tests/signer
  (:use #:cl #:eth/signer #:eth/provider #:eth/utils #:eth/tests/test-utils)
  (:import-from #:parachute
		#:true
   		#:define-test
   		#:define-test+run
   		#:is)
  (:import-from #:serapeum
		#:fmt
		#:dict
		#:octet-vector=)
  (:import-from #:alexandria
		#:with-gensyms)
  (:export #:signer-test))
(in-package #:eth/tests/signer)

(define-test signer-test)


(define-test+run can-start-anvil
  :parent signer-test
  (true (try-launch-anvil)))

(defun wait-receipt (tx-hash)
  (dotimes (i 1 (* 1 100))
    (when (eth/get-tx-receipt tx-hash)
      (return))
    (sleep 1/100))
  (eth/get-tx-receipt tx-hash))


(define-test+run send-tx
  :parent signer-test
  :depends-on (can-start-anvil)
  (with-anvil-provider
    (let* ((signer (make-secret-key-signer
		    (oct<- "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80")))
	   (address (signer-address signer))
	   (nonce (eth/get-tx-count address :block-tag :pending))
	   (chain-id (net/version)))
      (is octet-vector=
	  (oct<- "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 20)
	  address)
      ;; try different nonces
      (dotimes (i 5)
	(let* ((tx-hash
      		 (eth/send-raw-tx (sign-transaction signer
      						    :chain-id chain-id
      						    :gas-limit 30000
      						    :gas-price (unit-as-int 1 :gwei)
      						    :value 0
      						    :to address
      						    :data (oct<- "0xdeadbeef")
      						    :nonce nonce)))
	       (status (hex->int (gethash "status" (wait-receipt tx-hash))))
      	       )
      	  (is #'= 1 status "legacy tx")
      	  (incf nonce))

	(let* ((tx-hash
      		 (eth/send-raw-tx (sign-transaction signer
      						    :chain-id chain-id
      						    :gas-limit 50000
      						    :gas-price (unit-as-int 1 :gwei)
      						    :value 2333
      						    :to address
      						    :data (oct<- "0xdeadbeef")
      						    :access-list (dict #'equalp
      						  		       (oct<- "0x0" 20) (list (oct<- "0x1" 32)
      						  					      (oct<- "0x2" 32))
      						  		       (oct<- "0x1" 20) (list (oct<- "0x3" 32)
      						  					      (oct<- "0x4" 32)))
      						    :nonce nonce)))
      	       (receipt (wait-receipt tx-hash))
      	       (status (hex->int (gethash "status" receipt))))
      	  (is #'= 1 status "tx with access list")
      	  (incf nonce))
	(let* ((tx-hash
		 (eth/send-raw-tx (sign-transaction signer
						    :chain-id chain-id
						    :gas-limit 70000
						    :max-fee-per-gas (unit-as-int 2 :gwei)
						    :max-priority-fee-per-gas (unit-as-int 1 :gwei)
						    :value 2333
						    :to address
						    :data (oct<- "0xdeadbeef")
						    :access-list (dict #'equalp
						  		       (oct<- "0x0" 20) (list (oct<- "0x1" 32)
						  					      (oct<- "0x2" 32))
						  		       (oct<- "0x1" 20) (list (oct<- "0x3" 32)
						  					      (oct<- "0x4" 32)))
						    :nonce nonce)))
	       (receipt (wait-receipt tx-hash))
	       (status (hex->int (gethash "status" receipt))))
	  (is #'= 1 status "eip-1559 tx")
	  (incf nonce))))))
