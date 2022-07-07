(uiop:define-package #:eth/tests/provider
  (:use #:cl #:eth/provider/all #:eth/utils/all)
  (:import-from #:parachute
		#:of-type
		#:finish
		#:false
		#:isnt
		#:define-test
		#:define-test+run
		#:is)
  (:import-from #:serapeum
		#:octet-vector)
  (:export #:provider-tests))
(in-package #:eth/tests/provider)

;; block with 1 uncle block
(defparameter *block-hash* (oct<- "0x56973e89d1cf462d7a636f8aa6ac12d52ff132fd584249713f337f1daa29603c"))
(defparameter *block-num* 14055954)
(defparameter *non-block-hash* (oct<- "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
(defparameter *non-block-num* 999999999)
;; erc20 (DAI)
(defparameter *contract-addr* (oct<- "0x6B175474E89094C44Da98b954EedeAC495271d0F"))
(defparameter *eoa-addr* (oct<- "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))
(defparameter *tx-hash* (oct<- "0x93d0c3547d49af9cca5b26385a84670d1dff27be6cbe812cedb59b80981ccdb4" 32))
(defparameter *non-tx-hash* (oct<- "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" 32))
(defparameter *event-transfer* (oct<- "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"))

(define-test provider-tests)

(define-test provider-set
  :parent provider-tests
  (false (null *default-provider*)))

(defmacro ignore-jsonrpc-error (call)
  "Ignore jsonrpc error because some methods are not supported by infura, basic geth setup."
  `(handler-case ,call
    (jsonrpc-error ())))

(define-test+run sanity-test
  :parent provider-tests
  :depends-on (provider-set)
  (finish (web3/client-version))
  (finish (web3/sha3 (oct<- "aabbcc")))
  (finish (net/version))
  (finish (net/listening))
  (finish (ignore-jsonrpc-error (eth/protocol-version)))
  (finish (net/peer-count))
  (finish (eth/syncing))
  (finish (ignore-jsonrpc-error (eth/coinbase)))
  (finish (eth/mining))
  (finish (eth/hashrate))
  (of-type integer (eth/gas-price))
  (of-type integer (eth/block-number))

  (of-type integer (eth/get-balance *eoa-addr*))
  (of-type oct32 (eth/get-storage-at *contract-addr* (oct<- 0 32)))
  (of-type oct32 (eth/get-storage-at *eoa-addr* (oct<- 0 32)))
  (of-type octet-vector (eth/get-code *contract-addr*))
  (of-type octet-vector (eth/get-code *eoa-addr*))
  (of-type integer (eth/get-tx-count *eoa-addr*))
  (of-type integer (eth/get-tx-count *contract-addr*))
  (of-type integer (eth/get-block-tx-count :latest))
  (of-type integer (eth/get-block-tx-count *block-hash*))
  (of-type integer (eth/get-block-tx-count *block-num*))
  (of-type null (eth/get-block-tx-count *non-block-hash*))
  (of-type null (eth/get-block-tx-count *non-block-num*))


  (of-type integer (eth/get-uncle-count :latest))
  (of-type integer (eth/get-uncle-count *block-hash*))
  (of-type integer (eth/get-uncle-count *block-num*))
  (of-type null (eth/get-uncle-count *non-block-hash*))
  (of-type null (eth/get-uncle-count *non-block-num*))

  (of-type (or null hash-table) (eth/get-uncle-by-index :latest 0))
  (of-type hash-table (eth/get-uncle-by-index *block-hash* 0))
  (of-type hash-table (eth/get-uncle-by-index *block-num* 0))
  (of-type null (eth/get-uncle-by-index :latest 1))
  (of-type null (eth/get-uncle-by-index *block-hash* 1))
  (of-type null (eth/get-uncle-by-index *block-num* 1))
  (of-type null (eth/get-uncle-by-index *non-block-hash* 0))
  (of-type null (eth/get-uncle-by-index *non-block-num* 0))


  (of-type hash-table (eth/get-block :latest))
  (of-type hash-table (eth/get-block *block-hash*))
  (of-type hash-table (eth/get-block *block-num*))
  (of-type null (eth/get-block *non-block-hash*))
  (of-type null (eth/get-block *non-block-num*))


  (of-type hash-table (eth/get-tx *tx-hash*))
  (of-type null (eth/get-tx *non-tx-hash*))

  (of-type hash-table (eth/get-tx-from-block *block-hash* 0))
  (of-type null (eth/get-tx-from-block *non-block-hash* 0))


  (of-type hash-table (eth/get-tx-receipt *tx-hash*))
  (of-type null (eth/get-tx-receipt *non-tx-hash*))

  (of-type octet-vector (eth/call :to *contract-addr* :data (oct<- "313ce567")))
  (of-type octet-vector (eth/call :to *contract-addr*
				  :data (oct<- "313ce567")
				  :from *eoa-addr*
				  :gas 100000
				  :gas-price (unit-as-int 100 :gwei)
				  :value 0
				  :block :latest))

  (of-type vector (eth/get-logs :from-block *block-num*
				:to-block *block-num*
				:topics (list *event-transfer*)))
  (of-type vector (eth/get-logs :from-block *block-num*
				:to-block *block-num*
				:topics (list (list *event-transfer*
						    *event-transfer*)))))


