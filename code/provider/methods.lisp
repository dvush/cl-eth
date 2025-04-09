(uiop:define-package #:eth/provider/methods
  (:use #:cl #:eth/provider/base
	#:eth/utils)
  (:import-from #:serapeum
		#:length<=
		#:octet-vector
		#:->
		#:last-elt
		#:median)
  (:import-from #:alexandria
		#:required-argument)
  (:export #:web3/client-version
	   #:web3/sha3
	   #:net/version
	   #:net/listening
	   #:eth/protocol-version
	   #:net/peer-count
	   #:eth/syncing
	   #:eth/coinbase
	   #:eth/mining
	   #:eth/hashrate

	   #:eth/gas-price
	   #:eth/fee-history
	   #:suggest-gas-fee
	   #:eth/block-number

	   #:eth/get-balance
	   #:eth/get-storage-at
	   #:eth/get-code
	   #:eth/get-tx-count
	   #:eth/get-block-tx-count
	   #:eth/get-uncle-count
	   #:eth/get-uncle-by-index
	   #:eth/get-block

	   #:eth/send-raw-tx
	   #:eth/get-tx
	   #:eth/get-tx-from-block
	   #:eth/get-tx-receipt

	   #:eth/call
	   #:eth/estimate-gas

	   #:eth/get-logs
	   #:eth/new-block-filter
	   #:eth/new-pending-tx-filter
	   #:eth/new-filter
	   #:eth/get-filter-changes
	   #:eth/uninstall-filter
	   #:debug/trace-transaction))
(in-package #:eth/provider/methods)


(defun web3/client-version (&key (provider *default-provider*))
  (rpc-call provider "web3_clientVersion"))

(defun web3/sha3 (data &key (provider *default-provider*))
  (declare (type octet-vector data))
  (hex->oct (rpc-call provider "web3_sha3" (list (oct->hex data)))))

(defun net/version (&key (provider *default-provider*))
  (parse-integer (rpc-call provider "net_version")))

(defun net/listening (&key (provider *default-provider*))
  (rpc-call provider "net_listening"))

(defun eth/protocol-version (&key (provider *default-provider*))
  (hex->int (rpc-call provider "eth_protocolVersion")))

(defun net/peer-count (&key (provider *default-provider*))
  (hex->int (rpc-call provider "net_peerCount")))

(defun eth/syncing (&key (provider *default-provider*))
  (rpc-call provider "eth_syncing"))

(defun eth/coinbase (&key (provider *default-provider*))
  (rpc-call provider "eth_coinbase"))

(defun eth/mining (&key (provider *default-provider*))
  (rpc-call provider "eth_mining"))

(defun eth/hashrate (&key (provider *default-provider*))
  (hex->int (rpc-call provider "eth_hashrate")))

(defun eth/gas-price (&key (provider *default-provider*))
  (hex->int (rpc-call provider "eth_gasPrice")))

(defun eth/fee-history (block-count &key (newest-block :latest)  reward-percentiles (provider *default-provider*) )
  "see suggest-gas-fee for a simple wrapper"
  (rpc-call provider "eth_feeHistory"
	    (list (int->hex block-count) (block-tag->string newest-block) reward-percentiles)))

(defun eth/block-number (&key (provider *default-provider*))
  (hex->int (rpc-call provider "eth_blockNumber")))

(defun block-tag->string (block-tag)
  (etypecase block-tag
    (integer (int->hex block-tag))
    (keyword
     (assert (member block-tag '(:latest :earliest :pending)))
     (string-downcase (string block-tag)))))

(defun eth/get-balance (address &key (block-tag :latest) (provider *default-provider*)) 
  (declare (type address address))
  (hex->int (rpc-call provider "eth_getBalance"
		      (list (oct->hex address) (block-tag->string block-tag)))))

(defun eth/get-storage-at (address slot &key (block-tag :latest) (provider *default-provider*))
  (declare (type address address)
	   (type oct32 slot))
  (hex->oct
   (rpc-call provider "eth_getStorageAt" (list (oct->hex address) (oct->hex slot) (block-tag->string block-tag)))
   32))


(defun eth/get-code (address &key (block-tag :latest) (provider *default-provider*))
  (declare (type address address))
  (hex->oct (rpc-call provider "eth_getCode" (list (oct->hex address) (block-tag->string block-tag)))))

(defun eth/get-tx-count (address &key (block-tag :latest) (provider *default-provider*))
  (declare (type address address))
  (hex->int (rpc-call provider "eth_getTransactionCount" (list (oct->hex address)
							       (block-tag->string block-tag)))))


(defun eth/get-block-tx-count (block-id &key (provider *default-provider*))
  (declare (type (or oct32 (or u256 keyword)) block-id))

  (let ((result (etypecase block-id
		  (oct32
		   (rpc-call provider "eth_getBlockTransactionCountByHash"
			     (list (oct->hex block-id))))
		  ((or integer keyword)
		   (rpc-call provider "eth_getBlockTransactionCountByNumber"
			     (list (block-tag->string block-id)))))))
    (when (stringp result)
      (hex->int result))))

(defun eth/get-uncle-count (block-id &key (provider *default-provider*))
  (declare (type (or oct32 (or u256 keyword)) block-id))

  (let ((result (etypecase block-id
		  (oct32
		   (rpc-call provider "eth_getUncleCountByBlockHash"
			     (list (oct->hex block-id))))
		  ((or integer keyword)
		   (rpc-call provider "eth_getUncleCountByBlockNumber"
			     (list (block-tag->string block-id)))))))
    (when (stringp result)
      (hex->int result))))

(defun eth/get-uncle-by-index (block-id index &key (provider *default-provider*))
  (declare (type (or oct32 (or u256 keyword)) block-id
		 integer index))
  (let ((result (etypecase block-id
		  (oct32
		   (rpc-call provider "eth_getUncleByBlockHashAndIndex"
			     (list (oct->hex block-id) (int->hex index))))
		  ((or integer keyword)
		   (rpc-call provider "eth_getUncleByBlockNumberAndIndex"
			     (list (block-tag->string block-id) (int->hex index)))))))
    (unless (eql result :null)
      result)))

(defun eth/get-block (block-id  &key full-txs (provider *default-provider*))
  (declare (type (or oct32 (or u256 keyword)) block-id))
  (let ((result (etypecase block-id
		  (oct32
		   (rpc-call provider "eth_getBlockByHash"
			     (list (oct->hex block-id) full-txs)))
		  ((or integer keyword)
		   (rpc-call provider "eth_getBlockByNumber"
			     (list (block-tag->string block-id) full-txs))))))
    (unless (eql result :null)
      result)))


;; ; eth_accounts
;; ; eth_signTransaction

;; ; eth_call
;; ; eth_estimateGas

(defun eth/call (&rest args &key provider to from gas gas-price value data (block :latest))
  (declare (ignore provider to from gas gas-price value data block))
  (hex->oct
   (apply #'call-like-method "eth_call" args)))

(defun eth/estimate-gas (&rest args &key provider to from gas gas-price value data (block :latest))
  (declare (ignore provider to from gas gas-price value data block))
  (hex->int
   (apply #'call-like-method "eth_estimateGas" args)))

(defun call-like-method (method &key (provider *default-provider*)
		      (to (required-argument 'to))
		      from 
		      gas gas-price
		      value
		      data
		      (block :latest))
  (declare (type address to)
	   (type (or null address) from)
	   (type (or null u256) gas gas-price value)
	   (type (or null octet-vector) data)
	   (type (or null u256 keyword) block))
  (let ((call (make-hash-table :test #'equal)))
    (setf (gethash "to" call) (oct->hex to))
    (when from
      (setf (gethash "from" call) (oct->hex from)))
    (when gas
      (setf (gethash "gas" call) (int->hex gas)))
    (when gas-price
      (setf (gethash "gasPrice" call) (int->hex gas-price)))
    (when value
      (setf (gethash "value" call) (int->hex value)))
    (when data
      (setf (gethash "data" call) (oct->hex data)))
    (rpc-call provider method (list call (block-tag->string block)))))


(defun eth/send-raw-tx (data &key (provider *default-provider*))
  (declare (type octet-vector data))
  (hex->oct 
   (rpc-call provider "eth_sendRawTransaction" (list (oct->hex data)))
   32))


(defun eth/get-tx (hash &key (provider *default-provider*))
  (declare (type oct32 hash))
  (let ((result (rpc-call provider "eth_getTransactionByHash" (list (oct->hex hash)))))
    (unless (eql result :null)
      result)))

(defun eth/get-tx-from-block (block-id index &key (provider *default-provider*))
  (declare (type (or oct32 (or integer keyword)) block-id
 		 integer index))
  (let ((result (etypecase block-id
 		  (oct32
 		   (rpc-call provider "eth_getTransactionByBlockHashAndIndex"
 			     (list (oct->hex block-id) (int->hex index))))
 		  ((or integer keyword)
 		   (rpc-call provider "eth_getTransactionByBlockNumberAndIndex"
 			     (list (block-tag->string block-id) (int->hex index)))))))
    (unless (eql result :null)
      result)))

(defun eth/get-tx-receipt (hash &key (provider *default-provider*))
  (declare (type oct32 hash))
  (let ((result (rpc-call provider "eth_getTransactionReceipt" (list (oct->hex hash)))))
    (unless (eql result :null)
      result)))


(defun event-filter-args->rpc-args (from-block to-block address topics)
  (declare (type (or null u256 keyword) from-block to-block)
	   (type (or null address) adress)
	   (type (or null sequence) topics))
  (let ((opt (make-hash-table)))
    (when from-block
      (setf (gethash "fromBlock" opt) (block-tag->string from-block)))
    (when to-block
      (setf (gethash "toBlock" opt) (block-tag->string to-block)))
    (when address
      (setf (gethash "address" opt) (oct->hex address)))
    (when topics
      (assert (length<= topics 4))
      (setf (gethash "topics" opt)
	    (map 'vector
		 (lambda (el)
		   (typecase el
		     (octet-vector (oct->hex el))
		     (null :null)
		     (sequence (map 'vector
				    (lambda (topic)
				      (check-type topic oct32 "Incorrect topic in topics specifier.")
				      (oct->hex topic))
				    el))
		     (t (error "Incorrect event topic specifier"))))
		 topics)))
    opt))

(defun eth/get-logs (&key (provider *default-provider*) from-block to-block address topics)
  (rpc-call provider "eth_getLogs" (list (event-filter-args->rpc-args from-block to-block address topics))))

(defun eth/new-block-filter (&key (provider *default-provider*))
  (rpc-call provider "eth_newBlockFilter"))

(defun eth/new-pending-tx-filter (&key (provider *default-provider*))
  (rpc-call provider "eth_newPendingTransactionFilter"))

(defun eth/new-filter (&key (provider *default-provider*) from-block to-block address topics)
  (rpc-call provider "eth_newFilter" (list (event-filter-args->rpc-args from-block to-block address topics))))

(defun eth/get-filter-changes (filter &key (provider *default-provider*))
  (rpc-call provider "eth_getFilterChanges" (list filter)))

(defun eth/uninstall-filter (filter &key (provider *default-provider*))
  (rpc-call provider "eth_uninstallFilter" (list filter)))


(defun debug/trace-transaction (hash &key (provider *default-provider*) tracer)
  (declare (type oct32 hash))
  (let ((options (make-hash-table)))
    (when tracer
      (setf (gethash "tracer" options) tracer))
    (rpc-call provider "debug_traceTransaction" (list (oct->hex hash) options))))



(defun suggest-gas-fee (&key (provider *default-provider*)
			     (block-count 5)
			     (newest-block :latest)
			     (base-fee-min-max-percentage-add 15)
			     (tip-percentile 10))
  (let* ((fee-history (eth/fee-history
		       block-count
		       :reward-percentiles (list tip-percentile)
		       :newest-block newest-block
		       :provider provider))
	 
	 ;; base fee = last base fee + (max - min)*base-fee-min-max-percentage-add% 
	 (base-fees (map 'vector #'hex->int (gethash "baseFeePerGas" fee-history)))
	 (max  (reduce #'max base-fees))
	 (min  (reduce #'min base-fees))
	 (var (- max min))
	 (var-pct (/ base-fee-min-max-percentage-add 100))
	 (last (last-elt base-fees))
	 (suggested-base-fee (+ last (ceiling (* var var-pct))))
	 
	 ;; tip = median across blocks of (tip-percentile of txs in the block)
	 (rewards-percentile (map 'vector (lambda (x) (hex->int (elt x 0))) (gethash "reward" fee-history)))
	 (suggested-tip (median rewards-percentile)))
    
    (list :base-fee suggested-base-fee :tip suggested-tip)))
