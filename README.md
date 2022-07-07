# Intro

Common Lisp library for interactions with Ethereum and other compatible chains. 
It is not unlike `ethers.js`.

It's written in package inferred style. You can load any subsystem into your image without 
requiring anything else. 
For example, system `eth/utils/rlp` is defined in the file `code/utils/rlp` and it contains utilities for rlp encoding.

## Non-trivial dependecies

- https://github.com/dvush/cl-secp256k1 (not in quicklisp) and it needs native library `libsecp256k1` 
- iolib (ipc provider) needs native lib `libfixposix`

## Status

It's in active development and while a foundation is done there are some major tasks that must be done 
before it can be solidified and published.
When they will be done I'll revisit api as a whole and will probably break something.

- syntax for forking with contract ABI
  - including abi decoding for events
- proper documentation generated from source
- comparison/tests against ehers.js (via deno) and anvil.


# Examples

## Utilities

Converting values between hex, octet-vector(see `serapeum:octet-vector`), integer. 
Similar to `bit-smasher` but tailored to eth use case.
```lisp
;; Concrete conversions
(hex->oct "0xaabbcc") ;; #(#xaa #xbb #xcc)
(hex->int "0xaabbcc") ;; #xaabbcc
;; Flexible conversions
(hex<- 10) ;; "0xa"
(hex<- (octet-vector 10)) ;; "0x0a"
(oct<- 3 20) ;; (octet-vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3)
```

Hash.
```lisp
;; keccak of empty octet-vector
(keccak256 (octet-vector)) ;; (hex->oct "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
```

Unit conversion. Similar to `parse-unit`, `format-unit` in other libraries.
```lisp
(unit-as-int "2.2" :eth) ;; (* 22/10 (expt 10 18))
(unit-as-int 22/10 :eth) ;; (* 22/10 (expt 10 18))
(int-as-unit (* 22/10 (expt 10 18)) :eth) ;; 22/10
```
Available units are: `:eth` `:kwei` `:mwei` `:gwei` or integer representing decimals (i.e. 6 for USDC)

## ABI encoding/decoding

```lisp
; Type description
(abi-tuple (vector abi-address abi-bytes abi-uint))

;; encoding 
(abi-encode type (vector (oct<- #xaabbcc 20) (oct<- #xddeeff) #x101112))
```

## Provider

Set default provider
```lisp
(setf *default-provider* (make-http-provider "http://127.0.0.1:8545"))
```

Call methods
```lisp
;; check balance of ripemd
(eth/get-balance (oct<- 3 20)) ;;
```

## Signing transactions with a secret key.

Make signer
```lisp
(make-secret-key-signer
    (oct<- "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
			
;; Get address
(signer-address signer) ; (oct<- "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 20)
```

Sign transaction
```lisp
(sign-transaction signer
                  :chain-id 1
                  :gas-limit 70000
                  :max-fee-per-gas (unit-as-int 2 :gwei)
                  :max-priority-fee-per-gas (unit-as-int 1 :gwei)
                  :value 2333
                  :to (oct<- "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 20)
                  :data (oct<- "0xdeadbeef")
                  :access-list (dict #'equalp
                                     (oct<- "0x0" 20) (list (oct<- "0x1" 32)
                                                            (oct<- "0x2" 32))
                                     (oct<- "0x1" 20) (list (oct<- "0x3" 32)
                                                            (oct<- "0x4" 32)))
                  :nonce 3)
;; Returns a signed raw transaction that can be sent via (eth/send-raw-tx tx)
```


See `code/tests` for more.


# Testing

1. Load "eth/tests" system
2. (opt.) Set *default-provider* to point to eth mainnet node.
3. (opt.) Check if you have `anvil` from foundry installed.
4. Call `(eth/tests/all:test-all)`.
