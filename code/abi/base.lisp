(uiop:define-package #:eth/abi/base
  (:use #:cl)
  (:import-from #:serapeum
		#:fmt
		#:eval-always
		#:match-of
		#:defconst
		#:defunion)
  (:export #:abi-type
	   #:<abi-type>
	   #:abi-uint
	   #:abi-uint-size
	   #:abi-int
	   #:abi-int-size
	   #:abi-address
	   #:abi-bool
	   #:abi-array
	   #:abi-array-array-type
	   #:abi-array-fixed
	   #:abi-array-fixed-size
	   #:abi-array-fixed-array-type
	   #:abi-bytes
	   #:abi-bytes-fixed
	   #:abi-bytes-fixed-size
	   #:abi-string
	   #:abi-tuple
	   #:abi-tuple-types
	   #:abi-static-type-p
	   #:abi-cannonical-name))
(in-package #:eth/abi/base)

(eval-always
  (defunion abi-type
    (abi-uint        (size (integer 8 256)))
    (abi-int         (size (integer 8 256)))
    abi-address
    abi-bool
    (abi-array       (array-type <abi-type>))
    (abi-array-fixed (size (integer 0))
		     (array-type <abi-type>))
    abi-bytes
    (abi-bytes-fixed (size (integer 1 32)))
    abi-string
    (abi-tuple       (types (or (vector <abi-type>)
				(vector (cons string <abi-type>)))))))


(defconst abi-uint (abi-uint 256))
(defconst abi-int (abi-int 256))
 
(defun abi-static-type-p (abi-type)
  "Given abi-type returns multiple values:
1. boolean indicating if type is static
2. size of head in bytes"
  
  (match-of <abi-type> abi-type
    ;; Always static types
    ((or (abi-uint _)
	 (abi-int _)
	 abi-address
	 abi-bool
	 (abi-bytes-fixed _))
     (values t 32))
    ;; Always dynamic types
    ((or (abi-array _)
	 abi-bytes
	 abi-string)
     (values nil 32))
    ;; Depends on inner type being static
    ((abi-array-fixed array-size inner-type)
     (multiple-value-bind (inner-static? inner-size) (abi-static-type-p inner-type)
			 (if inner-static?
			     (values t (* inner-size array-size))
			     (values nil 32))))
    ((abi-tuple inner-types)
     (loop for inner-type across inner-types
	   with total-size = 0
	   do (multiple-value-bind (inner-static? inner-size) (abi-static-type-p inner-type)
		 (if inner-static?
		     (incf total-size inner-size)
		     (return (values nil 32))))
	   finally (return (values t total-size))))
    (_ (error "Unreachable, unknown type"))))


(defun abi-cannonical-name (abi-type)
  (match-of <abi-type> abi-type
    ((abi-uint d) (fmt "uint~d" d)) 
    ((abi-int d) (fmt "int~d" d))
    (abi-address "address")
    (abi-bool "bool")
    ((abi-bytes-fixed n) (fmt "bytes~d" n))
    (abi-bytes "bytes")
    (abi-string "string")
    ((abi-array ty) (fmt "~a[]" (abi-cannonical-name ty)))
    ((abi-array-fixed size ty) (fmt "~a[~d]" (abi-cannonical-name ty) size))
    ((abi-tuple inner-types) (fmt "(~{~a~^,~})" (map 'list #'abi-cannonical-name inner-types)))
    (_ (error "Unreachable, unknown type"))))


; TODO
#|
const humanReadableAbi = [

  // Simple types
  "constructor(string symbol, string name)",
  "function transferFrom(address from, address to, uint value)",
  "function balanceOf(address owner) view returns (uint balance)",
  "event Transfer(address indexed from, address indexed to, address value)",
  "error InsufficientBalance(account owner, uint balance)",

  // Some examples with the struct type, we use the tuple keyword:
  // (note: the tuple keyword is optional, simply using additional
  //        parentheses accomplishes the same thing)
  // struct Person {
  //   string name;
  //   uint16 age;
  // }
  "function addPerson(tuple(string name, uint16 age) person)",
  "function addPeople(tuple(string name, uint16 age)[] person)",
  "function getPerson(uint id) view returns (tuple(string name, uint16 age))",

  "event PersonAdded(uint indexed id, tuple(string name, uint16 age) person)"
];

|#
