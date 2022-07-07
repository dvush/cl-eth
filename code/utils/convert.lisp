(uiop:define-package #:eth/utils/convert
  (:use #:cl)
  (:import-from #:serapeum
		#:drop-while
		#:equals
		#:pad-start
		#:defsubst
		#:concat
		#:octet-vector
		#:->
		#:defconst
		#:length>
		#:drop-prefix)
  (:import-from #:bit-smasher)
  (:import-from #:alexandria
		#:array-length)
  (:export #:hex->oct
	   #:hex->int
	   #:oct->hex
	   #:oct->int
	   #:int->oct
	   #:int->hex
	   #:hex<-
	   #:oct<-
	   #:int<-)
  (:documentation "Defines conversions between 3 types:
- hex -- hex encoded bytes with optional 0x prefix
- oct -- octet-vector
- int -- integer"))
(in-package #:eth/utils/convert)

;; hex, oct, int
;; hex<- possible padding with 0
;; oct<- possible padding with 0

(defsubst oct-ensure-sized (oct &optional size)
  (if size
      (let ((oct (pad-start oct size 0)))
	(when (length> oct size)
	  (error "Size of result is bigger than specified size."))
	oct)
      oct))


(-> hex->oct (string &optional array-length) octet-vector)
(defun hex->oct (hex &optional byte-size)
  "Converts hex string to octets.
- 'hex can have 0x prefix
- 'hex is right padded to even length with 0 (e.g. \"a\" = \"0a\")
- if 'byte-size is specified right pads result to the length of 'byte-size
  raises error if result is bigger than 'byte-size"
  (let* ((hex (drop-prefix "0x" hex))
	 (hex (if (evenp (length hex)) hex (concat "0" hex)))
	 (oct (bit-smasher:hex->octets hex))
	 (oct (oct-ensure-sized oct byte-size)))
    oct))


(-> hex->int (string) integer)
(defun hex->int (hex)
  "Converts hex string to integer.
- 'hex is big-endian byte encoding of integer
- 'hex can have 0x prefix
- leading 0 are ignored"
  (let ((res (bit-smasher:octets->int (hex->oct hex))))
    res))


(-> oct->hex (octet-vector &optional boolean) string)
(defun oct->hex (oct &optional (prefix t))
  "Converts octet-vector to hex string.
- when 'prefix is t (defualt) 0x prefix is appended"
  (let* ((hex (bit-smasher:octets->hex oct))
	 (hex (if prefix (concat "0x" hex) hex)))
    hex))

(-> oct->int (octet-vector) integer)
(defun oct->int (oct)
"Converts octet-vector to integer.
- 'oct is big-endian byte encoding of result"
  (let ((int (bit-smasher:octets->int oct)))
    int))


(-> int->oct (integer &optional array-length) octet-vector)
(defun int->oct (int &optional byte-size)
"Converts integer to octet-vector.
- result is big-endian byte encoding of 'int
- if 'byte-size is specified right pads result to the length of 'byte-size
  raises error if result is bigger than 'byte-size"
  (let* ((oct (bit-smasher:int->octets int))
	 (oct (if byte-size
		  (oct-ensure-sized oct byte-size)
		  (drop-while (equals #x00) oct))))
    oct))

(-> int->hex (integer &optional boolean) string)
(defun int->hex (int &optional (prefix t))
"Converts integer to hex
- result is hex-encoded big-endian byte encoding of 'int
- when 'prefix is t (defualt) 0x prefix is appended"
  (let* ((hex (oct->hex (int->oct int) nil))
	 (hex (drop-while (equals #\0) hex))
	 (hex (if (string= hex "") "0" hex))
	 (hex (if prefix (concat "0x" hex) hex)))
    hex))


(-> hex<- ((or string octet-vector integer) &optional boolean) string)
(defun hex<- (hex-designator &optional (prefix t))
  "Converts integer, octet-vector to hex, strings are passed without change."
  (etypecase hex-designator
    (string hex-designator)
    (octet-vector (oct->hex hex-designator prefix))
    (integer (int->hex hex-designator))))


(-> oct<- ((or string octet-vector integer) &optional array-length) octet-vector)
(defun oct<- (oct-designator &optional byte-size)
  "Converts integer, hex strings, octet-vector to octet-vector of size byte-size.
- if 'byte-size is specified right pads result to the length of 'byte-size
  raises error if result is bigger than 'byte-size "
  (etypecase oct-designator
    (string (hex->oct oct-designator byte-size))
    (octet-vector (oct-ensure-sized oct-designator byte-size))
    (integer (int->oct oct-designator byte-size))))

(-> int<- ((or string octet-vector integer)) integer)
(defun int<- (int-designator)
  "Converts integer, hex string, octet-vector to integer."
  (etypecase int-designator
    (string (hex->int int-designator))
    (octet-vector (oct->int int-designator))
    (integer int-designator)))
