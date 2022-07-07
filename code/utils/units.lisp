(uiop:define-package #:eth/utils/units
  (:use #:cl)
  (:import-from #:serapeum
		#:->)
  (:import-from #:decimals
		#:format-decimal-number
		#:parse-decimal-number)
  (:export #:unit-as-int
	   #:int-as-unit
	   #:int-as-fmt-unit))
(in-package #:eth/utils/units)


(defun unit-divisor-power (unit)
  (etypecase unit
    (keyword (ecase unit
	       (:wei 0)
	       (:kwei 3)
	       (:mwei 6)
	       (:gwei 9)
	       (:eth 18)))
    (integer unit)))

(defun unit-divisor (unit)
  (expt 10 (unit-divisor-power unit)))

(-> unit-as-int ((or string rational) (or keyword integer)) integer)
(defun unit-as-int (unit-value unit)
  "Converts decimal designator (rational or string) into integer representation of that unit.
Similar to \"parse units\" in other libraries.
- unit values -- :{,k,m,g}wei, :eth, integer representing decimals (e.g. 18 = :eth)

Examples:
(unit-as-int \"0.1\" :eth) ;; -> 100000000000000000"
  
  (let* ((divisor (unit-divisor unit))
	 (decimal-unit-value (etypecase unit-value
			       (string (parse-decimal-number unit-value))
			       (rational unit-value)))
	 (number (* decimal-unit-value divisor)))

    (multiple-value-bind (integer rem) (truncate number)
      (assert (zerop rem) (decimal-unit-value)
	      "Incorrect decimal value for the given unit, too many digits after 0.")
      integer)))

(-> int-as-unit (integer (or keyword integer)) rational)
(defun int-as-unit (int unit)
  "Converts integer representation of unit into rational representation.
Similar to \"format units\" in other libraries but outputs rational instead of string.
- unit values -- :{,k,m,g}wei, :eth, integer representing decimals (e.g. 18 = :eth)

Examples:
(int-as-unit (expt 10 18) :eth) ; -> 1
"
  (let* ((divisor (unit-divisor unit)))
    (/ int divisor)))

(-> int-as-fmt-unit (integer (or keyword integer)) string)
(defun int-as-fmt-unit (int unit)
  "Same as 'int-as-unit but formats result as a decimal string."
  (let* ((divisor-power (unit-divisor-power unit))
	 (divisor (unit-divisor unit))
	 (fmt (format-decimal-number (/ int divisor)
				     :round-magnitude (- divisor-power))))
    fmt))

