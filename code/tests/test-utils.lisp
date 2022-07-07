(uiop:define-package #:eth/tests/test-utils
  (:use #:cl #:eth/provider/all)
  (:import-from #:serapeum
		#:fmt)
  (:import-from #:uiop
		#:process-alive-p
		#:launch-program
		#:terminate-process)
  (:import-from #:alexandria
		#:with-gensyms)
  (:export #:with-anvil-provider
	   #:*anvil-port*
	   #:try-launch-anvil))
(in-package #:eth/tests/test-utils)

(defparameter *anvil-port* 8549)

(defun launch-anvil ()
  (launch-program (list "anvil" "-p" (fmt "~a" *anvil-port*))))

(defun try-launch-anvil ()
  (let* ((anvil (launch-anvil))
	 (alive? (process-alive-p anvil)))
    (terminate-process anvil :urgent t)
    alive?))

(defmacro with-anvil-provider (&body body)
  (with-gensyms (anvil)
    `(let ((,anvil (launch-anvil))
	   (*default-provider* (make-http-provider (fmt "http://127.0.0.1:~a" *anvil-port*))))
       (unwind-protect
	    (progn
	      (sleep 0.5)
	      ,@body)
	 (terminate-process ,anvil :urgent t)))))
