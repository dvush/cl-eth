(uiop:define-package #:eth/tests/all
  (:use #:cl)
  (:use-reexport #:eth/tests/utils/all #:eth/tests/abi #:eth/tests/provider #:eth/tests/signer)
  (:import-from #:serapeum
		#:package-exports)
  (:import-from #:parachute
		#:test
		#:plain
		#:find-test)
  (:export #:test-all
	   #:ci-test-all))
(in-package #:eth/tests/all)

(defun test-all (&key (report 'plain))
  (test (remove-if-not #'find-test (package-exports :eth/tests/all)) :report report))

(defun ci-test-all ()
  (let ((report (test-all)))
    (unless (eql (parachute:status report) :passed)
      (error "Tests failure."))))
