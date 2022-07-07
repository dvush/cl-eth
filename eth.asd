(defsystem "eth"
  :description "Library for interactions with Ethereum and other compatible chains."
  :author "Vitaly Drogan <vitaly@dvush.net>"
  :license  "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :pathname "code/"
  :depends-on ("eth/all"))

(defsystem "eth/utils" :depends-on ("eth/utils/all"))
(defsystem "eth/abi" :depends-on ("eth/abi/all"))
(defsystem "eth/provider" :depends-on ("eth/provider/all"))

(defsystem "eth/tests" :depends-on ("eth/tests/all"))
