(defsystem "primes"
  :depends-on ("cl-prime-maker" "cffi")
  :pathname "src/"
  :components
  ((:file "packages")
   (:file "trial-division" :depends-on ("packages"))
   (:file "prime-maker" :depends-on ("packages"))
   (:file "sieve" :depends-on ("packages"))
   (:file "primesieve-cmd" :depends-on ("packages"))
   (:file "primesieve-cffi" :depends-on ("packages")))
  :in-order-to ((test-op (test-op "primes/tests"))))

(defsystem "primes/tests"
  :depends-on ("fiveam" "primes")
  :pathname "tests/"
  :components
  ((:file "test-suite")
   (:file "trial-division-tests" :depends-on ("test-suite"))
   (:file "prime-maker-tests" :depends-on ("test-suite"))
   (:file "sieve-tests" :depends-on ("test-suite"))
   (:file "primesieve-cmd-tests" :depends-on ("test-suite"))
   (:file "primesieve-cffi-tests" :depends-on ("test-suite")))
  :perform (test-op (o c) (symbol-call :primes/tests :run-test-suite)))


