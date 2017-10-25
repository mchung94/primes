(defpackage #:primesieve-cffi-tests
  (:use #:common-lisp #:fiveam #:primesieve-cffi))
(in-package #:primesieve-cffi-tests)

(def-suite primesieve-cffi :in primes/tests:primes)
(in-suite primesieve-cffi)

(defconstant +lowest+ 0)
(defconstant +highest+ (1- (expt 2 64)))

;; PRIMEP tests
(test primep-too-low-signals-type-error
  (primes/tests:primep-too-low-error #'primep +lowest+))

(test primep-too-high-signals-type-error
  (primes/tests:primep-too-high-error #'primep +highest+))

(test primep-floating-point-signals-type-error
  (primes/tests:primep-floating-point-error #'primep))

(test primep-ratio-signals-type-error
  (primes/tests:primep-ratio-error #'primep))

(test primep-string-signals-type-error
  (primes/tests:primep-string-error #'primep))

(test primep-0-to-30
  (primes/tests:primep-0-to-30 #'primep))

;; PRIME-VECTOR tests
(test prime-vector-too-low-signals-type-error
  (primes/tests:prime-vector-too-low-error #'prime-vector +lowest+))

(test prime-vector-too-high-signals-type-error
  (primes/tests:prime-vector-too-high-error #'prime-vector +highest+))

(test prime-vector-floating-point-signals-type-error
  (primes/tests:prime-vector-floating-point-error #'prime-vector))

(test prime-vector-ratio-signals-type-error
  (primes/tests:prime-vector-ratio-error #'prime-vector))

(test prime-vector-low-gt-high-signals-error
  (primes/tests:prime-vector-low-gt-high-error #'prime-vector))

(test prime-vector-low-and-high-equal-works-correctly
  (primes/tests:prime-vector-low-and-high-equal #'prime-vector))

(test prime-vector-0-to-30
  (primes/tests:prime-vector-0-to-30 #'prime-vector))

(test prime-vector-includes-low-value
  (primes/tests:prime-vector-includes-low #'prime-vector))

(test prime-vector-includes-high-value
  (primes/tests:prime-vector-includes-high #'prime-vector))

;; PRIME-GENERATOR tests
(test prime-generator-too-low-signals-type-error
  (primes/tests:prime-generator-too-low-error #'prime-generator +lowest+))

(test prime-generator-too-high-signals-type-error
  (primes/tests:prime-generator-too-high-error #'prime-generator +highest+))

(test prime-generator-floating-point-signals-type-error
  (primes/tests:prime-generator-floating-point-error #'prime-generator))

(test prime-generator-ratio-signals-type-error
  (primes/tests:prime-generator-ratio-error #'prime-generator))

(test prime-generator-low-gt-high-signals-error
  (primes/tests:prime-generator-low-gt-high-error #'prime-generator))

(test prime-generator-low-and-high-equal-works-correctly
  (primes/tests:prime-generator-low-and-high-equal #'prime-generator))

(test prime-generator-high-nil-signals-error
  (signals error (prime-generator :low 0 :high nil)))

(test prime-generator-0-to-30
  (primes/tests:prime-generator-0-to-30 #'prime-generator))

(test prime-generator-includes-low-value
  (primes/tests:prime-generator-includes-low #'prime-generator))

(test prime-generator-includes-high-value
  (primes/tests:prime-generator-includes-high #'prime-generator))

;;; This test takes too long - it supports the entire unsigned 64 bit range
;(test prime-generator-default-args-works
;  (primes/tests:prime-generator-default-args-works #'prime-generator))

(test prime-generator-range-test
  (is (= 970704 (loop with gen = (prime-generator :low 0 :high 15000000)
                      for prime = (funcall gen)
                      while prime
                      count prime))))
