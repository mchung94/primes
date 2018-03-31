(defpackage #:prime-maker-tests
  (:use #:common-lisp #:fiveam #:prime-maker))
(in-package #:prime-maker-tests)

(def-suite prime-maker :in primes/tests:primes)
(in-suite prime-maker)

;; PRIMEP tests
(test primep-too-low-signals-type-error
  (primes/tests:primep-too-low-error #'primep 0))

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
  (primes/tests:prime-vector-too-low-error #'prime-vector 0))

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
  (primes/tests:prime-generator-too-low-error #'prime-generator 0))

(test prime-generator-floating-point-signals-type-error
  (primes/tests:prime-generator-floating-point-error #'prime-generator))

(test prime-generator-ratio-signals-type-error
  (primes/tests:prime-generator-ratio-error #'prime-generator))

(test prime-generator-low-gt-high-signals-error
  (primes/tests:prime-generator-low-gt-high-error #'prime-generator))

(test prime-generator-low-and-high-equal-works-correctly
  (primes/tests:prime-generator-low-and-high-equal #'prime-generator))

(test prime-generator-high-nil-works-correctly
  (is (equal '(23 29)
             (loop with gen = (prime-generator :low 20 :high nil)
                   for prime = (funcall gen)
                   while (< prime 30)
                   collect prime))))

(test prime-generator-0-to-30
  (primes/tests:prime-generator-0-to-30 #'prime-generator))

(test prime-generator-includes-low-value
  (primes/tests:prime-generator-includes-low #'prime-generator))

(test prime-generator-includes-high-value
  (primes/tests:prime-generator-includes-high #'prime-generator))

(test prime-generator-default-args-works
  (primes/tests:prime-generator-default-args-works #'prime-generator))
