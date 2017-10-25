(defpackage #:primes/tests
  (:use #:common-lisp #:fiveam)
  (:export
    #:primes
    #:run-test-suite
    #:primep-too-low-error
    #:primep-too-high-error
    #:primep-floating-point-error
    #:primep-ratio-error
    #:primep-string-error
    #:primep-0-to-30
    #:prime-vector-too-low-error
    #:prime-vector-too-high-error
    #:prime-vector-floating-point-error
    #:prime-vector-ratio-error
    #:prime-vector-low-gt-high-error
    #:prime-vector-low-and-high-equal
    #:prime-vector-0-to-30
    #:prime-vector-includes-low
    #:prime-vector-includes-high
    #:prime-generator-too-low-error
    #:prime-generator-too-high-error
    #:prime-generator-floating-point-error
    #:prime-generator-ratio-error
    #:prime-generator-low-gt-high-error
    #:prime-generator-low-and-high-equal
    #:prime-generator-0-to-30
    #:prime-generator-includes-low
    #:prime-generator-includes-high
    #:prime-generator-default-args-works))
(in-package #:primes/tests)

(def-suite primes :description "Parent of all prime test suites.")

(defun run-test-suite ()
  (run! 'primes))

;;; Functions to help test PRIMEP functions.
(defun primep-too-low-error (primep-fn lowest)
  "PRIMEP must signal TYPE-ERROR if the argument is too low.
LOWEST must be the lowest possible valid value for a PRIMEP argument."
  (signals type-error (funcall primep-fn (1- lowest))))

(defun primep-too-high-error (primep-fn highest)
  "PRIMEP must signal TYPE-ERROR if the argument is too high.
HIGHEST must be the highest possible valid value for a PRIMEP argument."
  (signals type-error (funcall primep-fn (1+ highest))))

(defun primep-floating-point-error (primep-fn)
  "PRIMEP must signal TYPE-ERROR if the argument is floating point."
  (signals type-error (funcall primep-fn -1.0))
  (signals type-error (funcall primep-fn 0.0))
  (signals type-error (funcall primep-fn 1.0))
  (signals type-error (funcall primep-fn 2.0))
  (signals type-error (funcall primep-fn 3.s0))
  (signals type-error (funcall primep-fn 4.f0))
  (signals type-error (funcall primep-fn 5.d0))
  (signals type-error (funcall primep-fn 6.l0)))

(defun primep-ratio-error (primep-fn)
  "PRIMEP must signal TYPE-ERROR if the argument is a ratio."
  (signals type-error (funcall primep-fn 20/3)))

(defun primep-string-error (primep-fn)
  "PRIMEP must signal TYPE-ERROR if the argument is a string."
  (signals type-error (funcall primep-fn "11")))

(defun primep-0-to-30 (primep-fn)
  "PRIMEP must be correct for integers from 0 to 30 inclusive."
  (let ((expected '(2 3 5 7 11 13 17 19 23 29))
        (actual (loop for i from 0 to 30
                      when (funcall primep-fn i)
                      collect i)))
    (is (equal expected actual)
        "The primes from 0 to 30 should be ~A but were ~A~%" expected actual)))

;;; Functions to help test PRIME-VECTOR functions.
(defun prime-vector-too-low-error (prime-vector-fn lowest)
  "PRIME-VECTOR must signal TYPE-ERROR if any argument is too low.
LOWEST must be the lowest possible valid value for a PRIME-VECTOR argument."
  (finishes (funcall prime-vector-fn lowest (1+ lowest)))
  (signals type-error (funcall prime-vector-fn (1- lowest) lowest))
  (signals type-error (funcall prime-vector-fn (- lowest 2) (1- lowest))))

(defun prime-vector-too-high-error (prime-vector-fn highest)
  "PRIME-VECTOR must signal TYPE-ERROR if any argument is too high.
HIGHEST must be the highest possible valid value for a PRIME-VECTOR argument."
  (signals type-error (funcall prime-vector-fn highest (1+ highest)))
  (signals type-error (funcall prime-vector-fn (1+ highest) (+ highest 2))))

(defun prime-vector-floating-point-error (prime-vector-fn)
  "PRIME-VECTOR must signal TYPE-ERROR if any argument is floating point."
  (signals type-error (funcall prime-vector-fn 2 30.0))
  (signals type-error (funcall prime-vector-fn 2.0 30))
  (signals type-error (funcall prime-vector-fn 2s0 30s0))
  (signals type-error (funcall prime-vector-fn 2f0 30f0))
  (signals type-error (funcall prime-vector-fn 2d0 30d0))
  (signals type-error (funcall prime-vector-fn 2l0 30l0)))

(defun prime-vector-ratio-error (prime-vector-fn)
  "PRIME-VECTOR must signal TYPE-ERROR if any argument is a ratio."
  (signals type-error (funcall prime-vector-fn 20/3 100))
  (signals type-error (funcall prime-vector-fn 3 200/3)))

(defun prime-vector-low-gt-high-error (prime-vector-fn)
  "PRIME-VECTOR must signal ERROR if LOW is greater than HIGH."
  (signals error (funcall prime-vector-fn 30 2)))

(defun prime-vector-low-and-high-equal (prime-vector-fn)
  "PRIME-VECTOR must be finish correctly when LOW and HIGH are equal."
  (is (equalp #() (funcall prime-vector-fn 30 30)))
  (is (equalp #(29) (funcall prime-vector-fn 29 29))))

(defun prime-vector-0-to-30 (prime-vector-fn)
  "PRIME-VECTOR must be correct for 0 through 30."
  (is (equalp #(2 3 5 7 11 13 17 19 23 29) (funcall prime-vector-fn 0 30))))

(defun prime-vector-includes-low (prime-vector-fn)
  "PRIME-VECTOR must include the low value if it's a prime number."
  (is (equalp #(11 13 17 19 23 29) (funcall prime-vector-fn 11 30))))

(defun prime-vector-includes-high (prime-vector-fn)
  "PRIME-VECTOR must include the high value if it's a prime number."
  (is (equalp #(11 13 17 19 23 29) (funcall prime-vector-fn 10 29))))

;;; Functions to help test PRIME-GENERATOR functions.
(defun prime-generator-too-low-error (prime-generator-fn lowest)
  "PRIME-GENERATOR must signal TYPE-ERROR if any argument is too low.
LOWEST must be the lowest possible valid value for a PRIME-GENERATOR argument
if one exists."
  (finishes (funcall (funcall prime-generator-fn :low lowest :high lowest)))
  (signals type-error (funcall prime-generator-fn :low (1- lowest)))
  (signals type-error (funcall prime-generator-fn :high (1- lowest))))

(defun prime-generator-too-high-error (prime-generator-fn highest)
  "PRIME-GENERATOR must signal TYPE-ERROR if any argument is too high.
HIGHEST must be the highest possible valid value for a PRIME-GENERATOR argument
if one exists."
  (signals type-error (funcall prime-generator-fn :low (1+ highest)))
  (signals type-error (funcall prime-generator-fn :high (1+ highest))))

(defun prime-generator-floating-point-error (prime-generator-fn)
  "PRIME-GENERATOR must signal TYPE-ERROR if any arguments are floating point."
  (signals type-error (funcall prime-generator-fn :low 2 :high 30.0))
  (signals type-error (funcall prime-generator-fn :low 2.0 :high 30))
  (signals type-error (funcall prime-generator-fn :low 2s0 :high 30s0))
  (signals type-error (funcall prime-generator-fn :low 2f0 :high 30f0))
  (signals type-error (funcall prime-generator-fn :low 2d0 :high 30d0))
  (signals type-error (funcall prime-generator-fn :low 2l0 :high 30l0)))

(defun prime-generator-ratio-error (prime-generator-fn)
  "PRIME-GENERATOR must signal TYPE-ERROR if any argument is a ratio."
  (signals type-error (funcall prime-generator-fn :low 20/3 :high 100))
  (signals type-error (funcall prime-generator-fn :low 3 :high 200/3)))

(defun prime-generator-low-gt-high-error (prime-generator-fn)
  "PRIME-GENERATOR must signal ERROR if LOW is greater than HIGH."
  (signals error (funcall prime-generator-fn :low 30 :high 2)))

(defun prime-list (prime-generator-fn low high)
  "Return a list of primes using PRIME-GENERATOR-FN from LOW to HIGH."
  (loop with gen = (funcall prime-generator-fn :low low :high high)
        for prime = (funcall gen)
        while prime
        collect prime))

(defun prime-generator-low-and-high-equal (prime-generator-fn)
  "PRIME-GENERATOR must be correct when LOW and HIGH are equal."
  (is (equal () (prime-list prime-generator-fn 30 30)))
  (is (equal '(29) (prime-list prime-generator-fn 29 29))))

(defun prime-generator-0-to-30 (prime-generator-fn)
  "PRIME-GENERATOR must be correct for 0 through 30."
  (is (equal '(2 3 5 7 11 13 17 19 23 29)
             (prime-list prime-generator-fn 0 30))))

(defun prime-generator-includes-low (prime-generator-fn)
  "PRIME-GENERATOR must include the low value if it's a prime number."
  (is (equal '(11 13 17 19 23 29) (prime-list prime-generator-fn 11 30))))

(defun prime-generator-includes-high (prime-generator-fn)
  "PRIME-GENERATOR must include the high value if it's a prime number."
  (is (equal '(11 13 17 19 23 29) (prime-list prime-generator-fn 10 29))))

(defun prime-generator-default-args-works (prime-generator-fn)
  "PRIME-GENERATOR works when called without any arguments."
  (finishes (funcall (funcall prime-generator-fn))))
