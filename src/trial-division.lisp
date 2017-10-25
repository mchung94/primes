(in-package #:trial-division)

(defun primep (n)
  "Return T if N is a prime number, NIL otherwise.
A TYPE-ERROR will be signaled if N is not a non-negative integer."
  (check-type n (integer 0 *) "a non-negative integer")
  (and (> n 1)
       (loop for i from 2 to (isqrt n)
             never (zerop (rem n i)))))

(defun prime-vector (low high)
  "Return a vector containing the prime numbers between LOW and HIGH inclusive.
A TYPE-ERROR will be signaled if LOW or HIGH are not non-negative integers.
An ERROR will be signaled if LOW is greater than HIGH."
  (check-type low (integer 0 *) "a non-negative integer")
  (check-type high (integer 0 *) "a non-negative integer")
  (assert (<= low high) (low high))
  (coerce (loop for i from low to high
                when (primep i)
                collect i)
          'vector))

(defun prime-generator (&key (low 0) (high nil))
  "Return a closure that returns each prime number from LOW to HIGH inclusive.
The closure will return NIL when it has passed HIGH, but if HIGH is NIL then
there will be no pre-set upper limit and will not stop returning more primes.
A TYPE-ERROR will be signaled if LOW is not a non-negative integer.
A TYPE-ERROR will be signaled if HIGH is not NIL or a non-negative integer.
An ERROR will be signaled if LOW is greater than HIGH."
  (check-type low (integer 0 *) "a non-negative integer")
  (check-type high (or null (integer 0 *)) "a non-negative integer or NIL")
  (assert (or (not high) (<= low high)) (low high))
  (let ((current-prime (1- low)))
    (lambda ()
      (when current-prime
        (loop for i from (1+ current-prime)
              when (and high (> i high))
              return (setf current-prime nil)
              when (primep i)
              return (setf current-prime i))))))
