(in-package #:sieve)

(deftype sieve-value ()
  "The range of integers that the Sieve of Eratosthenes can support."
  `(integer 0 ,(- array-dimension-limit 2)))

(defun sieve-of-eratosthenes (limit)
  "Return a bit vector of length LIMIT where the Nth bit is 1 if N is prime."
  (declare (optimize speed)
           (fixnum limit))
  (case limit
    (1 #*0) ; a bit vector indicating 0 isn't a prime number
    (2 #*00) ; a bit vector indicating 0 and 1 aren't prime numbers
    (otherwise
     (let ((sieve (make-array limit :element-type 'bit :initial-element 1)))
       (flet ((set-not-prime (index)
                "Mark INDEX as not prime in SIEVE."
                (declare (type sieve-value index))
                (setf (sbit sieve index) 0)))
         (declare (inline set-not-prime))
         ;; 0 and 1 aren't prime
         (set-not-prime 0)
         (set-not-prime 1)
         ;; optimization: sieve evens first, so the next loop does less work
         (loop for i of-type sieve-value from 4 below limit by 2
               do (set-not-prime i))
         (loop with i-limit of-type sieve-value = (isqrt limit)
               for i of-type sieve-value from 3 to i-limit by 2
               when (eql 1 (sbit sieve i))
               do (loop with j-start of-type sieve-value = (* i i)
                        with j-step of-type sieve-value = (* i 2)
                        for j of-type sieve-value from j-start below limit by j-step
                        do (set-not-prime j)))
         sieve)))))

(defun primep (n)
  "Return T if N is a prime number, NIL otherwise.
A TYPE-ERROR will be signaled if N is not of type SIEVE-VALUE."
  (check-type n sieve-value)
  (eql 1 (sbit (sieve-of-eratosthenes (1+ n)) n)))

(defun prime-vector (low high)
  "Return a vector containing the prime numbers between LOW and HIGH inclusive.
A TYPE-ERROR will be signaled if LOW or HIGH are not of type SIEVE-VALUE.
An ERROR will be signaled if LOW is greater than HIGH."
  (check-type low sieve-value)
  (check-type high sieve-value)
  (assert (<= low high) (low high))
  (coerce (loop with sieve = (sieve-of-eratosthenes (1+ high))
                for i from low to high
                when (eql 1 (sbit sieve i))
                collect i)
          'vector))

(defun prime-generator (&key (low 0) (high (- array-dimension-limit 2)))
  "Return a closure that returns each prime number from LOW to HIGH inclusive.
The closure will return NIL when it has passed HIGH.
A TYPE-ERROR will be signaled if LOW or HIGH are not of type SIEVE-VALUE.
An ERROR will be signaled if LOW is greater than HIGH."
  (check-type low sieve-value)
  (check-type high sieve-value)
  (assert (<= low high) (low high))
  (let ((sieve (sieve-of-eratosthenes (1+ high)))
        (sieve-index (1- low)))
    (lambda ()
      (when (<= sieve-index high)
        (loop
         (incf sieve-index)
         (when (> sieve-index high)
           (return nil))
         (when (eql 1 (sbit sieve sieve-index))
           (return sieve-index)))))))
