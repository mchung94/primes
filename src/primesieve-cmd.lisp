(in-package #:primesieve-cmd)

(defun primesieve-list (low high)
  "Return a list of the prime numbers in the interval [LOW, HIGH].
This runs the command-line primesieve executable and gathers the results from
standard output."
  (uiop:run-program (list "primesieve"
                          (write-to-string low)
                          (write-to-string high)
                          "-p")
                    :output :forms))

(defun primep (n)
  "Return T if N is a prime number, NIL otherwise.
A TYPE-ERROR will be signaled if N is not in the unsigned 64 bit range."
  (check-type n (unsigned-byte 64))
  (not (null (primesieve-list n n))))

(defun prime-vector (low high)
  "Return a vector containing the prime numbers in the interval [LOW, HIGH].
A TYPE-ERROR will be signaled if LOW or HIGH are not in the unsigned 64 bit
range.  An ERROR is signaled if LOW is greater than HIGH."
  (check-type low (unsigned-byte 64))
  (check-type high (unsigned-byte 64))
  (assert (<= low high) (low high))
  (coerce (primesieve-list low high) 'vector))

(defun inclusive-ranges (low high range-size)
  "Return a closure that returns inclusive start/end ranges covering LOW to HIGH
inclusive, with each range's size being at most RANGE-SIZE."
  (lambda ()
    (when (<= low high)
      (values low (min (1- (incf low range-size)) high)))))

(defun prime-generator (&key (low 0) (high (1- (expt 2 64))))
  "Return a closure that returns each prime number from LOW to HIGH inclusive.
The closure will return NIL when it has passed HIGH.  A TYPE-ERROR will be
signaled if LOW or HIGH are not in the unsigned 64 bit range.  An error is
signaled if if LOW is greater than HIGH."
  (check-type low (unsigned-byte 64))
  (check-type high (unsigned-byte 64))
  (assert (<= low high) (low high))
  (let ((range-gen (inclusive-ranges low high 10000000))
        (primes ()))
    (lambda ()
      (loop while (and (null primes)
                       (multiple-value-setq (low high) (funcall range-gen)))
            do (setf primes (primesieve-list low high)))
      (prog1 (first primes)
        (setf primes (rest primes))))))
