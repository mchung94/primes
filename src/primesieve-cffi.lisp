(in-package #:primesieve-cffi)

;; Primesieve 6.2 64-bit
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library primesieve
    (:unix (:or "libprimesieve.so.8" "libprimesieve.so"))
    (:windows "primesieve.dll")
    (t (:default "libprimesieve")))
  (cffi:use-foreign-library primesieve))

(cffi:defcenum prime-type
  "The type of primes to generate.  Used by PRIMESIEVE-GENERATE-PRIMES."
  :short-primes     ; generate primes of short type
  :ushort-primes    ; generate primes of unsigned short type
  :int-primes       ; generate primes of int type
  :uint-primes      ; generate primes of unsigned int type
  :long-primes      ; generate primes of long type
  :ulong-primes     ; generate primes of unsigned long type
  :longlong-primes  ; generate primes of long long type
  :ulonglong-primes ; generate primes of unsigned long long type
  :int16-primes     ; generate primes of int16_t type
  :uint16-primes    ; generate primes of uint16_t type
  :int32-primes     ; generate primes of int32_t type
  :uint32-primes    ; generate primes of uint32_t type
  :int64-primes     ; generate primes of int64_t type
  :uint64-primes)   ; generate primes of uint64_t type

(cffi:defcfun ("primesieve_generate_primes" :library primesieve) :pointer
  "Get an array with the primes inside the interval [START, STOP].
SIZE is the size of the returned primes array.
TYPE is the type of the primes to generate, e.g. INT_PRIMES."
  (start :uint64)
  (stop :uint64)
  (size (:pointer :uint64))
  (type prime-type))

(cffi:defcfun ("primesieve_get_max_stop" :library primesieve) :uint64
  "Returns the largest valid stop number for primesieve: (1- (EXPT 2 64)).")

(cffi:defcfun ("primesieve_free" :library primesieve) :void
  "Deallocate a primes array created using the primesieve_generate_primes()
or primesieve_generate_n_primes() functions.  PRIMES is a void*."
  (primes :pointer))

(cffi:defcfun ("primesieve_version" :library primesieve) :string
  "Get the primesieve version number as a string in the form i.j.")

(defun foreign-primes->list (foreign-array foreign-size)
  "Convert FOREIGN-ARRAY containing FOREIGN-SIZE primes into a list."
  (do ((primes ())
       (i (1- (cffi:mem-ref foreign-size :uint64)) (1- i)))
      ((< i 0) primes)
    (push (cffi:mem-aref foreign-array :uint64 i) primes)))

(defun foreign-primes->vector (foreign-array foreign-size)
  "Convert FOREIGN-ARRAY containing FOREIGN-SIZE primes into a simple vector."
  (let* ((size (cffi:mem-ref foreign-size :uint64))
         (primes (make-array size)))
    (dotimes (i size primes)
      (setf (svref primes i) (cffi:mem-aref foreign-array :uint64 i)))))

(defun primesieve-primes (low high conversion-function)
  "Return a list or vector of primes between LOW and HIGH inclusive.
PRIMESIEVE-GENERATE-PRIMES uses LOW and HIGH and returns a foreign array of
prime numbers and size, then the CONVERSION-FUNCTION converts it into a Common
Lisp sequence - use either #'FOREIGN-PRIMES->LIST or #'FOREIGN-PRIMES->VECTOR."
  (let* ((foreign-size (cffi:foreign-alloc :uint64))
         (foreign-primes
          (primesieve-generate-primes low high foreign-size :uint64-primes)))
    (unwind-protect
        (funcall conversion-function foreign-primes foreign-size)
      (primesieve-free foreign-primes)
      (cffi:foreign-free foreign-size))))

(defun primep (n)
  "Return T if N is a prime number, NIL otherwise.
A TYPE-ERROR will be signaled if N is not in the unsigned 64 bit range."
  (check-type n (unsigned-byte 64))
  (not (null (primesieve-primes n n #'foreign-primes->list))))

(defun prime-vector (low high)
  "Return a vector containing the prime numbers in the interval [LOW, HIGH].
A TYPE-ERROR will be signaled if LOW or HIGH are not in the unsigned 64 bit
range.  An ERROR is signaled if LOW is greater than HIGH."
  (check-type low (unsigned-byte 64))
  (check-type high (unsigned-byte 64))
  (assert (<= low high) (low high))
  (primesieve-primes low high #'foreign-primes->vector))

(defun prime-generator (&key (low 0) (high (1- (expt 2 64))))
  "Return a closure that returns each prime number from LOW to HIGH inclusive.
The closure will return NIL when it has passed HIGH.  A TYPE-ERROR will be
signaled if LOW or HIGH are not in the unsigned 64 bit range.  An error is
signaled if if LOW is greater than HIGH."
  (check-type low (unsigned-byte 64))
  (check-type high (unsigned-byte 64))
  (assert (<= low high) (low high))
  (flet ((inclusive-ranges (start end range-size)
           (lambda ()
             (when (<= start end)
               (values start (min (1- (incf start range-size)) end))))))
    (let ((range-gen (inclusive-ranges low high 10000000))
          (primes ()))
      (lambda ()
        (loop while (and (null primes)
                         (multiple-value-setq (low high) (funcall range-gen)))
              do (setf primes
                       (primesieve-primes low high #'foreign-primes->list)))
        (prog1 (first primes)
          (setf primes (rest primes)))))))
