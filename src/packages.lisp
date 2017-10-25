(defpackage #:trial-division
  (:use #:common-lisp)
  (:export
    #:primep
    #:prime-vector
    #:prime-generator))

(defpackage #:sieve
  (:use #:common-lisp)
  (:export
   #:primep
   #:prime-vector
   #:prime-generator))

(defpackage #:primesieve-cmd
  (:use #:common-lisp)
  (:export
   #:primep
   #:prime-vector
   #:prime-generator))

(defpackage #:primesieve-cffi
  (:use #:common-lisp)
  (:export
   #:primep
   #:prime-vector
   #:prime-generator))

