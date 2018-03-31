# Programming Techniques: Generating Prime Numbers in Common Lisp
I've been working on [Project Euler](https://projecteuler.net/) problems using Common Lisp as my programming language.  To solve some of them, it's useful to have a fast way to go through increasing prime numbers (2, 3, 5, 7, 11, ...).  I made do with trial division until I tried [Problem 263](https://projecteuler.net/problem=263) where I needed to go past one billion.

I want these three functions:
1. primep (n) - return t if n is a prime number, nil otherwise.
2. prime-vector (low high) - return a vector containing the prime numbers between low and high inclusive.
3. prime-generator (&key low high) - return a closure that returns each prime number when called, from low to high inclusive.  This function should also have suitable default values for low and high.

In programming, I'm used to having ranges exclude the high value, but sometimes an inclusive range is useful - for example, if you want to know all the primes in the range of 64-bit unsigned integers, you can just set the high value to the maximum possible 64-bit unsigned value instead of thinking of ways to pass in something that is one higher than that maximum.

I went through the following steps that I will describe in the rest of this document:
1. Try the simplest thing that works: trial division.
2. Look for a library that does what we want: cl-prime-maker.
3. Find an algorithm online: the sieve of Eratosthenes.
4. Find a code example, even in another language, and adapt it: [segmented sieve](https://primesieve.org/segmented_sieve.html).
5. Call an executable that does what we want, using `uiop:run-program`.
6. Call a C library that does what we want, using [CFFI](https://common-lisp.net/project/cffi/).

I thought writing about my investigation could be helpful to other programmers as an example of some Common Lisp code.  The timings are based on my laptop (with an Intel Core i5-5200U @ 2.2GHz) running LispWorks 7 64-bit on Windows 10.

## Step 1: Try the simplest thing that works: trial division
A prime number is a positive integer greater than one that has no positive divisors other than 1 and the number itself.  We can translate this definition almost directly into Common Lisp, using the technique that we only need to check for divisors up to the square root of n:

```lisp
(defun primep (n)
  "Return T if N is a prime number, NIL otherwise."
  (and (integerp n)
       (> n 1)
       (loop for i from 2 to (isqrt n)
             never (zerop (rem n i)))))
```

This function correctly reports that negative numbers like -1 aren't prime.  Common Lisp supports numbers that are ratios of two integers, like 20/3.  20/3 is reported by `primep` as not prime either - it's not an integer.  But 6/3 is prime according to `primep` because the canonical representation of 6/3 is the integer 2.  One possibly unexpected result is that the floating-point number 3.0 is not prime according to `primep`, because it's a floating-point number and not an integer, so it fails the `integerp` test.  If we want to make `primep` work for floating-point numbers that are equal in value to prime numbers, we can modify the code:

```lisp
(defun primep (n)
  "Return T if N is a prime number, NIL otherwise."
  (and (or (integerp n)
           (and (floatp n) (integerp (rational n))))
       (> n 1)
       (loop for i from 2 to (isqrt (rational n))
             never (zerop (rem n i)))))
```

Maybe this is a slippery slope - what if someone wanted us to make the string "3" be prime too?  Perhaps we should be strict about the input we allow and say `primep` will signal an error if N isn't a non-negative integer.  It might seem too restrictive to say `(primep -3)` signals an error instead of just saying it's not prime, but being strict can help us find bugs.

```lisp
(defun primep (n)
  "Return T if N is a prime number, NIL otherwise.
A TYPE-ERROR will be signaled if N is not a non-negative integer."
  (check-type n (integer 0 *) "a non-negative integer")
  (and (> n 1)
       (loop for i from 2 to (isqrt n)
             never (zerop (rem n i)))))
```

Now, passing in 3.0, "3", or -3 will signal an error.  Given this definition of `primep`, we can implement the two other functions:

```lisp
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
```

This code is slow - using `prime-generator` to iterate through primes up to 10 million takes 43 seconds on my laptop.  100 million takes 17 minutes and 35 seconds.

## Step 2: Look for a library that does what we want: cl-prime-maker
Here's one of the ways to search quicklisp for libraries:

```
CL-USER 2 > (ql:system-apropos "prime")
#<QL-DIST:SYSTEM cl-prime-maker / cl-prime-maker-20150302-git / quicklisp 2017-10-23>
```

We can run `(ql:quickload "cl-prime-maker")` to download and use it, and try out some of its functionality:

```
CL-USER 4 > (cl-prime-maker:primep 29)
T

CL-USER 5 > (cl-prime-maker:get-nth-prime 3)
5
```

We can implement `prime-vector` and `prime-generator` using the same code as trial division while replacing our old `primep` with `cl-prime-maker:primep`, or we can use `cl-prime-maker:get-nth-prime` to find the starting point and continue calling it until we reach the high value.  See [prime-maker.lisp](src/prime-maker.lisp).

Getting the primes up to 100,000 swapping out the `primep` implementation takes 19 seconds.  Getting the primes up to 100,000 using `cl-prime-maker:get-nth-prime` takes 3 minutes and 12 seconds.

The documentation states that there's a small chance of incorrectly identifying a number as prime.  `cl-prime-maker:primep` says that 75,361 is prime but it's not.  However, `cl-prime-maker:get-nth-prime` correctly skips over it.

## Step 3: Find an algorithm online: the sieve of Eratosthenes
I was aware of the sieve of Eratosthenes before starting my investigation, but not about any of the ways to speed it up.

See [sieve.lisp](src/sieve.lisp) for the implementation.

My implementation is based on a bit vector - a one-dimensional array where each value is only a 0 or 1.  If the Nth bit in the bit vector is 1, then N is a prime number.  There's a limit to how many elements an array can hold, which is why there's a `deftype` to define the range of numbers the sieve can support (starting from 0 up to and including `array-dimension-limit` - 2.  My understanding of this is that `array-dimension-limit` is an exclusive upper bound on the size of an array dimension, meaning I can create an array up to the size `array-dimension-limit` - 1.  But that means the array indexes into the maximum size array are from 0 to `array-dimension-limit` - 2.

On my laptop, running `prime-vector` to get all the primes up to 500 million takes about 11 seconds - not bad.  But `array-dimension-limit` is 536,870,911 in 64-bit LispWorks, so we can't use this code directly to find primes past this limit.  Other Common Lisp implementations like SBCL have a different limit (SBCL's is much higher).

There are two workarounds I tried, but eventually abandoned:
1. Instead of a bit vector, use a bignum - a large integer.  The functions `dpb` and `logbitp` treat integers as though they are binary.  `dpb` can set a bit in the integer, and `logbitp` can read a bit from the integer.  So when N is prime, we set the Nth bit of the integer to 1.  The performance was too slow unfortunately, because `dpb` is a non-destructive function - it creates a new bignum every time we just want to change a bit.
2. Instead of a single bit vector, use an array of bit vectors, where the next bit vector continues where the last one ended.  This is only a few extra lines of code to implement, and lets me get more, but still not the entire 64-bit range.  The segmented sieve described below is like this but far better with the insight that you just do one range at a time instead of holding onto every bit vector.

## Step 4: Find a code example, even in another language, and adapt it: segmented sieve
Here's a great example of a segmented sieve written in C++: [segmented sieve](https://primesieve.org/segmented_sieve.html).

I translated this into Common Lisp line-by-line and it took about 14 seconds to count all the prime numbers up to 500 million, and 35 seconds to count all the prime numbers up to 1.2 billion.  It's slower than the C++ code, although I didn't turn on any optimizations yet.  It doesn't have the `array-dimension-limit` limitation because it's segmented and doesn't need to hold all the primes in one array.

I was going to study the code and see how to optimize it, but I didn't need to because this code was on the excellent [primesieve](https://primesieve.org) website which has binaries and source code which can be compiled into libraries to use, which led me to the next step.

## Step 5: Call an executable that does what we want, using UIOP:RUN-PROGRAM.
The primesieve website includes downloadable GUI and command-line executables which implement a very fast sieve implementation written in C++ that can generate primes up to 2^64 - 1.

The easiest way to take advantage of this from Common Lisp is to write code to run the command-line executable and capture its output.  This is a technique I've used many times, not just in shell scripts or batch files, but in a variety of programming languages as well.

```lisp
(defun primesieve-list (low high)
  "Return a list of the prime numbers in the interval [LOW, HIGH].
This runs the command-line primesieve executable and gathers the results from
standard output."
  (uiop:run-program (list "primesieve"
                          (write-to-string low)
                          (write-to-string high)
                          "-p")
                    :output :forms))
```

If you run `(primesieve-list 20 50)`, it's equivalent to running `primesieve 20 50 -p` on the command line and gathering the output which is each prime number between 20 and 50 inclusive.

This takes about 22 seconds to get all the primes up to 500 million, and 52 seconds to get the primes up to 1.2 billion.  A significant amount of time is spent writing the output of the executable to a temporary file and reading the file back into the Common Lisp environment.  Just running the command-line executable at the command prompt to count the primes up to 1.2 billion takes less than a second.

## Step 6: Call a C library that does what we want, using CFFI
I'm currently using primesieve 6.2.  Running `(ql:quickload "cffi")` lets us use CFFI in our code, [primesieve-cffi.lisp](src/primesieve-cffi.lisp).

The code does the following things:
1. Load the primesieve library (primesieve.dll in Windows).
2. Create declarations for the C enums and functions in primesieve that we'll use.  The main one is `primesieve_generate_primes`, which gives us a C array containing the prime numbers between `start` and `stop`.
3. The function `primesieve-primes` calls the C function `primesieve_generate_primes` and takes care of allocating and freeing memory for it, but to use it you must also pass in a function, `foreign-primes->list` or `foreign-primes->vector`, to return the resulting primes in a Common Lisp list or vector.
4. Using `primesieve-primes`, it's trivial to implement `primep`, `prime-vector`, and `prime-generator`.  `prime-generator` caches a range of 10 million at a time.

This is much faster than any of the other implementations: it takes about 1 second to get all the primes up to 500 million, and 2.3 seconds to get all the primes up to 1.2 billion.

## Summary
We started with a program that took over 17 minutes to get the prime numbers up to 100 million and ended with one that took about 2 seconds to get the prime numbers up to 1.2 billion, thanks to [primesieve](https://primesieve.org).

Along the way, we covered several programming techniques while writing Common Lisp code, which I hope will be a good example if you haven't seen these before:
1. A few numeric functions like integerp, floatp, rational, isqrt, and rem.
2. A use of Common Lisp's loop macro with the "never" clause to check that an expression is never true.
3. Assertions using check-type and assert, and using deftype to define a type that covers the range of integers from 0 to array-dimension-limit - 2.
4. Loading libraries using Quicklisp.
5. Bit vectors, and using integers this way using dbp and logbitp.
6. Using uiop:run-program to run a command line executable and capture its output.
7. Using CFFI to load a C library and call its functions.

