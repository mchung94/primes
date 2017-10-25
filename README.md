# primes
This code shows a few methods of getting prime numbers using Common Lisp.  I started investigating this in order to solve some [Project Euler](https://projecteuler.net/) problems.

This is experimental code to look into the following ways to implement three functions:
1. PRIMEP - check if a number is a prime number or not.
2. PRIME-VECTOR - return a vector of prime numbers in a range (between low/high values).
3. PRIME-GENERATOR - return a closure that returns each prime number in a range.

The code implements these using the following approaches, with unit tests:
1. Using Trial Division for non-negative integers (no upper limit other than time and resources).
2. Using a Sieve of Eratosthenes for non-negative integers up to `(- ARRAY-DIMENSION-LIMIT 2)`.
3. Calling the [primesieve](http://primesieve.org/) 6.2 executable using `UIOP:RUN-PROGRAM` for any number in the unsigned 64-bit range.
4. Calling the [primesieve](http://primesieve.org/) 6.2 library using CFFI for any number in the unsigned 64-bit range.  This is the fastest out of the four.

The Sieve of Eratosthenes may have a small limit depending on your Common Lisp implementation, due to the limit in the size of a vector you can create.  I was using one where `ARRAY-DIMENSION-LIMIT` is 536,870,911 which means I can only have a sieve that checks if 0 through 536,870,909 were prime or not.  I investigated and abandoned the following workarounds before deciding to use primesieve:
1. Implementing a sieve using the `DPB` and `LOGBITP` functions on bignums.  This is like implementing a bit vector using an integer.  This got really slow because `DPB` is a non-destructive function - it creates a new bignum every time you want to change a bit.
2. Creating an array of bit vectors and using it like one giant bit vector.  It increased the range of values I could sieve, but not enough for some of the Project Euler problems.  I could continue by creating arrays of arrays of bit vectors but it was starting to use a lot of memory.
3. Adapting the [segmented sieve C++ code](http://primesieve.org/segmented_sieve.html) from the primesieve web site.  I translated this into Common Lisp, but decided to just use primesieve directly because it saves time and it's fast.

I hope this code can be useful to you as an example.  Please let me know if you find any bugs.
