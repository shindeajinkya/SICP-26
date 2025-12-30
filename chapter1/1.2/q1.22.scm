(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))

(define (even? n)
    (= (remainder n 2) 0))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-for-primes start n)

    (cond 
        ((= n 0) (newline))
        ((even? start) (search-for-primes (+ start 1) n))
        ((prime? start) 
            (timed-prime-test start) 
            (search-for-primes (+ start 1) (- n 1))
        )
        (else (search-for-primes (+ start 1) n))
    ))


; computing 10^9, 10^10 and 10^11 since 1000, 10000 and 100000 are getting computed very fast and modern machines show 0 milliseconds, 
; the output for these does match the root 10 order of growth
(display (search-for-primes (expt 10 9) 3))
(display (search-for-primes (expt 10 10) 3))
(display (search-for-primes (expt 10 11) 3))

; 1000000007 *** .01999999999999999
; 1000000009 *** 9.999999999999995e-3
; 1000000021 *** 2.0000000000000018e-2
; #!unspecific
; 10000000019 *** .04000000000000001
; 10000000033 *** .03999999999999998
; 
; 10000000061 *** .02999999999999997
; #!unspecific
; 100000000003 *** .09999999999999998
; 100000000019 *** .10999999999999999
; 100000000057 *** .1100000000000001
; #!unspecific%