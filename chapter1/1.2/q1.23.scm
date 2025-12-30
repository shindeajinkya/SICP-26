(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))

(define (even? n)
    (= (remainder n 2) 0))

;(define (next num) (if (even? num) (+ num 1) (+ num 2)))
(define (next num) (if (= num 2) 3 (+ num 2)))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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
; the output for these does halve the growth than previous 1.22 exercise
(display (search-for-primes (expt 10 9) 3))
(display (search-for-primes (expt 10 10) 3))
(display (search-for-primes (expt 10 11) 3))

; output with even? check next
; 1000000007 *** 9.999999999999995e-3
; 1000000009 *** 1.0000000000000009e-2
; 1000000021 *** 9.999999999999995e-3
; #!unspecific
; 10000000019 *** .03
; 10000000033 *** .03
; 10000000061 *** .02999999999999997
; #!unspecific
; 100000000003 *** .07999999999999996
; 100000000019 *** .08000000000000007
; 100000000057 *** .08999999999999997
; #!unspecific%         


;output with = 2 next
; 1000000007 *** 9.999999999999995e-3
; 1000000009 *** 1.0000000000000009e-2
; 1000000021 *** 0.
; #!unspecific
; 10000000019 *** 2.0000000000000018e-2
; 10000000033 *** .01999999999999999
; 10000000061 *** 3.0000000000000027e-2
; #!unspecific
; 100000000003 *** .06999999999999995
; 100000000019 *** .07000000000000006
; 100000000057 *** .06999999999999995