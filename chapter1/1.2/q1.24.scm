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

(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (remainder
                (square (expmod base (/ exp 2) m))
            m))
        (else
            (remainder
                (* base (expmod base (- exp 1) m))
                m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (fast-prime? n 3)
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
; the output is significantly lesser than 1.22, so small that numbers are not getting printed on my machine
(display (search-for-primes (expt 10 9) 3))
(display (search-for-primes (expt 10 10) 3))
(display (search-for-primes (expt 10 11) 3))