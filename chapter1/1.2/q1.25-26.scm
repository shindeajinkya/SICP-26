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

(define (fast-expt b n)
    (cond 
        ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;(define (expmod base exp m)
;    (cond ((= exp 0) 1)
;        ((even? exp)
;            (remainder
;                (square (expmod base (/ exp 2) m))
;            m))
;        (else
;            (remainder
;                (* base (expmod base (- exp 1) m))
;                m))))
; (expmod 2 10 7)

(define (expmod-debug base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (let ((result (remainder (square (expmod-debug base (/ exp 2) m)) m)))
                (display "squaring, result: ") (display result) (newline)
                result))
        (else
            (let ((result (remainder (* base (expmod-debug base (- exp 1) m)) m)))
                (display "multiplying, result: ") (display result) (newline)
                result))))

(define (fast-expt-debug b n)
    (cond 
        ((= n 0) 1)
        ((even? n) 
            (let ((result (square (fast-expt-debug b (/ n 2)))))
                (display "squaring, result: ") (display result) (newline)
                result))
        (else 
            (let ((result (* b (fast-expt-debug b (- n 1)))))
                (display "multiplying, result: ") (display result) (newline)
                result))))


(define (expmod base exp m)
    (remainder (fast-expt base exp) m))

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


; Alyssa's version also works but it takes a lot of time than expmod version
(display (search-for-primes 100000 3))
(newline)

(expmod-debug 2 10 7)
(newline)
(fast-expt-debug 2 20)

; in Alyssa's version value to be computed grows exponentially and in expmod since we're using remainder operation, it always stays less than 7 and values are easier to computes, both operation take same number of steps which is O(log n)

; in q1.26, the function Louis has came up with is of O(n) because it first processes expmod operation before computing square, and does it twice, when we call square method, we first process expmod procedure only once(resulting in a O(logn) complexity)
