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

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
            (filtered-accumulate filter combiner null-value term (next a) next b)
        )
    ))

(define (inc x) (+ x 1))
(define (identity x) x)

(define (sum filter term a next b)
        (filtered-accumulate filter + 0 term a next b))

(define (product filter term a next b)
        (filtered-accumulate filter * 1 term a next b))

(define (sum-of-prime-squared n) (sum prime? square 2 inc n))

(define (sum-of-relatively-prime n) 
        (define (is-relative-prime? a) (= (gcd a n) 1))
        (product is-relative-prime? identity 2 inc (- n 1)))

(display (sum-of-prime-squared 20)) ;1028
(newline)
(display (sum-of-relatively-prime 20)) ;8729721
(newline)