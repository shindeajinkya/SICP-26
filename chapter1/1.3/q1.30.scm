(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
        (sum term (next a) next b))))

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (sum-integers a b)
    (sum-iter identity a inc b))

(display (sum-integers 1 10))