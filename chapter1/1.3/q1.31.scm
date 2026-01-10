(define (even? n)
    (= (remainder n 2) 0))

(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
        (product term (next a) next b))))

(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (inc x) (+ x 1))
(define (identity x) x)

(define (factorial n) (product-iter identity 1 inc n))

(define (pi-approx n)
        (define (compute-pi x) 
            (if (even? x) 
                (/ (+ x 2) (+ x 1)) 
                (/ (+ x 1) (+ x 2))
            ))
        (* 4.0 (product compute-pi 1 inc n)))

(display (factorial 5))
(newline)
(display (pi-approx 100))
(newline)
(newline)
(display (pi-approx 100000))
(newline)