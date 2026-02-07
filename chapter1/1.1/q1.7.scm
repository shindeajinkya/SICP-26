(define (average x y)
    (/ (+ x y) 2))

(define (square x) (* x x))

(define (improve guess x)
    (average guess (/ x guess)))

(define (new-good-enough? guess x)
	(<
		(/ (abs (- (improve guess x) guess)) guess)
		0.001
	)
)

(define (sqrt-iter guess x)
        (if (new-good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(display (sqrt 9))
(newline)
(display (sqrt 0.0004))
(newline)
(display (sqrt 160000000))
(newline)
(display (square 40000))
