(define (square x) (* x x))
(define (cube x) (* x x x))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (/ 
		(+ (/ x (square guess)) (* 2 guess))
		3))

(define (new-good-enough? guess x)
	(<
		(/ (abs (- (improve guess x) guess)) guess)
		0.001
	)
)

(define (cube-root-iter guess x)
        (if (new-good-enough? guess x)
            guess
            (cube-root-iter (improve guess x) x)))

(define (cube-root x)
    (cube-root-iter 1.0 x))

(display (cube-root 27))
