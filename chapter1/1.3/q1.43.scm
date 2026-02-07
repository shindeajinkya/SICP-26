(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f times)
	(lambda (x)
		(if (= times 1) 
			(f x) 
			(f ((repeated f (- times 1)) x))
	))
)

(define (repeated-with-compose f times)
		(if (= times 1) 
			f
			(compose f (repeated-with-compose f (- times 1))))
)

(display ((repeated-with-compose square 3) 5))
; ((repeated square 2) 5)