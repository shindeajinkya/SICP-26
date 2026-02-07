; (iterative-improve good-enough improve) -> (f guess)
(define (iterative-improve good-enough? improve)
	(define f (lambda (x)
		(let ((next (improve x)))
		(if (good-enough? x next)
			next
			(f next))))) f)


; testing fixed-point
(define tolerance 0.00001)

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
	((iterative-improve close-enough? f) first-guess)
)

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated-with-compose f times)
		(if (= times 1) 
			f
			(compose f (repeated-with-compose f (- times 1))))
)

(define (nth-root x n)
		(fixed-point ((repeated-with-compose average-damp (floor (/ (log n) (log 2)))) (lambda (y) (/ x (expt y (- n 1)))))
		1.0))

(display (nth-root (expt 2 7) 7))
(newline)

; testing sqrt

(define (square x) (* x x))

(define (improve guess x)
	(average guess (/ x guess)))

(define (sqrt x)
	(define (improve guess)
		(average guess (/ x guess)))
	(define (good-enough? guess x)
		(< (abs (- guess x)) 0.001))
	((iterative-improve good-enough? improve) 1.0)
)

(display (sqrt 9))
