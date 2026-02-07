(define dx 0.00001)

(define (square x) (* x x))

(define (average a b c) (/ (+ a b c) 3))

(define (smooth f) 
			(lambda (x)
				(average
						(f (- x dx))
						(f x)
						(f (+ x dx))
				)
			)
)

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated-with-compose f times)
		(if (= times 1) 
			f
			(compose f (repeated-with-compose f (- times 1))))
)

(define (smoothed-nfold times) (repeated-with-compose smooth times))

;; unsmoothed
(display (square 5))
(newline)
;; smoothed once
(display ((smooth square) 5))
(newline)
;; smoothed 3 times
(display (((smoothed-nfold 3) square) 5))
