(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
	(/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage-to-value n p) (* n (/ percentage 100)))

(define (make-center-percent c p)
	(make-center-width c (percentage-to-value c p)))

(define (percent interval)
	(abs (
		*
		(/ (width interval) (center interval))
		100
	)))

; value = (* n (/ percentage 100))
; percentage = (* (/ value n) 100)