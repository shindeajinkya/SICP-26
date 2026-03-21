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

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (mul-center-percent c1 c2 p1 p2)
	(make-center-percent (* c1 c2) (+ p1 p2)))

(define interval1 (make-center-percent 3.5 0.15))
(define interval2 (make-center-percent 4.0 0.05))
