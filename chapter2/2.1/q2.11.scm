(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (spans-zero interval) (and (< (lower-bound interval) 0) (> (upper-bound interval) 0)))

(define (mul-interval-alt x y)
	(cond
		((and (spans-zero x) (spans-zero y)) (mul-interval x y))
		((or (spans-zero x) (spans-zero y)) (
			cond
				((and (not (spans-zero x)) (>= (lower-bound x) 0)) 
					(let
						(
							(p1 (* (upper-bound x) (lower-bound y)))
							(p2 (* (upper-bound x) (upper-bound y)))
						)
						(make-interval (min p1 p2) (max p1 p2))
					)
				)
				((and (not (spans-zero x)) (< (lower-bound x) 0))
					(let
						(
							(p1 (* (lower-bound x) (upper-bound y)))
							(p2 (* (lower-bound x) (lower-bound y)))
						)
						(make-interval (min p1 p2) (max p1 p2))
					)
				)
				((and (not (spans-zero y)) (>= (lower-bound y) 0))
					(let
						(
							(p1 (* (lower-bound x) (upper-bound y)))
							(p2 (* (upper-bound x) (upper-bound y)))
						)
						(make-interval (min p1 p2) (max p1 p2))
					)
				)
				((and (not (spans-zero y)) (< (lower-bound y) 0))
					(let
						(
							(p1 (* (upper-bound x) (lower-bound y)))
							(p2 (* (lower-bound x) (lower-bound y)))
						)
						(make-interval (min p1 p2) (max p1 p2))
					)
				)
		))
		(else 
			(let 
				(
					(p1 (* (lower-bound x) (lower-bound y)))
					(p2 (* (upper-bound x) (upper-bound y)))
				)
				(make-interval (min p1 p2) (max p1 p2))
			)
		)
	)
)
