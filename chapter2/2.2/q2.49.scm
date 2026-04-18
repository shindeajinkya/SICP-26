(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))


(define (add-vect v1 v2)
	(make-vect
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))
	)
)

(define (sub-vect v1 v2)
	(make-vect
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))
	)
)

(define (scale-vect s v)
	(make-vect
		(* s (xcor-vect v))
		(* s (ycor-vect v))
	)
)

(define (make-segment start end)
    (cons start end))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					(
						(frame-coord-map frame)
						(start-segment segment)
					)
					(
						(frame-coord-map frame)
						(end-segment segment)
					)
				)
			)
			segment-list
		)
	)	
)

;a
(define outline
	(segments->painter
		(list 
			(make-segment
				(make-vect 0 0)
				(make-vect 1 0)
			)
			(make-segment
				(make-vect 1 0)
				(make-vect 1 1)
			)
			(make-segment
				(make-vect 1 1)
				(make-vect 0 1)
			)
			(make-segment
				(make-vect 0 1)
				(make-vect 0 0)
			)
		)
	)
)
;b
(define xFrame
	(segments->painter
		(list 
			(make-segment
				(make-vect 0 0)
				(make-vect 1 1)
			)
			(make-segment
				(make-vect 0 1)
				(make-vect 1 0)
			)
		)
	)
)
;c
(define diamond
	(segments->painter
		(list 
			(make-segment
				(make-vect 0.5 0)
				(make-vect 1 0.5)
			)
			(make-segment
				(make-vect 1 0.5)
				(make-vect 0.5 1)
			)
			(make-segment
				(make-vect 0.5 1)
				(make-vect 0 0.5)
			)
			(make-segment
				(make-vect 0 0.5)
				(make-vect 0.5 0)
			)
		)
	)
)
;d