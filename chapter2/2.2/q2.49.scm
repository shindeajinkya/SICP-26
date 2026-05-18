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

(define (seg x1 y1 x2 y2)
  (make-segment (make-vect x1 y1) (make-vect x2 y2)))

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

(define wave
	(segments->painter
		(list
                        (seg 0 0 1 0)
                        (seg 1 0 1 1)
                        (seg 1 1 0 1)
                        (seg 0 1 0 0)
                        ;top left
                        (seg 0 0.75 0.15 0.58)
                        (seg 0.15 0.58 0.20 0.58)
                        (seg 0.20 0.58 0.28 0.68)
                        (seg 0.28 0.68 0.32 0.68)
                        (seg 0.32 0.68 0.35 0.64)
                        (seg 0.35 0.64 0.38 0.70)
                        (seg 0.38 0.70 0.32 0.80)
                        (seg 0.32 0.80 0.32 0.85)
                        (seg 0.32 0.85 0.38 1.0)
                        ;bottom left
                        (seg 0 0.70 0.13 0.48)
                        (seg 0.13 0.48 0.20 0.48)
                        (seg 0.20 0.48 0.26 0.55)
                        (seg 0.26 0.55 0.32 0.54)
                        (seg 0.32 0.54 0.24 0)
                        ;bottom center
                        (seg 0.34 0 0.40 0.3)
                        (seg 0.40 0.3 0.48 0.32)
                        (seg 0.48 0.32 0.56 0.3)
                        (seg 0.56 0.3 0.62 0)
                        ;bottom right
                        (seg 0.75 0 0.60 0.48)
                        (seg 0.60 0.48 0.62 0.54)
                        (seg 0.62 0.54 1 0.32)
                        ;top right
                        (seg 1 0.40 0.70 0.63)
                        (seg 0.70 0.63 0.66 0.65)
                        (seg 0.66 0.65 0.6 0.61)
                        (seg 0.6 0.61 0.57 0.68)
                        (seg 0.57 0.68 0.63 0.79)
                        (seg 0.63 0.79 0.63 0.84)
                        (seg 0.63 0.84 0.56 1)
		)
	)
)