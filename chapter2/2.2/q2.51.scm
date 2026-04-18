(define (beside painter1 painter2)
	(let 
		((split-point (make-vect 0.5 0.0)))
		(let 
			(
				(paint-left
					(transform-painter
						painter1
						(make-vect 0.0 0.0)
						split-point
						(make-vect 0.0 1.0)
					)
				)
				(paint-right
					(transform-painter
						painter2
						split-point
						(make-vect 1.0 0.0)
						(make-vect 0.5 1.0)
					)
				)
			)
			(lambda (frame)
				(paint-left frame)
				(paint-right frame)
			)
		)
	)
)

(define (rotate90 painter)
	(transform-painter 
		painter
		(make-vect 1.0 0.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)
	)
)

;n-rotate180
(define (n-rotate180 painter)
	(transform-painter 
		painter
		(make-vect 1.0 1.0)
		(make-vect 0.0 1.0)
		(make-vect 1.0 0.0)
	)
)
;n-rotate270
(define (n-rotate270 painter)
	(transform-painter 
		painter
		(make-vect 1.0 0.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)
	)
)


(define (below painter1 painter2)
	(let 
		((split-point (make-vect 0.0 0.5)))
		(let 
			(
				(paint-below
					(transform-painter
						painter1
						(make-vect 0.0 0.0)
						(make-vect 1.0 0.0)
						split-point
					)
				)
				(paint-above
					(transform-painter
						painter2
						split-point
						(make-vect 1.0 0.5)
						(make-vect 0.0 1.0)
					)
				)
			)
			(lambda (frame)
				(paint-above frame)
				(paint-below frame)
			)
		)
	)	
)

;n-rotate90
(define (n-rotate90 painter)
	(transform-painter 
		painter
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)
	)
)

(define (below-using-beside painter1 painter2)
	(rotate90 
		(beside 
			(n-rotate90 painter1)
			(n-rotate90 painter2) 
		)
	)
)