(define (transform-painter painter origin corner1 corner2)
	(lambda (frame)
		(let 
			((m (frame-coord-map frame)))
			(let 
				((new-origin (m origin)))
				(painter 
					(make-frame
						new-origin
						(sub-vect (m corner1) new-origin)
						(sub-vect (m corner2) new-origin)
					)
				)
			)
		)
	)
)

(define (flip-vert painter)
	(transform-painter 
		painter
		(make-vect 0.0 1.0) ; new origin
		(make-vect 1.0 1.0) ; new end of edge1
		(make-vect 0.0 0.0)
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

;flip-horz
(define (flip-horz painter)
	(transform-painter 
		painter
		(make-vect 1.0 0.0) ; new origin
		(make-vect 0.0 0.0) ; new end of edge1
		(make-vect 1.0 1.0)
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