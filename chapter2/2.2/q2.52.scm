;part a

(define (seg x1 y1 x2 y2)
  (make-segment (make-vect x1 y1) (make-vect x2 y2)))

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
            ;smile
            (seg 0.40 0.90 0.40 0.90)
            (seg 0.52 0.90 0.52 0.90)
            (seg 0.38 0.80 0.42 0.78)
            (seg 0.42 0.78 0.50 0.78)
            (seg 0.50 0.78 0.54 0.80)
		)
	)
)

;part b

(define (split pre post)
	(define (rec-split painter n)
		(if 
			(= n 0)
			painter
			(let 
				((smaller (rec-split painter (- n 1))))
				(pre painter (post smaller smaller))
			)
		)
	)
	rec-split
)

(define right-split (split beside below))
(define up-split (split below beside))


(define (corner-split painter n)
	(if 
		(= n 0)
		painter
		(let 
            (
                (up (up-split painter (- n 1)))
                (right (right-split painter (- n 1)))
                (corner (corner-split painter (- n 1)))
            )
            (beside (below painter up)
                    (below right corner))
        )
	)
)

; part c
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

(paint (square-limit einstein 2))

