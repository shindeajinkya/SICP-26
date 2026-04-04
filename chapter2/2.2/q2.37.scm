(define (accumulate op initial sequence)
        (if 
            (null? sequence)
            initial
            (op 
                (car sequence)
                (accumulate op initial (cdr sequence))
            )
        )
)

(define (accumulate-n op init seqs)
    (if 
        (null? (car seqs))
        (list)
        (cons 
            (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))
        )
    )
)

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
	(accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
	(let 
		((cols (transpose n)))
		(map (lambda (x) (matrix-*-vector cols x)) m)
	)
)

(display 
	(matrix-*-matrix 
		(list (list 1 2 3) (list 4 5 6) (list 7 8 9))
		(list (list 1 0 2) (list 0 1 0) (list 1 0 1))
	)
) ; ((4 2 5) (10 5 14) (16 8 23))
