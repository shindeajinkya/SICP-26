(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

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

(define (flatmap proc seq)
	(accumulate append (list) (map proc seq)))


(define (adjoin-position row k queens) (cons (cons row k) queens))

(define (safe? k positions)
	(=
		(length 
			(filter 
				(lambda (x) 
					(let ((lq (car positions)))
						(or
							(= (car x) (car lq))
							(= 
								(abs (- (car x) (car lq)))
								(abs (- (cdr x) (cdr lq)))
							)
						)
					)
				)
				(cdr positions)
			)
		)
		0
	)
)

(define (queens board-size)
  (define (queen-cols k)
		(if 
			(= k 0)
			(list (list))
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map 
							(lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
							(enumerate-interval 1 board-size)
						)
					)
					(queen-cols (- k 1))
				)
			)
		)
	)
  (queen-cols board-size)
)

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
	(if 
		(null? s) ; empty set?
		(list (list)) ; sequence containing empty set
		(flatmap 
			(lambda (x)
				(map 
					(lambda (p) (begin
						;(newline)
						;(display p)
						(cons x p)
					))
					(begin 
						(display (remove x s))
						(newline)
						(permutations (remove x s))
					)
				)
			)
			s
		)
	)
)

(display 
	(length (flatmap 
		(lambda (i)
			(map 
				(lambda (j) (list i j))
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 0 6)
	))
)

(newline)
(display (remove 1 (list 1 2 3)))
(newline)
(display (permutations (list 1 2 3)))