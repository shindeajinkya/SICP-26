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
					(lambda (new-row)
						(map 
							(lambda (rest-of-queens)
								(adjoin-position new-row k rest-of-queens))
							(queen-cols (- k 1))
						)
					)
					(enumerate-interval 1 board-size)
				)
			)
		)
	)
  (queen-cols board-size)
)

; this takes around 8^8(approx 16 million in time complexity), so it's like n^n in terms of big o notation
(display (length (queens 8)))