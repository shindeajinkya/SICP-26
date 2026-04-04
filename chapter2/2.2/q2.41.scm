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
	(accumulate append nil (map proc seq)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (equal-sum? pair s)
	(= s (+ (car pair) (cadr pair) (caddr pair)))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (caddr pair) (+ (car pair) (cadr pair) (caddr pair))))

(define (unique-pairs n)
	(flatmap
		(lambda (i)
			(flatmap
				(lambda (j)
					(map 
						(lambda (k) (list i j k))
						(enumerate-interval 1 (- j 1))
					)
				)
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 1 n)
	)
)

(define (sum-pairs-triples n s)
	(map make-pair-sum
		(filter 
			(lambda (x) (equal-sum? x s)) 
			(unique-pairs n)
		)
	)
)

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
