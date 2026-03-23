; (reverse (list 1 4 9 16 25)) -> (25 16 9 4 1)
(define (reverse-iterative seq)
    (define (reverse-iter seq result)
                (if 
                    (null? (cdr seq))
                    (cons (car seq) result)
                    (reverse-iter (cdr seq) (cons (car seq) result))
            ))
    (reverse-iter seq (list))
)

(define (reverse seq)
    (if
        (null? seq)
        (list)
        (append (reverse (cdr seq)) (list (car seq)))
    ))

(display (reverse (list 1 4)))
(newline)
(display (reverse (list 1 4 9 16 25)))