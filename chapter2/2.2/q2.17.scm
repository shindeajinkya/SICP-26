; (last-pair (list 23 72 149 34)) -> (34)

(define (last-pair seq)
    (if (null? (cdr seq))
        (car seq)
        (last-pair (cdr seq))
    )
)

(display (last-pair (list 23 72 149 34 45)))
