(define (is-even? n) (= (remainder n 2) 0))
(define (is-odd? n) (not (is-even? n)))

(define (same-parity-iter . seq)
    (define (iter x check result)
        (cond
            ((null? x) result)
            ((check (car x)) (iter (cdr x) check (append result (list (car x)))))
            (else (iter (cdr x) check result))
        ))
    (if 
        (is-even? (car seq))
        (iter seq is-even? (list))
        (iter seq is-odd? (list))
    )    
)

(define (same-parity . seq)
    (define (recur x check)
            (cond
                ((null? x) (list))
                ((check (car x)) (cons (car x) (recur (cdr x) check)))
                (else (recur (cdr x) check))
            )
    )
    (if 
        (is-even? (car seq))
        (recur seq is-even?)
        (recur seq is-odd?)
    )
)

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity-iter 2 3 4 5 6 7))