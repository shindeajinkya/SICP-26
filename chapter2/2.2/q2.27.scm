(define (reverse seq)
    (if
        (null? seq)
        (list)
        (append (reverse (cdr seq)) (list (car seq)))
    )
)

(define (deep-reverse x) 
    (cond
        ((null? x) (list))
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
    )
)

(define x (list (list 1 2) (list 3 4)))

(newline)
(display (deep-reverse x))
