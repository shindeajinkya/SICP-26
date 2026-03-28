(define (count-leaves x)
    (cond 
        ((null? x) 0)
        ((not (pair? x)) 1)
        (else 
            (+ 
                (count-leaves (car x))
                (count-leaves (cdr x))
            ))
    )
)
(define (fringe x)
    (cond 
        ((null? x) (list))
        ((not (pair? x)) (list x))
        (else 
            (append 
                (fringe (car x))
                (fringe (cdr x))
            ))
    )
)

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)
(display (fringe (list x x)))
