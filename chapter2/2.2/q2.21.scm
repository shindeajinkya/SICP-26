(define (square x) (* x x))

(define (square-list items)
    (if 
        (null? items)
        (list)
        (cons (square (car items)) (square-list (cdr items)))
    ))

(define (square-list-map items)
    (map square items))

(display (square-list (list 1 2 3 4)))
(newline)
(display (square-list-map (list 1 2 3 4)))