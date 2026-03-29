(define (square x) (* x x))

(define (tree-map proc tree)
    (cond 
        ((null? tree) (list))
        ((not (pair? tree)) (proc tree))
        (else 
            (cons 
                (tree-map proc (car tree))
                (tree-map proc (cdr tree))
            )
        )
    ))

(define (square-tree tree) (tree-map square tree))

(define (square-tree-map tree)
    (map 
        (lambda (sub-tree)
            (if 
                (pair? sub-tree)
                (square-tree-map sub-tree)
                (square sub-tree)
            ))
        tree
    )
)

(display (square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))))
