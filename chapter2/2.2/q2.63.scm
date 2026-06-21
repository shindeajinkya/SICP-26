(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
    (cond 
        ((null? set) false)
        ((= x (entry set)) true)
        (
            (< x (entry set))
            (element-of-set? x (left-branch set))
        )
        (
            (> x (entry set))
            (element-of-set? x (right-branch set))
        )
    )    
)

(define (adjoin-set x set)
    (cond 
        ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        (
            (< x (entry set))
            (make-tree 
                (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set)
            )
        )
        (
            (> x (entry set))
            (make-tree 
                (entry set) 
                (left-branch set)
                (adjoin-set x (right-branch set))
            )
        )
    )
)

(define (tree->list-1 tree)
    (if 
        (null? tree)
        '()
        (append 
            (tree->list-1 (left-branch tree))
            (cons 
                (entry tree)
                (tree->list-1
                (right-branch tree))
            )
        )
    )
)
; (tree->list1 test-tree)
; (append (tree-list-> 1 (1 3 5)) (cons 7 (tree->list-1 (9 11))))
; (append (append (tree->list-1 1) (cons 3 (tree->list-1 5))) (cons 7 (append (tree->list-1 '()) (cons 9 (tree->list-1 11)))))
; (append (append (append (tree->list-1 '()) (cons 1 (tree->list-1 '())))) (cons 3 (append (tree->list-1 '() (cons 5 (tree->list-1 '())))))) (cons 7 (append (tree->list-1 '()) (cons 9 (append (tree->list-1 '()) (cons 1 (tree->list-1 '()))))))))
; (append (append (append '() (cons 1 '()))) (cons 3 (append '() (cons 5 '()))))) (cons 7 (append '() (cons 9 (append '() (cons 1 '())))))))
; 

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if 
            (null? tree)
            result-list
            (copy-to-list 
                (left-branch tree)
                (cons 
                    (entry tree)
                    (copy-to-list
                        (right-branch tree)
                        result-list
                    )
                )
            )
        )
    )
    (copy-to-list tree '())
)
; (tree->list1 test-tree)
; (copy-to-list test-tree '())
; (copy-to-list (1 3 5) (cons 7 (copy-to-list (9 11) '())))
; (copy-to-list (1 3 5) (cons 7 (copy-to-list '() (cons 9 (copy-to-list 11 '())))))
; (copy-to-list (1 3 5) (cons 7 (copy-to-list '() (cons 9 (copy-to-list '() (cons 11 (copy-to-list '() '())))))))
; (copy-to-list (1 3 5) (cons 7 (copy-to-list '() (cons 9 (copy-to-list '() (cons 11 '()))))))
; (copy-to-list (1 3 5) (cons 7 (copy-to-list '() (cons 9 (11)))))
; (copy-to-list (1 3 5) (cons 7 (9 11)))
; (copy-to-list (1 3 5) '(7 9 11))
; (copy-to-list (1) (cons 3 (copy-to-list (5) '(7 9 11))))
; (copy-to-list (1) (cons 3 (copy-to-list '() (cons 5 (copy-to-list '() '(7 9 11))))))
; (copy-to-list (1) (cons 3 (copy-to-list '() '(5 7 9 11))))
; (copy-to-list (1) '(3 5 7 9 11))
; (copy-to-list '() (cons 1 (copy-to-list '() '(3 5 7 9 11))))
; (copy-to-list '() '(1 3 5 7 9 11))
; '(1 3 5 7 9 11)

(define test-tree
    (make-tree
        7
        (make-tree
            3
            (make-tree 1 '() '())
            (make-tree 5 '() '())
        )
        (make-tree
            9
            '()
            (make-tree 11 '() '())
        )
    )
)

(display (tree->list-1 test-tree))
(newline)
(display (tree->list-2 test-tree))

; a: both the procedures produce same result
; b: first procedure grown by the o(nlogn) and second one is o(n), so first one is slower than the second one
