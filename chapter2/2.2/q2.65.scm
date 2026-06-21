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

(define (partial-tree elts n)
    (newline)
    (display elts)
    (display " elts")
    (newline)
    (display n)
    (display " n")
    (newline)
    (if 
        (= n 0)
        (cons '() elts)
        (let 
            ((left-size (quotient (- n 1) 2)))
            (display left-size)
            (display " left-size")
            (newline)
            (let 
                ((left-result
                    (partial-tree elts left-size))
                )
                (display left-result)
                (display " left-result")
                (newline)
                (let 
                    (
                        (left-tree (car left-result))
                        (non-left-elts (cdr left-result))
                        (right-size (- n (+ left-size 1)))
                    )
                    (display left-tree)
                    (display " left-tree")
                    (newline)
                    (display non-left-elts)
                    (display " non-left-elts")
                    (newline)
                    (display right-size)
                    (display " right-size")
                    (newline)
                    (let 
                        (
                            (this-entry (car non-left-elts))
                            (right-result
                                (partial-tree (cdr non-left-elts) right-size)
                            )
                        )
                        (display this-entry)
                        (display " this-entry")
                        (newline)
                        (let 
                            (
                                (right-tree (car right-result))
                                (remaining-elts (cdr right-result))
                            )
                            (display right-tree)
                            (display " right-tree")
                            (newline)
                            (display remaining-elts)
                            (display " remaining-elts")
                            (newline)
                            (display "ends")
                            (newline)
                            (newline)
                            (cons 
                                (make-tree this-entry left-tree right-tree)
                                remaining-elts
                            )
                        )
                    )
                )
            )
        )
    )
)

(define (list->tree elements)
    (car (partial-tree elements (length elements)))
)

(define (intersection-set set1 set2)
    (if 
        (or (null? set1) (null? set2))
        '()
        (let 
            ((x1 (car set1)) (x2 (car set2)))
            (cond 
                ((= x1 x2)
                    (cons 
                        x1 
                        (intersection-set (cdr set1) (cdr set2))
                    )
                )
                ((< x1 x2)
                    (intersection-set (cdr set1) set2)
                )
                ((< x2 x1)
                    (intersection-set set1 (cdr set2))
                )
            )
        )
    )
)

(define (union-set set1 set2)
    (cond
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let
            ((x1 (car set1)) (x2 (car set2)))
            (cond
                ((= x1 x2)
                    (cons 
                        x1 
                        (union-set (cdr set1) (cdr set2))
                    )
                )
                ((< x1 x2)
                    (cons x1 (union-set (cdr set1) set2))
                )
                ((< x2 x1)
                    (cons x2 (union-set set1 (cdr set2)))
                )
            )
        ))
    )
)

(define (union-set-tree set1 set2)
    (list->tree
        (union-set
            (tree->list-2 set1)
            (tree->list-2 set2)
        )
    )
)

(define (intersection-set-tree set1 set2)
    (list->tree
        (intersection-set
            (tree->list-2 set1)
            (tree->list-2 set2)
        )
    )
)
