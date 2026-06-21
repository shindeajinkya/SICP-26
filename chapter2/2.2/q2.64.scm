(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

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

(display (list->tree '(1 3 5 7 9 11)))
(newline)


; this recursively breaking down the list by 2 and breaking it to create 
; a balanced binary tree. it's doing that by producing left-tree and right-tree 
; at the same time, also i think this procedure is assuming list of elts is ordered

; this procedure visits every element once, so it's order of growth is O(n)