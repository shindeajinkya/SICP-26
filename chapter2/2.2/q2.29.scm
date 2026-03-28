(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile) 
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (total-weight mobile)
    (cond
        ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else 
            (+
                (total-weight (branch-structure (left-branch mobile)))
                (total-weight (branch-structure (right-branch mobile)))
            )
        )
    )
)

(define (is-balanced? mobile)
    (cond
        ((null? mobile) true)
        ((not (pair? mobile)) true)
        (else 
            (and
                (
                    =
                    (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
                    (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))
                )
                (is-balanced? (branch-structure (left-branch mobile)))
                (is-balanced? (branch-structure (right-branch mobile)))
            )
        )
    )
)

(define test-mobile
  (make-mobile
    (make-branch 2 5)
    (make-branch 3 10)))

(define nested-mobile
  (make-mobile
    (make-branch 2 
      (make-mobile
        (make-branch 1 3)
        (make-branch 1 4)))
    (make-branch 3 8)))

(define balanced-mobile
  (make-mobile
    (make-branch 3 10)
    (make-branch 2 15)))

(define nested-balanced
  (make-mobile
    (make-branch 4 6)
    (make-branch 3 
      (make-mobile
        (make-branch 2 4)
        (make-branch 2 4)))))

(display (total-weight test-mobile)) ;-> 15
(newline)
(display (total-weight nested-mobile)) ;-> 15
(newline)
(display (is-balanced? test-mobile)) ;-> #f
(newline)
(display (is-balanced? nested-mobile)) ;-> #f
(newline)
(display (is-balanced? balanced-mobile)) ;-> #t
(newline)
(display (is-balanced? nested-balanced)) ;-> #t


(define (make-mobile-cons left right) (cons left right))
(define (make-branch-cons length structure)
    (cons length structure))

; if we use cons(pair) version we only have to change definition of right-branch and branch-structure, remove the car call from them

(define (right-branch-cons mobile) 
    (cdr mobile))

(define (branch-structure branch)
    (cdr branch))