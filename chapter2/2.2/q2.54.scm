(define (equal? list1 list2)
    (cond
        ((and (null? list1) (null? list2)) true)
        ((or (null? list1) (null? list2)) false)
        (else 
            (let   
                (
                    (cur1 (car list1))
                    (cur2 (car list2))
                )
                (cond
                    ((and (pair? cur1) (pair? cur2)) (and (equal? cur1 cur2) (equal? (cdr list1) (cdr list2))))
                    ((not (eq? cur1 cur2)) false)
                    (else (equal? (cdr list1) (cdr list2)))
                )
            )
        )
    )
)

(display (equal? '(this is a list) '(this is a list)))
(newline)
(display (equal? '(this (is a) list) '(this (is a) list)))
(newline)
(display (eq? '(this list) '(this list)))

;there's a cleaner version claude suggested which makes so much and sense and less verbose than my solution
(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) false)
        (else (eq? a b))))
