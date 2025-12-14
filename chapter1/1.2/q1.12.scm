; Pascal's triangle
(define (pascal-triangle n)
        ; finding [row][column] position
        (define (pascal row col)
                (cond ((> col row) 0)
                        ((or (= col 1) (= row 1)) 1)
                        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))
                ))
        ; computing a single row
        (define (pascal-row n start)
                (if (> start n)
                    (newline)
                    (begin
                        (display (pascal n start))
                        (display " ")
                        (pascal-row n (+ start 1))
                    )))
        ;print whole triangle
        (define (print-triangle n start)
                (if (> start n)
                    0
                    (begin
                        (pascal-row start 1)
                        (print-triangle n (+ start 1)))
                ))
        (print-triangle n 1)
)

(pascal-triangle 5)