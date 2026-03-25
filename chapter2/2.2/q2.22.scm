(define (square-list-v1 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                (cons (square (car things))
                    answer))
        ))
    (iter items (list)))

; (iter (list 1 2) (list))
; (iter (2) (cons (square 1) (list)))
; (iter (2) (1))
;   (iter nil (cons (square 2) (1)))
;   (4 1)
; iter with cons first evaluates the cons part of the procedure and then passes it to cons method, this causes the list to get reversed
; to solve this we can first create a list using cons and iterate on the cdr part of it

(define (square-list-v2 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (cons (square (car things)) (iter (cdr things) answer))
        ))
    (iter items (list)))

(display (square-list-v1 (list 1 2)))
(newline)
(display (square-list-v2 (list 1 2)))