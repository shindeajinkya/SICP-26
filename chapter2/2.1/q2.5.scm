(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car c)
    (define (find-a d n)
        (if (not (= (remainder d 2) 0))
            n
            (find-a (/ d 2) (+ n 1)))
    )
    (find-a c 0))

(define (cdr c)
    (define (find-b d n)
        (if (not (= (remainder d 3) 0))
            n
            (find-b (/ d 3) (+ n 1)))
    )
    (find-b c 0))

(display (car (cons 0 0)))
(newline)
(display (cdr (cons 0 0)))
(newline)
(display (car (cons 3 2)))
(newline)
(display (cdr (cons 3 2)))

; find-a and find-b can be abstracted into a common procedure, but kept it separate for readability