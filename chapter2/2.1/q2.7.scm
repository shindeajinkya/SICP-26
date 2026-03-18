(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(display (lower-bound (make-interval 2.12 2.20)))
(newline)
(display (upper-bound (make-interval 2.12 2.20)))
