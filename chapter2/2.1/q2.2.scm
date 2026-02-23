(define (make-point x y)
    (cons x y))

(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start end)
    (cons start end))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment seg)
    (let* (
        (p1 (start-segment seg))
        (p2 (end-segment seg))
        (mid-x (/ (+ (x-point p1) (x-point p2)) 2))
        (mid-y (/ (+ (y-point p1) (y-point p2)) 2))
    ) (make-point mid-x mid-y)))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))


(define seg (make-segment (make-point 2 4) (make-point 6 8)))
(define mid (midpoint-segment seg))
(print-point mid)