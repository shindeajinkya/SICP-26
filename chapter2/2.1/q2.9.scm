(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
    (make-interval 
        (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))
    ))

(define (sub-interval x y)
    (make-interval 
        (- (lower-bound x) (upper-bound y))
        (- (upper-bound x) (lower-bound y))
    ))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval
        x
        (make-interval 
            (/ 1.0 (upper-bound y))
            (/ 1.0 (lower-bound y))
    )))

(define (width interval) 
    (
        /
        (- (upper-bound interval) (lower-bound interval))
        2
    ))

(define A (make-interval 2.12 2.21))
(define B (make-interval 2.76 2.86))

(define C (make-interval 100 100.09))
(define D (make-interval 1 1.1))

(display (width A))
(newline)
(display (width B))
(newline)
(display (width C))
(newline)
(display (width D))
(newline)
(display (+ (width A) (width B)))
(newline)
(display (+ (width C) (width B)))
(newline)
(display (* (width A) (width B)))
(newline)
(display (* (width C) (width D)))
(newline)
(display (width (mul-interval A B)))
(newline)
(display (width (mul-interval C D)))

; .04499999999999993
; 5.0000000000000044e-2
; 4.5000000000015916e-2
; 5.0000000000000044e-2
; .09499999999999997
; .09500000000001596
; 2.2499999999999985e-3
; 2.250000000000798e-3
; .23470000000000013
; 5.049500000000009%

; width(A + B) -> width (lo_A + lo_B, hi_A, hi_B)
