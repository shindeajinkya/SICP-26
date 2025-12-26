(define (* a b)
    (if (= b 0)
    0
    (+ a (* a (- b 1)))))

(define (even? n)
    (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
        (cond 
            ((= b 0) 0)
            ((even? b) (fast-mult (double a) (halve b)))
            (else (+ a (fast-mult a (- b 1)))))
        )

(display (fast-mult 12 4))

; (* 3 2)
; (+ 3 (* 3 (- 2 1)))
; (+ 3 (* 3 1))
; (+ 3 (+ 3 (* 3 0)))
; (+ 3 (+ 3 0))
; (+ 3 3)
; 6
