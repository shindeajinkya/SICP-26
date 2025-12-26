(define (even? n)
    (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

;(define (fast-mult a b)
;        (cond 
;            ((= b 0) 0)
;            ((even? b) (fast-mult (double a) (halve b)))
;            (else (+ a (fast-mult a (- b 1)))))
;        )

(define (fast-mult a b)
        (fast-mult-iter a b 0))

(define (fast-mult-iter a b x)
        (cond
            ((= b 0) x)
            ((even? b) (fast-mult-iter (double a) (halve b) x))
            (else (fast-mult-iter a (- b 1) (+ x a)))
        ))

(display (fast-mult 3 2))

; (fast-mult 3 2)
; (fast-mult-iter 3 2 1)
; (fast-mult-iter 6 1 1)