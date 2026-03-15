(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

(display (cdr (cons 1 4)))

; (car (cons 1 4))
; (car (lambda (m) (m 1 4)))
; (car (lambda (m) (m 1 4)))
; ((lambda (m) (m 1 4)) (lambda (p q) p))
; ((lambda (p q) p) 1 4)
; 1
