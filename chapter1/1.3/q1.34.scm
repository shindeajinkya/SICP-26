(define (f g) (g 2))

(display (f square))
(newline)
(display (f (lambda (z) (* z (+ z 1)))))
(newline)
(display (f f))
; (f 2)
; (2 2)
