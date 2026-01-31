(define tolerance 0.00001)
(define (average a b) (/ (+ a b) 2))
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess step)
        (let ((next (f guess)))
            (display step)
            (display " ")
            (display next)
            (newline)
            (if (close-enough? guess next)
                next
                (try next (+ step 1.0)))))
    (try first-guess 1.0))


(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
(newline)
(newline)
(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

; first method takes around 35 steps
; average damping takes 10 steps