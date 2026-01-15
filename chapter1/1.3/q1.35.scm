(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
; 1.618032786885245

; golden ratio in mentioned in book = (1 + root 5)/2 = 1.6180
; run against x -> 1 + 1/x