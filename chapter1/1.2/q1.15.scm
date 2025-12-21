(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
    (if (not (> (abs angle) 0.1))
            angle
            (p (sine (/ angle 3.0)))))

; (sine 12.15)
; (p (sine (/ 12.15 3.0)))
; (p (sine 4.05)) -> 1
; (p (p (sine (/ 4.05 3.0)))) -> 2
; (p (p (sine 1.3499999999999999))) -> 2
; (p (p (p (sine (/ 1.3499999999999999 3.0))))) -> 3
; (p (p (p (sine .44999999999999996)))) -> 3
; (p (p (p (p (sine (/ .44999999999999996 3.0)))))) -> 4
; (p (p (p (p (sine 0.15))))) -> 4
; (p (p (p (p (p (sine (/ 0.15 3.0))))))) -> 5
; (p (p (p (p (p (sine 4.9999999999999996e-2)))))) -> 5


; a: procedure p is evalutated 5 for computing 12.15
; b: order of growth for this function is O(log(a)) with base of 3 (took help from claude(mostly hints) and watched this video for logarithms refresher https://youtu.be/zzu2POfYv0Y?si=kA8_FcUfU3yjrDuW)

