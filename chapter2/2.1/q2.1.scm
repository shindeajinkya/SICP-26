; if any one of the argument is negative make the numerator negative, keep the denominator as it
; or take a sign argument in the procedure (overkill i think personally)
(define (make-rat n d)
    (let* (
        (sn 
            (cond 
                ((and (> n 0) (< d 0)) (* n -1))
                ((and (< n 0) (< d 0)) (abs n)) 
                (else n))
        )
        (sd (abs d))
        (g (gcd sn sd))
        )
        (cons (/ sn g) (/ sd g))))

;(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (add-rat x y)
    (make-rat 
        (+ (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat 
        (- (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
        (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
        (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
        (* (numer y) (denom x))))

(print-rat (make-rat 6 9))    ; positive/positive
(print-rat (make-rat -6 9))   ; negative/positive
(print-rat (make-rat 6 -9))   ; positive/negative
(print-rat (make-rat -6 -9))  ; negative/negative
