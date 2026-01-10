(define (even? n)
    (= (remainder n 2) 0))

(define (cube n) (* n n n))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
        (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
        dx))

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (y-val x y z) (f (+ x (* y z))))
    (define (next x) (+ x 1))
    (define (compute-simpson k) 
        (cond 
            ((or (= k 0) (= k n)) (y-val a k h))
            ((even? k) (* 2 (y-val a k h)))
            (else (* 4 (y-val a k h)))
        ))
    (* 
        (sum compute-simpson 0 next n) 
        (/ h 3)
    )
)

(display (integral cube 0 1 0.01))
(newline)
;.24998750000000042
(display (integral cube 0 1 0.001))
(newline)
;.249999875000001

(display (simpson-integral cube 0 1 100))
(newline)
; 1/4
(display (simpson-integral cube 0 1 1000))
(newline)