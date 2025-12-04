(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
    (else else-clause)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    
        (display (good-enough? guess x))
        (newline)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(display (sqrt 9))

// new-if causes to run in an infinte loop cause new-if first evaluates all the arguments being sent to it, this causes sqrt-iter to be called in an infinite loop
