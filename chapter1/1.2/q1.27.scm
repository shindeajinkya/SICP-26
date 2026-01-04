(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (remainder
                (square (expmod base (/ exp 2) m))
                m))
        (else
            (remainder
                (* base (expmod base (- exp 1) m))
                m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (test-carmichael n) 
    (define (try-it a)
        (= (expmod a n n) a))
    (define (carmichael-iter n a) 
        (cond ((= n a) true)
            ((not (try-it a)) false)
            (else (carmichael-iter n (+ a 1)))))
    (carmichael-iter n 2))

(display (test-carmichael 561))
(newline)
(display (test-carmichael 1105))
(newline)
(display (test-carmichael 1729))
(newline)
(display (test-carmichael 2465))
(newline)
(display (test-carmichael 2821))
(newline)
(display (test-carmichael 6601))
(newline)
(display (test-carmichael 7))
(newline)
(display (test-carmichael 10))

; carmichael number, 561, 1105, 1729, 2465, 2821, and 6601
