(define (cont-frac n d k)
        (define (compute-frac n d k s)
            (if (= s k)
                (/ (n s) (d s))
                (/ 
                    (n s) 
                    (+ 
                        (d s) 
                        (compute-frac n d k (+ s 1))
                    )
                )
            )
        )
        (compute-frac n d k 1))

(define (square x) (* x x))

(define (tan-cf x k) 
    (cont-frac 
        (lambda (i) (if (= i 1) x (* -1 (square x))))
        (lambda (i) (- (* i 2) 1))
        k
    ))
(display (tan 1.0))
(newline)
(display (tan-cf 1.0 20))
(newline)
(display (tan 0.7854))
(newline)
(display (tan-cf 0.7854 20))
(newline)