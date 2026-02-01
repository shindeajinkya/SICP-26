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

(display (+ 2 (cont-frac
                (lambda (i) 1.0)
                (lambda (i) 
                    (if (= (remainder i 3) 2)
                        (* (/ (+ i 1) 3) 2)
                        1)
                )
                20)))
