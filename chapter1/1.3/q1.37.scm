
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

(define (cont-fraci n d k)
        (define (compute-frac-iter n d k r)
            (if (= k 0)
                r
                (compute-frac-iter n d (- k 1)
                    (/ (n k) (+ (d k) r))
                ))
        )
        (compute-frac-iter n d k 0)
)

(display (cont-fraci (lambda (i) 1.0)
            (lambda (i) 1.0)
            11))