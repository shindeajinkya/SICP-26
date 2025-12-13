; Recurive Process
(define (fr n)
    (if (< n 3)
        n
        (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))

(display (fr 2))
(newline)
(display (fr 3))
(newline)
(display (fr 4))
(newline)
(display (fr 5))
(newline)
(display (fr 6))
(newline)

; iterative process
(define (fi n)
        (define (fi-iter a b c count)
                (if (= count (- n 1))
                    a
                    (fi-iter 
                        (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (+ count 1)
                    )))
        (fi-iter 2 1 0 1))

(display (fi 2))
(newline)
(display (fi 3))
(newline)
(display (fi 4))
(newline)
(display (fi 5))
(newline)
(display (fi 6))
(newline)
