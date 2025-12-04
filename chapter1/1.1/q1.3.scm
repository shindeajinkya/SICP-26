;  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(define (square x) (* x x))
; first and two largest numbers in set of two
; then return sum of squared of those two
(define (sumOfLargestSquares a b c)
        (cond ((and (> a c) (> b c)) (+ (square a) (square b)))
                ((and (> b a) (> c a)) (+ (square b) (square c)))
                ((and (> a b) (> c b)) (+ (square a) (square c)))))

(display (sumOfLargestSquares 2 3 4))
(newline)
(display (sumOfLargestSquares 3 4 6))
