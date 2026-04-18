(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))


(define (add-vect v1 v2)
	(make-vect
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))
	)
)

(define (sub-vect v1 v2)
	(make-vect
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))
	)
)

(define (scale-vect s v)
	(make-vect
		(* s (xcor-vect v))
		(* s (ycor-vect v))
	)
)

(define v1 (make-vect 3 4))
(define v2 (make-vect 1 2))

(display (add-vect v1 v2))      ; expect a vector with coords (4, 6)
(newline)
(display (sub-vect v1 v2))      ; expect (2, 2)
(newline)
(display (scale-vect 2 v1))     ; expect (6, 8)
