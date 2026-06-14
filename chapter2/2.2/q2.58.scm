(define (accumulate op initial sequence)
        (if 
            (null? sequence)
            initial
            (op 
                (car sequence)
                (accumulate op initial (cdr sequence))
            )
        )
)

(define (variable? x) (symbol? x))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))
    )
)

(define (make-product m1 m2)
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
    )
)

(define (unwrap-single lst)
  (if (and (pair? lst) (null? (cdr lst)))
      (car lst)
      lst))

(define (find-operator-position exp)
  ; scan exp and return the position of the first +
  ; if no +, return the position of the first *
  ; (x * y + z)
  (define (find-position left right)
    ;(display left)
    ;(newline)
    ;(display right)
    ;(newline)
    ;(display (cdr (cdr right)))
    ;(newline)
    (cond
        ((< (length right) 2) (unwrap-single (append left right)))
        ((eq? (cadr right) '+) 
            (list 
                '+ 
                (unwrap-single (append left (list (car right)))) 
                (unwrap-single (cdr (cdr right)))
            )
        )
        ((eq? (cadr right) '*) 
            (find-position 
                (append left (list (cadr right)) (list (car right))) 
                (cdr (cdr right))
            )
        )
        (else 
            (unwrap-single (list '* left right))
        )
    )
  )
  (find-position (list) exp)
)

(define (sum? x) (and (pair? x) (eq? (car (find-operator-position x)) '+)))

(define (addend s) (cadr (find-operator-position s)))

(define (augend s)
  (caddr (find-operator-position s))
)

(define (product? x) (and (pair? x) (eq? (car (find-operator-position x)) '*)))

(define (multiplier p) (cadr (find-operator-position p)))

(define (multiplicand p) 
   (caddr (find-operator-position p))
)

; exponentiation?
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
; base
(define (base e) (cadr e))
; exponenxt
(define (exponent e) (caddr e))
; make-exponetiation
; x^0 = 1, x^1 = x
(define (make-exponentiation b e)
    (cond
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))
    )
)

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum 
                (deriv (addend exp) var)
                (deriv (augend exp) var)
            )
        )
        ((product? exp)
            (make-sum
                (make-product 
                    (multiplier exp)
                    (deriv (multiplicand exp) var)
                )
                (make-product 
                    (deriv (multiplier exp) var)
                    (multiplicand exp)
                )
            )
        )
        (else
            (error "unknown expression type: DERIV" exp))
    )
)

;(display (deriv '(** x 3) 'x))
;(newline)
;(display (deriv '(** x 2) 'x))
;(newline)
;(display (deriv '(* x y (+ x 3)) 'x))
(display (find-operator-position '(x * y + z)))
(newline)
(display (caddr (find-operator-position '(x + y + z))))
(newline)
(display (cadr '(x * y + z)))
(newline)
(display (deriv '(x + 3 * y) 'x))

