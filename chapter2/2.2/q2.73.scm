; get and put
(define *op-table* (list '*table*))

(define (put op type item)
  (let ((op-entry (assoc op (cdr *op-table*))))
    (if op-entry
        (let ((type-entry (assoc type (cdr op-entry))))
          (if type-entry
              (set-cdr! type-entry item)
              (set-cdr! op-entry (cons (cons type item) (cdr op-entry)))))
        (set-cdr! *op-table*
                  (cons (list op (cons type item)) (cdr *op-table*)))))
  'ok)

(define (get op type)
  (let ((op-entry (assoc op (cdr *op-table*))))
    (if op-entry
        (let ((type-entry (assoc type (cdr op-entry))))
          (if type-entry
              (cdr type-entry)
              #f))
        #f)))

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

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

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
        ((exponentiation? exp)
            (make-product
                (exponent exp)
                (make-product
                    (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                    (deriv (base exp) var)
                )
            )
        )
        (else
            (error "unknown expression type: DERIV" exp))
    )
)

(define (install-deriv-package)
    (define (addend s) (car s))
    (define (augend s) (cadr s))

    (define (multiplier p) (car p))

    (define (multiplicand p) (cadr p))

    ; base
    (define (base e) (car e))
    ; exponenxt
    (define (exponent e) (cadr e))

    (define (deriv-sum ops var)
        (make-sum
            (deriv-data-driven (addend ops) var)
            (deriv-data-driven (augend ops) var)
        )
    )

    (define (deriv-product ops var)
        (make-sum
            (make-product
                (multiplier ops)
                (deriv-data-driven (multiplicand ops) var) 
            )
            (make-product
                (deriv-data-driven (multiplier ops) var)
                (multiplicand ops)
            )
        )
    )

    (define (deriv-exponentiation ops var)
        (make-product
            (exponent ops)
            (make-product
                (make-exponentiation (base ops) (make-sum (exponent ops) -1))
                (deriv-data-driven (base ops) var)
            )
        )
    )

    (put 'deriv '+ deriv-sum)
    (put 'deriv '* deriv-product)
    (put 'deriv '** deriv-exponentiation)
'done)

(install-deriv-package)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv-data-driven exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else 
            (
                (get 'deriv (operator exp))
                (operands exp) 
                var
            )
        )
    )
)

;(display (deriv '(** x 3) 'x))
;(newline)
;(display (deriv '(** x 2) 'x))
;(newline)
;
;
;(newline)
;(display (number? 3))
;(newline)
;(display (variable? '(** x 2)))

(newline)
(display (deriv-data-driven '(** x 2) 'x))
(newline)
(display (deriv-data-driven '(* x y) 'x))
(newline)
(display (deriv-data-driven '(* (** x 2) y) 'x))
(newline)
(display (deriv-data-driven '(+ x 3) 'x))
(newline)
(display (deriv-data-driven '(+ (** x 2) (* x y)) 'x))


; a
;  -> 1: what was done above -> (get 'deriv (operator exp)) using the date driven approach mentioned in section 2.4.3 which will attach procedures in tabular form, 
;       so operator of the expression and 'deriv can be mapped to different procedures of deriv based on different operators (e.g. ('deriv, +) set will only be mapped to the sum part of the deriv procedure from our older implementation)
; -> 2: why can't number? and variable? be part of data directed dispatch -> this is because if the given expression is either a number or a symbol and not a pair, it will throw a type error on calling (operator exp) unable to find it's type tag,
;       to look into the table and find it procedure, so it's added as a early check to avoid crashing the procedure


; d -> to make the retrieval of procedure type first ((get (operator exp) 'deriv) (operands exp) var),
;      it makes sense to have separate packages based on separate type (sum-package, product-package and so on...)
;      that way later more operations can be added specific to that type