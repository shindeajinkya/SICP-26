(define (fold-left op initial sequence)
	(define (iter result rest)
		(if 
			(null? rest)
			result
			(iter 
				(op result (car rest))
				(cdr rest)
			)
		)
	)
	(iter initial sequence)
)

; operations with commutative and associative property produce same result for fold-right and fold-left, 
; (for e.g. addition(+) and multiplication(*) will produce same results, no matter order or fold right, left being used)

(display (fold-right / 1 (list 1 2 3))); 3/2
(newline)
(display (fold-left / 1 (list 1 2 3))); 1/6
(newline)
(display (fold-right list (list) (list 1 2 3))); (1 (2 (3 ())))
(newline)
(display (fold-left list (list) (list 1 2 3))); (((() 1) 2) 3)