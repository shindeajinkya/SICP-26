(define (reverse-right sequence)
	(fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse-left sequence)
	(fold-left (lambda (x y) (cons y x)) (list) sequence))

(display (reverse-left (list 1 4 9 16 25)))
(newline)
