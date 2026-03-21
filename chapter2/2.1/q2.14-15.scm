(define one (lambda (f) (lambda (x) (f x))))

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
    (make-interval 
        (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))
    ))

(define (sub-interval x y)
    (make-interval 
        (- (lower-bound x) (upper-bound y))
        (- (upper-bound x) (lower-bound y))
    ))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
        (error "Division by interval spanning zero")
        (mul-interval
            x
            (make-interval 
                (/ 1.0 (upper-bound y))
                (/ 1.0 (lower-bound y))
        ))
    ))

(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
	(/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage-to-value n p) (* n (/ p 100)))

(define (make-center-percent c p)
	(make-center-width c (percentage-to-value c p)))

(define (percent interval)
	(abs (
		*
		(/ (width interval) (center interval))
		100
	)))

(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2)
	(add-interval r1 r2)))

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval
			one (add-interval (div-interval one r1)
			(div-interval one r2)))))

(define interval1 (make-interval 3.35 3.65))
(define interval2 (make-interval 4.0 4.28))

(define interval-with-percent1 (make-center-percent 3.5 0.01))
(define interval-with-percent2 (make-center-percent 4.14 0.01))

(display (par1 interval1 interval2)) ; (1.6897856242118539 . 2.1254421768707483)
(newline)
(display (par2 interval1 interval2)) ; (1.8231292517006803 . 1.9699873896595208)
(newline)

(display (par1 interval-with-percent1 interval-with-percent2)) ; (1.888370482807187 . 1.904855033730706)
(newline)
(display (par2 interval-with-percent1 interval-with-percent2)) ; (1.8938388446174927 . 1.8993548632435355)
(newline)

; q1.15
(display (percent (par1 interval-with-percent1 interval-with-percent2))) ; 
(newline)
(display (percent (par2 interval-with-percent1 interval-with-percent2))) ;
(newline)

; in par1 both intervals appear twice while computing and in par 2 it appears only once computing better and accurate output than par1
