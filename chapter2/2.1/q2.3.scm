(define (make-point x y)
    (cons x y))

(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start end)
    (cons start end))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (point-length point) 
            (abs (
                sqrt 
                (+ 
                    (square (x-point point))
                    (square (y-point point))
                )
            )))

(define (segment-length segment) 
                (point-length 
                    (make-point 
                        (- (x-point (end-segment segment)) (x-point (start-segment segment)))
                        (- (y-point (end-segment segment)) (y-point (start-segment segment)))
                    )
                ))

(define (make-rectangle width-segment height-segment)
    (cons width-segment height-segment))

(define (width rectangle) (segment-length (car rectangle)))
(define (height rectangle) (segment-length (cdr rectangle)))

(define (wishful-make-rectangle width height)
    (cons width height))

(define (wishful-width rectangle) (car rectangle))
(define (wishful-height rectangle) (cdr rectangle))

(define (perimeter rectangle) (* 2 (+ (width rectangle) (height rectangle))))
(define (area rectangle) (* (width rectangle) (height rectangle)))

(define w (make-segment (make-point 0 0) (make-point 3 0)))
(define h (make-segment (make-point 0 0) (make-point 0 4)))
(define r (make-rectangle w h))

(display (perimeter r)) ; should give 14
(newline)
(display (area r))      ; should give 12

; 1. define rectangle using segments
; 2. calculate area
; 3. calculate perimeter