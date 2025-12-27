(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b))))


; applicative order evaluation (no of remainder calls)
; (gcd 206 40)
; (gcd 40 (remainder 206 40)) -> 1
; (gcd 40 (remainder 206 40)) -> 1
; (gcd 40 6)) -> 1
; (gcd 6 (remainder 40 6)) -> 2
; (gcd 6 4) -> 2
; (gcd 4 (remainder 6 4)) -> 3
; (gcd 4 2) -> 3
; (gcd 2 (remainder 4 2)) -> 4
; (gcd 2 0) -> 4
; 2 -> 4

; normal order evaluation (no of remainder calls)
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; if (= (remainder 260 40) 0) -> 1
; if (= 6 0) -> 1
; (gcd (remainder 206 40) (remainder 40 (remainder 260 40)))
; (if (= (remainder 40 (remainder 260 40)))) -> 3
; (if (= (remainder 40 6))) -> 3
; (if (= 4 0)) -> 3
; (gcd 
;       (remainder 40 (remainder 260 40)) 
;       (remainder (remainder 206 40) (remainder 40 (remainder 260 40)))
; )
; (if (remainder (remainder 206 40) (remainder 40 (remainder 260 40)))) -> 7
; (if (remainder 6 4)) -> 7
; (if 2) -> 7
; (gcd 
;       (remainder (remainder 206 40) (remainder 40 (remainder 260 40))) 
;       (remainder (remainder 40 (remainder 260 40)) (remainder (remainder 206 40) (remainder 40 (remainder 260 40))))
; )

; if (remainder (remainder 40 (remainder 260 40)) (remainder (remainder 206 40) (remainder 40 (remainder 260 40)))) -> 14
; if (remainder 4 2) -> 14
; if 0 -> 14

; (remainder (remainder 206 40) (remainder 40 (remainder 260 40)))
; (remainder 6 4)
; 2
; 18 times remainder getting called by normal order evaluation