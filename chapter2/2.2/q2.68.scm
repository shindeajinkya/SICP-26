(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if 
        (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if 
        (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)

(define (make-code-tree left right)
    (list 
        left
        right
        (append 
            (symbols left) 
            (symbols right)
        )
        (+ (weight left) (weight right))
    )
)

(define (choose-branch bit branch)
    (cond 
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))
    )
)

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let 
                ((next-branch
                    (choose-branch (car bits) current-branch))
                )
                (if 
                    (leaf? next-branch)
                    (cons 
                        (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree)
                    )
                    (decode-1 (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-1 bits tree)
)

(define (encode-symbol symbol tree)
    (define (encode-acc symbol tree result)
        (cond
            ((leaf? tree) result)
            (
                (memq symbol (symbols (left-branch tree)))
                (encode-acc symbol (left-branch tree) (append result '(0)))
            )
            (
                (memq symbol (symbols (right-branch tree)))
                (encode-acc symbol (right-branch tree) (append result '(1)))
            )
            (else (error "bad symbol" symbol))
        )
    )
    (encode-acc symbol tree '())
)

(define (encode message tree)
    (if (null? message)
        '()
        (append 
            (encode-symbol (car message) tree)
            (encode (cdr message) tree)
        )
    )
)

(define sample-tree
    (make-code-tree 
        (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree
                (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (encode '(a d a b b c a) sample-tree))
(newline)

; (
;   (leaf a 4) 
;   (
;        (leaf b 2) 
;       (
;           (leaf d 1) 
;            (leaf c 1) 
;           (d c) 
;           2
;       ) 
;       (b d c) 
;       4) 
;   (a b d c) 
;   8)