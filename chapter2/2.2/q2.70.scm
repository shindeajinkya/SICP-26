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

(define (adjoin-set x set)
    (cond 
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else 
            (cons 
                (car set)
                (adjoin-set x (cdr set))
            )
        )
    )
)

(define (make-leaf-set pairs)
    (if 
        (null? pairs)
        '()
        (let 
            ((pair (car pairs)))
            (adjoin-set 
                (make-leaf 
                    (car pair) ; symbol
                    (cadr pair)
                ) ; frequency
                (make-leaf-set (cdr pairs))
            )
        )
    )
)

(define (successive-merge leaves)
    (if
        (< (length leaves) 2) 
        (car leaves)
        (successive-merge 
            (adjoin-set
                (make-code-tree
                    (car leaves)
                    (cadr leaves)
                )
                (cdr (cdr leaves))
            )
        )
    )
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
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

(define song-tree 
    (generate-huffman-tree 
        '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 1) (NA 16) (YIP 9))
    )
)

(display
    (length (encode
        '(
            Get a job
            Sha na na na na na na na na
            Get a job
            Sha na na na na na na na na
            Wah yip yip yip yip yip yip yip yip yip
            Sha boom
        )
        song-tree
    ))
) ; 84 bits, for fixed length it would require 108 bits (36 * 3)
