(define (lookup given-key set-of-records)
    (cond 
        ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
                (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))
    )
)

(define (lookup-binary-tree given-key set-of-records)
    (cond
        ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
                (entry set-of-records))
        (
            (< given-key (key (entry set-of-records)))
            (lookup-binary-tree given-key (left-branch set-of-records))
        )
        (
            (> given-key (key (entry set-of-records)))
            (lookup-binary-tree given-key (right-branch set-of-records))
        )
    )
)