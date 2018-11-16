(define (expr-compare x y)
    (cond
        [(equal? x y) x]
        [(and (eq? x #t) (eq? y #f)) (quote %)]
        [(and (eq? x #f) (eq? y #t)) (quote (not %))]
        [(or (not (and (list? x) (list? y)))
         (not (equal? (length x) (length y)))
         (or (equal? (car x) 'quote) (equal? (car y) 'quote))
         (and (not (equal? (car x) 'if)) (equal? (car y) 'if))
         (and (not (equal? (car y) 'if)) (equal? (car x) 'if))
         (and (not (equal? (car x) 'let)) (equal? (car y) 'let))
         (and (not (equal? (car y) 'let)) (equal? (car x) 'let)))
         (list 'if '% x y)]
        []
        [else #t]
    )
)

; returns t if x is one of the keywords
(define (is-keyword x)
  (member x '(quote lambda let if))
)

; returns the index of n in l
(define (index-of l n)
    (index-of-rec l n 0)
)

; i is the index of current element
(define (index-of-rec l n i)
    (cond
        [(equal? (car l) n) i]
        [else (index-of-rec (cdr l) n (+ i 1))]
    )
)

; bind x and y -> x!y
(define (bind x y)
    (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

