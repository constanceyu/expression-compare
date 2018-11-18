(define (expr-compare x y)
    (cond
        [(equal? x y) x]
        [(and (eq? x #t) (eq? y #f)) (quote %)]
        [(and (eq? x #f) (eq? y #t)) (quote (not %))]
        [(or (not (and (list? x) (list? y)))
         (not (= (length x) (length y)))
         (or (equal? (car x) 'quote) (equal? (car y) 'quote))
         (and (not (equal? (car x) 'if)) (equal? (car y) 'if))
         (and (equal? (car x) 'if) (not (equal? (car y) 'if))))
            (list 'if '% x y)]

        ; handle 'let
        ; if only one expr has let, stop recur
        [(or (and (not (equal? (car x) 'let)) (equal? (car y) 'let))
         (and (equal? (car x) 'let) (not (equal? (car y) 'let))))
            (list 'if '% x y)]
        ; [(and (equal? (car x) 'let) (equal? (car y) 'let))
        ;  (if (= (length (cadr x)) (length (cadr y)))
        ;      (let ((bind (check-let (cadr x) (cadr y) '() '() '())))
        ;         (display bind)
        ;         (cons 'let (expr-compare 
        ;             (cons 
        ;                 (replace-first (cadr x) (car bind) (caddr bind))
        ;                 (replace (cddr x) (cddr y) (car bind) (caddr bind))
        ;             )
        ;             (cons 
        ;                 (replace-first (cadr y) (cadr bind) (caddr bind))            
        ;                 (replace (cddr y) (cddr x) (cadr bind) (caddr bind))
        ;             )
        ;         ))
        ;     )
        ;     (list 'if '% x y)
        ; )]
        [(and (equal? (car x) 'let) (equal? (car y) 'let))
         (if (= (length (cadr x)) (length (cadr y)))
             (let ((bind (check-let (cadr x) (cadr y) '() '() '())))
                (display bind)
                (cons 'let (expr-compare 
                    (cons 
                        (replace-first (cadr x) (car bind) (caddr bind))
                        (replace (cddr x) (car bind) (caddr bind))
                    )
                    (cons 
                        (replace-first (cadr y) (cadr bind) (caddr bind))            
                        (replace (cddr y) (cadr bind) (caddr bind))
                    )
                ))
            )
            (list 'if '% x y)
        )]

        ; handle 'lambda
        ; if only one expr has lambda, stop recur
        [(or (and (not (equal? (car x) 'lambda)) (equal? (car y) 'lambda))
         (and (equal? (car x) 'lambda) (not (equal? (car y) 'lambda))))
            (list 'if '% x y)]
        ; lambda directly replaces all instances
        [(and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
         (if (= (length (cadr x)) (length (cadr y)))
            (let ((bind (check-lambda (cadr x) (cadr y) '() '() '()))) 
                (cons 'lambda
                    (expr-compare
                        (replace (cdr x) (car bind) (caddr bind)) 
                        (replace (cdr y) (cadr bind) (caddr bind))
                    )
                )
            )
            (list 'if '% x y)
        )]

        [else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
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
        [else (index-of-rec (cdr l) n (+ 1 i))]
    )
)

; bind x and y -> x!y
(define (bind x y)
    (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

; args1 -> args in x
; args2 -> args in y
; lx -> list of vars in x
; ly -> list of vars in y
; lxy -> list of vars representing x!y
(define (check-let args1 args2 lx ly lxy)
    (cond 
        [(and (empty? args1) (empty? args2)) (list lx ly lxy)]
        [(not (equal? (caar args1) (caar args2)))
            (check-let (cdr args1) (cdr args2)  (cons (caar args1) lx) (cons (caar args2) ly) (cons (bind (caar args1) (caar args2)) lxy))]
        [else (check-let (cdr args1) (cdr args2) lx ly lxy)]
    )
)

; here x y are the args in lambda
; same arguments as check-let (see above)
(define (check-lambda args1 args2 lx ly lxy)
    (cond
        [(and (empty? args1) (empty? args2)) (list lx ly lxy)]
        [(not (equal? (car args1) (car args2)))
            (check-lambda (cdr args1) (cdr args2) (cons (car args1) lx) (cons (car args2) ly) (cons (bind (car args1) (car args2)) lxy))]
        [else (check-lambda (cdr args1) (cdr args2) lx ly lxy)]
    )
)

(define (replace expr ele lxy)
    ; (display "expr: ")
    ; (display expr)
    ; (display "expr1: ")
    ; (display expr1)
    ; (display "\n")
    (cond
        [(empty? expr) expr]
        [(list? (car expr)) (cons (replace (car expr) ele lxy) (replace (cdr expr) ele lxy))]
        [(member (car expr) ele)
            (cons (let ((i (index-of ele (car expr)))) (list-ref lxy i)) (replace (cdr expr) ele lxy))]
        [else (cons (car expr) (replace (cdr expr) ele lxy))]
    )
)


(define (replace-first bind ele lxy)
    (cond 
        [(empty? bind) bind]
        [(member (caar bind) ele)
            (cons (let ((i (index-of ele (caar bind) ))) (cons (list-ref lxy i) (cdar bind))) (replace-first (cdr bind) ele lxy ))]
        [else (cons (car bind) (replace-first (cdr bind) ele lxy))]
    )
)
