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
        [(and (equal? (car x) 'let) (equal? (car y) 'let))
         (if (= (length (cadr x)) (length (cadr y)))
             (let ((bind (check-let (cadr x) (cadr y) '() '() '())))
                (cons 'let (expr-compare 
                    (cons 
                        (replace-first (cadr x) (car bind) (caddr bind))
                        (replace-all (cddr x) (car bind) (caddr bind))
                    )
                    (cons 
                        (replace-first (cadr y) (cadr bind) (caddr bind))            
                        (replace-all (cddr y) (cadr bind) (caddr bind))
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
                        (replace-all (cdr x) (car bind) (caddr bind)) 
                        (replace-all (cdr y) (cadr bind) (caddr bind))
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

(define (replace-all expr ele lxy)
    (cond
        [(empty? expr) expr]
        [(list? (car expr)) (cons (replace-all (car expr) ele lxy) (replace-all (cdr expr) ele lxy))]
        [(member (car expr) ele)
            (cons (let ((i (index-of ele (car expr)))) (list-ref lxy i)) (replace-all (cdr expr) ele lxy))]
        [else (cons (car expr) (replace-all (cdr expr) ele lxy))]
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

(define (test-expr-compare x y)
    (let ((expr (expr-compare x y)))
        (and
            (equal?
                (eval x)
                (eval (list 'let '((% #t)) expr))
            )
            (equal?
                (eval y)
                (eval (list 'let '((% #f)) expr))
            )
        )
    )
)

(define test-expr-x
    (list
        ; ===== base case =====
        12
        12
        #t
        #t
        #f
        #f
        'a
        'a
        '"Hi"
        '"Hi"
        '(1 2 3)
        '(1 2 3)
        '(+ 1 2)
        '(+ 1 2)
        '(+ 1 2)
        '(+ a b)
        '(+ a b)
        '(- 9 (+ 1 2))
        '(- 9 (+ 1 2))
        '(- 9 (+ 1 2))
        '(+ #f (let ((a 1) (b 2)) (f a b)))
        ; ===== quote =====
        ''(1 2)
        ''(1 2)
        '(quote (1 2))
        '(quote (1 2))
        '(quote (1 2))
        ; ===== lambda =====
        '(lambda (x y) (+ x y))
        '(lambda (x y) (+ x y))
        '(lambda (x y) (+ x y))
        '(lambda (x y) (+ x y))
        '((lambda (a) (f a)) 1)
        ; ===== let =====
        '(let ((x y)) (+ x 1))
        '(let ((x y)) (+ x 1))
        '(let ((x y)) (+ x 1))
        '(let ((x y)) x)
        '(let ((x y)) x)
        '(let ((a 1)) (f a))
        ; ===== conditional =====
        '(if x y z)
        '(if x y z)
        '(if x y z)
        '(if x y z)
        '(if x (if y a b) z)
        '(if x (if y a b) z)
        ; ===== examples =====
        'a
        '(cons a b)
        '(cons a b)
        '(cons (cons a b) (cons b c))
        '(cons a b)
        '(list)
        ''(a b)
        '(quote (a b))
        '(quoth (a b))
    )
)

(define test-expr-y
    (list
        12
        20
        #t
        #f
        #t
        #f
        'a
        'b
        '"Hi"
        '"Hello"
        '(1 2 3)
        '(2 3 1)
        '(+ 1 2)
        '(+ 1 3)
        '(- 1 2)
        '(+ a b)
        '(+ a c)
        '(- 9 (+ 1 2))
        '(+ 9 (- 1 2))
        '(- 8 (+ 1 2))
        '(+ #t (let ((a 1) (c 2)) (f a c)))
        ''(1 2)
        ''(2 1)
        '(quote (1 2))
        '(quote (3 4))
        'hihihi
        '(lambda (x z) (+ x z))
        '(lambda (x y) (* x y))
        '(lambda (z y) (+ x y))
        '(lambda (z y) (+ z y))
        '((lambda (a) (g a)) 2)
        '(let ((x y)) (+ x 1))
        '(let ((x z)) (+ x 1))
        '(let ((z y)) (+ z 1))
        '(let ((x y)) x)
        '(let ((z y)) z)
        '(let ((a 2)) (g a))
        '(if x y z)
        '(if a y z)
        '(if x z y)
        '(if z x y)
        '(if x (if y a b) z)
        '(if x (if a y b) z)
        '(cons a b)
        '(cons a b)
        '(cons a c)
        '(cons (cons a c) (cons a c))
        '(list a b)
        '(list a)
        ''(a c)
        '(quote (a c))
        '(quoth (a c))
    )
)