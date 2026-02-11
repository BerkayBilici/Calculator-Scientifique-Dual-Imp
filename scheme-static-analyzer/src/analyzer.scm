(define get-definitions (lambda (p)
    (car p)
))

(define get-calculations (lambda (p)
    (cadr p)
))

(define get-func-name (lambda (f)
    (car f)
))

(define get-parameters(lambda (f)
    (cadr f)
))

(define get-body (lambda (f)
    (caddr f)
))

(define get-defined-func-names (lambda (d)
    (map get-func-name d)
))

(define is-in-list? (lambda (val lst)
    (cond
        ((null? lst) #f)
        ((eq? val (car lst)) #t)
        (else (is-in-list? val (cdr lst)))
    )
))

(define is-operator? (lambda (op)
    (is-in-list? op '(^ = + - * /))
))

(define check-redefined-recursive (lambda(names)
    (cond
        ((null? names) '())
        ((is-in-list? (car names) (cdr names)) 
            (cons (car names) (check-redefined-recursive(cdr names)))
        )
        (else (check-redefined-recursive(cdr names)))


    )
))

(define find-redefined-functions (lambda (p) ;;func 1 completed.
    (check-redefined-recursive(get-defined-func-names(get-definitions p)))
))

(define check-body-recursive (lambda(params body)
    (cond 
        ((null? body) '())
        ((number? body) '())
        ((is-operator? body) '())
        ((symbol? body)
            (if (is-in-list? body params)
                '()
                (list body)
            )
        )
        ((list? body)
            (apply append (map (lambda (sub-element) 
                                 (check-body-recursive params sub-element)) 
                               body))
        )

        (else '())
    )
))

(define find-undefined-parameters (lambda (p)
    (let ((lst (get-definitions p)))
        (apply append (map (lambda (def)
                             (let ((params (get-parameters def))
                                   (body (get-body def)))
                               (check-body-recursive params body ))) 
                           lst))
    )
))

(define get-arity-table (lambda (d)
    (if (null? d)
        '()  
        (cons 
            (cons (get-func-name (car d)) 
                  (length (get-parameters (car d))))
            (get-arity-table (cdr d)) 
        )
    )
))

(define get-expected-arity (lambda (name lst)
    (cond
        ((null? lst) #f)
        ((eq? name (caar lst))
            (cdar lst))
        (else (get-expected-arity name (cdr lst)))
    )
))

(define check-arity-recursive (lambda (expr lst)
    (cond
        ((or (null? expr) (not (list? expr))) '())
        (else
            (append
                (if
                    (and 
                        (symbol? (car expr))
                        (not(is-operator? (car expr)))
                        (get-expected-arity (car expr) lst)
                        (not (= (length (cdr expr)) (get-expected-arity (car expr) lst)))
                    )
                    (list (car expr))
                    '()
                )

                (check-arity-recursive (car expr) lst)
                (check-arity-recursive (cdr expr) lst)
            )
        )
    )
))

(define find-arity-contradictions (lambda (p)
    (check-arity-recursive
        (get-calculations p)
        (get-arity-table (get-definitions p))
    )   
))

(define check-missing-names-list (lambda (lst)
    (cond
        ((null? lst) '())
        (else
            (append
                (check-missing-names-recursive (car lst))
                (check-missing-names-list (cdr lst))
            )
        )
    )
))

(define traverse-calculations-missing (lambda (c)
    (cond
        ((null?  c) '())
        (else
            (append
                (check-missing-names-recursive (car c))
                (traverse-calculations-missing (cdr c))
            )
        )
    )
))

(define check-missing-names-recursive (lambda (expr)
    (cond
        ((or (null? expr) (not (list? expr)))'())
        (else
            (append
                (if
                    (list? (car expr))
                    (list expr)
                    '()
                )
                (check-missing-names-recursive (car expr))
                (check-missing-names-list (cdr expr))
            )
        )
    )   
))

(define find-missing-function-names (lambda (p)
    (traverse-calculations-missing (get-calculations p))
))

(define check-list-undefined (lambda (lst defs)
    (cond
        ((null? lst) '())
        (else
            (append
                (check-undefined-recursive (car lst) defs)
                (check-list-undefined (cdr lst) defs)
            )
        )
    )
))

(define check-undefined-recursive (lambda(c lst)
    (cond
        ((null? c) '())
        ((not(list? c )) '())
        (else
            (append
                (if (list? (car c))
                    (check-undefined-recursive (car c) lst)
                    
                    (if (and (symbol? (car c))
                             (not (is-operator? (car c)))
                             (not (equal? (car c) 'calculate))
                             (not (is-in-list? (car c) lst)))
                        (list (car c))
                        '()
                    )
                )
                
                (check-list-undefined (cdr c) lst)
            )
        )
    )
))

(define traverse-calculations-undefined (lambda (calcs lst)
    (cond
        ((null? calcs) '())
        (else
            (append
                (check-undefined-recursive (car calcs) lst)
                (traverse-calculations-undefined (cdr calcs) lst)
            )
        )
    )
))

(define find-undefined-functions (lambda (p)
    (traverse-calculations-undefined 
            (get-calculations p) 
            (get-defined-func-names (get-definitions p))
    )
))