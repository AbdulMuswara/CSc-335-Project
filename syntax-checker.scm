(define my-car car)
(define my-cdr cdr)
(define my-cadr (lambda (x) (car (cdr x))))
(define my-caddr (lambda (x) (car (cdr (cdr x)))))
(define my-cddr (lambda (x) (cdr (cdr x))))
(define my-pair? pair?)

(define (every pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst)) (every pred (cdr lst)))))

(define primitives '(cons car cdr null? eq? atom? zero? add1 sub1 mul sub1 number? square + - * /))

(define (syntax-checker expr env)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? expr)
     (or (not (not (member expr env)))
         (not (not (member expr primitives)))))
    ((my-pair? expr)
     (case (my-car expr)
       ((quote) (and (my-pair? (my-cdr expr)) (null? (my-cddr expr))))
       ((lambda) (let ((params (my-cadr expr))
                       (body (my-caddr expr)))
                   (and (list? params)
                        (every symbol? params)
                        (syntax-checker body (append params env)))))
       ((cond)
        (let ((clauses (my-cdr expr)))
          (and (not (null? clauses))
               (every (lambda (clause)
                      (and (my-pair? clause)
                      (my-pair? (my-cdr clause))
                      (null? (my-cddr clause))
                      (let ((test (my-car clause))
                            (result (my-cadr clause)))
                        (and
                         (or (eq? test 'else)
                             (syntax-checker test env))
                         (syntax-checker result env)))))
               clauses))))
       (else (let ((fn (my-car expr))
                   (args (my-cdr expr)))
               (and (syntax-checker fn env)
                    (every (lambda (arg) (syntax-checker arg env)) args)
                    (let ((arity (get-arity fn env)))
                      (eq? (length args) arity)))))))
     (else #f)))

(define (get-arity fn env)
  (cond
    ((eq? fn 'cons) 2)
    ((eq? fn 'car) 1)
    ((eq? fn 'cdr) 1)
    ((eq? fn 'null?) 1)
    ((eq? fn 'eq?) 2)
    ((eq? fn 'atom?) 1)
    ((eq? fn 'zero?) 1)
    ((eq? fn 'add1) 1)
    ((eq? fn 'mul) 2)
    ((eq? fn 'sub1) 1)
    ((eq? fn 'number?) 1)
    ((eq? fn 'square) 1)
    ((eq? fn '+) 2)
    ((eq? fn '-) 2)
    ((eq? fn '*) 2)
    ((eq? fn '/) 2)
    (else #f)))

(syntax-checker '(cond (#t 1)) '())           ;;#t
(syntax-checker '(cond (#f 1) (else 2)) '())  ;;#t


(syntax-checker '(cond) '())                    ;;#f
(syntax-checker '(cond (else)) '())             ;;#f
(syntax-checker '(cond (#t x) (else y)) '(x))   ;;#f (unbound y)

(syntax-checker '(lambda(x) y) '())   ;;#f
(syntax-checker '(lambda(x) y) '(y))  ;;#t

(syntax-checker '(car) '())           ;;#f
(syntax-checker '(car '(1 2 3)) '())  ;;#t

(syntax-checker '(cons) '())          ;;#f
(syntax-checker '(cons 'a '(b)) '())  ;;#t

(syntax-checker '(cdr) '())           ;;#f
(syntax-checker '(cdr '(1 2 3)) '())  ;;#t

(syntax-checker '(+ 1) '())           ;;#f
(syntax-checker '(+ 1 1) '())         ;;#t

(syntax-checker '(add1 1 2) '())      ;;#f
(syntax-checker '(add1 1) '())        ;;#t