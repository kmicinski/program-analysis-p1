;; CEK Machine
#lang racket

;; Expression language
(define (expr? e)
  (match e
    [(? symbol?) #t]
    [(? lit?) #t]
    [`(,(? expr?) ,(? expr?)) #t]
    [`(lambda (,x) ,(? expr?)) #t]
    [`(prim ,(? expr?) ,(? expr?)) #t]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [else #f]))

(define (lit? l)
  (or (number? l) (boolean? l)))

(define (env? e)
  (and (andmap (lambda (key) (symbol? key)) (hash-keys e))
       (andmap value? (hash-values e))))

(define (value? v)
  (match v
    [(? lit?) #t]
    [`(clo (lambda (,x) ,(? expr?)) ,(? env?)) #t]))

(define (kont? k)
  (match k
    ['mt #t]
    [`(ar ,(? expr?) ,(? env?) ,(? kont?)) #t]
    [`(fn (lambda (,x) ,(? expr?)) ,(? env?) ,(? kont?)) #t]
    [`(prim-rhs ,prim ,(? expr?)) #t]
    [`(apply-prim ,prim ,(? value?)) #t]
    ;; if...
    ))

(define (cesk*-state? state)
  (match state
    [`(,(? expr?) ,(? env?) ,(? kont?)) #t]
    [else #f]))

;; Create a CESK* state from e
(define (inject e)
  `(,e ,(hash) 'mt))

;; Examples
(define id-id '((lambda (x) x) (lambda (x) x)))
(define omega '((lambda (x) (x x)) (lambda (x) (x x))))

;; Step relation
(define (step state)
  (match state
    ;; Variable lookup
    [`(,(? symbol? x) ,env ,sto ,a) 
     (match (hash-ref sto (hash-ref env x))
       [`(clo (lambda (,x) ,body) ,rho-prime)
        `((lambda (,x) ,body) ,rho-prime ,sto ,a)])]
    ;; Application
    [`((,e0 ,e1) ,ρ ,σ ,a)
     (let* ([b (fresh-addr)]
            [new-k `(ar ,e1 ,ρ ,a)]
            [new-σ (hash-set σ b new-k)])
       `(,e0 ,ρ ,new-σ ,b))]
    ;; Lambdas...
    [`(,v ,ρ ,σ ,a)
     (let ([k (hash-ref σ a)]
           [b (fresh-addr)])
       (match k
         [`(ar ,e ,ρ1 ,c)
          `(,e ,ρ1 ,(hash-set σ b `(fn ,v ,ρ ,c)) ,b)]
         [`(fn (lambda (,x) ,e) ,ρ1 ,c)
          `(,e ,(hash-set ρ1 x b) ,(hash-set σ b `(clo ,v ,ρ)) ,c)]
         [else state]))]))

(define (iterate state)
  (displayln "Iterating state...")
  (pretty-print state)
  (let ([next-state (step state)])
    (if (equal? next-state state)
        ;; Done
        (displayln "Done w/ evaluation.")
        (iterate next-state))))

(define (repl)
  (displayln "Type an expression...")
  (display "> ")
  (let ([input (read)])
    ;; Execute the expression
    (iterate (inject input))
    (repl)))

(repl)
