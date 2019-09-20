#lang racket
;; CEK Machine

(require "church.rkt")

;; Expression language
(define (expr? e)
  (match e
    [(? symbol?) #t]
    [`(,(? expr?) ,(? expr?)) #t]
    [`(lambda (,x) ,(? expr?)) #t]
    [else #f]))

;; Literals
(define (lit? l)
  (or (number? l) (boolean? l)))

;; Environments
(define (env? e)
  (and (andmap (lambda (key) (symbol? key)) (hash-keys e))
       (andmap value? (hash-values e))))

;; Values are either literals or closures
(define (value? v)
  (match v
    [(? lit?) #t]
    [`(clo (lambda (,x) ,(? expr?)) ,(? env?)) #t]))

;; Continuation forms
(define (kont? k)
  (match k
    ['mt #t]
    [`(ar ,(? expr?) ,(? env?) ,(? kont?)) #t]
    [`(fn (lambda (,x) ,(? expr?)) ,(? env?) ,(? kont?)) #t]))

;; A state is C, E, K
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
(define id-id-id-id '(((lambda (x) x) (lambda (x) x)) ((lambda (x) x) (lambda (x) x))))

;; Step relation
(define (step state)
  (match state
    ;; Variable lookup
    [`(,(? symbol? x) ,env ,k) 
     (match (hash-ref env x)
       [`(clo (lambda (,x) ,body) ,rho-prime)
        `((lambda (,x) ,body) ,rho-prime ,k)])]
    ;; Application
    [`((,e0 ,e1) ,ρ ,k)
     (let* ([new-k `(ar ,e1 ,ρ ,k)])
       `(,e0 ,ρ ,new-k))]
    ;; Lambdas...
    [`((lambda (,x) ,e-body) ,ρ ,k)
     (match k
       [`(ar ,e ,ρ1 ,k1)
        `(,e ,ρ1 (fn (lambda (,x) ,e-body) ,ρ ,k1))]
       [`(fn (lambda (,x1) ,e1) ,ρ1 ,k1)
        `(,e1 ,(hash-set ρ1 x1 `(clo (lambda (,x) ,e-body) ,ρ)) ,k1)])]))

;; Is this state at a done configuration
(define (done? state)
  (match state
    [`((lambda (,x) ,e) ,ρ 'mt) #t]
    [else #f]))

;; Iterate the state to a final answer, build up a transition graph.
(define (iterate state)
  (define (h state state-graph last-state)
    (if (done? state)
        (begin (displayln "done!") (pretty-print state) state-graph)
        (let* ([next-state (step state)]
               ;; Add to the state graph by adding an edge between the
               ;; last state and this state.
               [next-state-graph
                (if (null? last-state)
                    (hash-set state-graph next-state (set))
                    (hash-set state-graph
                              last-state
                              (set-add (hash-ref state-graph last-state (set)) next-state)))])
          (displayln (format "--> ~a" (pretty-format state)))
          (h next-state next-state-graph next-state))))
  (h state (hash) null))

;;
;; REPL
;;

;; Run a REPL
(define (repl)
  (displayln "Type an expression...")
  (display "> ")
  (let ([input (read)])
    (if (expr? input)
        ;; Execute the expression
        (pretty-print (iterate (inject input)))
        (displayln "Input expression does not satisfy expr?"))
    (repl)))

;; Top level entry point to the program
(repl)
