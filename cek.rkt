;; Project 1 -- CIS 700
;; Implementing an ANF-style lambda calculus
;; 
;; For this project, you will implement a CEK-style interpreter for
;; the 
#lang racket
;; CEK Machine

(require "church.rkt")

;; Atomic expressions
(define (aexpr? ae)
  (match ae
    ;; Variables
    [(? symbol? x) #t]
    ;; Literals
    [(? number? n) #t]
    ;; Booleans
    [(? boolean? b) #t]
    ;; Lambdas
    [`(lambda (,(? symbol? xs) ...) ,(? expr?)) #t]
    [else #f]))

;; Expression language
(define (expr? e)
  (match e
    ;; Function calls
    [`(,(? aexpr? ae0) ,(? aexpr? aes) ...) #t]
    ;; Let forms
    [`(let ([x ,(? expr? e)]) ,(? expr? let-body)) #t]
    ;; Returns
    [(? aexpr? ae) #t]
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
    [`(let ,(? symbol? x) ,(? expr? let-body)) #t]))

;; A state is C, E, K
(define (state? state)
  (match state
    [`(,(? expr?) ,(? env?) ,(? kont?)) #t]
    [else #f]))

;; Create a CEK state from e
(define (inject e)
  `(,e ,(hash) 'mt))

;; Examples
(define id-id '(let ([x (lambda (x) x)]) (x x)))
(define omega '(let ([x (lambda (x) (x x))]) (x x)))
(define id-id-id-id '(let ([x (lambda (x) x)]) (let ([y x x]) (y y))))

(define (aeval ae ρ)
  (match ae
    [(? number? n) n]
    [(? boolean? b) b]
    [(? symbol? x) (hash-ref ρ x)]
    [`(lambda (,x) ,e) `(clo ,ae ,ρ)]))

;; Step relation
(define (step state)
  (match state
    ;; Returns
    [`(,(? aexpr? ae) ,ρ (let ,x ,ρ-prime ,e ,k))
     `(,e ,(hash-set ρ-prime x (aeval ae ρ)) ,k)]
    
    ;; Application
    [`((,ae0 ,aes ...) ,ρ ,k)
     (match (aeval ae0 ρ)
       [`(clo (lambda (,xs ...) ,e-body) ,ρ-prime)
        (let ([new-ρ (foldl (lambda (x v ρ) (hash-set ρ x v)) ρ-prime xs (map (lambda (ae) (aeval ae ρ))))])
          `(e-body ,new-ρ ,k))])]

    ;; Lambdas...
    ))

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

;; Render h as a DOT graph
(define (graphify h)
  (define lines
    (flatten (hash-map h (lambda (key value) (map (lambda (v) (format "\"~a\" -> \"~a\"" key v)) (set->list value))))))
  (displayln "digraph {")
  (for ([line lines])
    (displayln (format "  ~a" line)))
  (displayln "}"))

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
        (graphify (iterate (inject input)))
        (displayln "Input expression does not satisfy expr?"))
    (repl)))

;; Top level entry point to the program
(repl)
