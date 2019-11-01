#lang racket
;; Project 1 -- CIS 700
;; Implementing an ANF-style lambda calculus
;; 
;; Kris Micinski, Syracuse University, Fall 2019.  Please send
;; corrections and comments to kkmicins@syr.edu.
;; 
;; DESCRIPTION: 
;;
;; For this project, you will implement a CEK-style interpreter for
;; the ANF-converted lambda calculus.
;;
;; This project has three parts:
;; 
;; - Part 1. Complete each section that is marked TODO. None of these
;; should take more than a few lines of code, so feel free to write
;; email if you find yourself getting lost.
;; 
;; - Part 2. Extend the language with one of the following forms:
;; `letrec`, `if`, or `call/cc`. Of these, `if` is fairly easy,
;; `letrec` is not too much harder (but you may have to email me to
;; talk about it) and `call/cc` is a bit trickier (requires extending
;; the kinds of continuations and then defining how to apply a
;; continuation). If you want to get a lot out of the project, I would
;; recommend you implement all three, but only one is required (so if
;; you don't want to do as much work, just implement `if`). As you
;; extend the language, you should ensure that you extend the
;; predicates defined below. 
;; 
;; Make sure you make life easy on yourself by assuming that the
;; language has been ANF converted. For example, assume you don't have
;; to handle something like `(if (let ([x ((lambda (x) x) #t)]) x) 1
;; 2), feel free to require the language to be in ANF.
;; 
;; - Part 3. This part is optional. I have a few questions below. You
;; can answer these in comments if you would like.
;; 
;; By the way, please see if you can figure out how to use the
;; `grapify` function. It will produce output which can be visualized
;; using graphviz. If you don't want to set up graphviz, there are a
;; few graphviz visualizers online:
;; 
;;     https://dreampuf.github.io/GraphvizOnline

;; ANF Lambda calculus
;; Definition of core language

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
    [`(let ([,x ,(? expr? e)]) ,(? expr? let-body)) #t]
    ;; Returns
    [(? aexpr? ae) #t]
    [else #f]))

;; Literals
(define (lit? l)
  (or (number? l) (boolean? l)))

;; ρ ∈ Env, Environments
(define (env? e)
  (and (andmap (lambda (key) (symbol? key)) (hash-keys e))
       (andmap value? (hash-values e))))

;; Values are either literals or closures
(define (value? v)
  (match v
    [(? lit?) #t]
    [`(clo (lambda (,x) ,(? expr?)) ,(? env?)) #t]))

;; Continuations forms
(define (kont? k)
  (match k
    ['mt #t]
    [`(let ,(? symbol? x) ,(? env? ρ) ,(? expr? let-body) ,(? kont? k)) #t]))

;; Definition of ς ∈ Σ ::= ⟨ C, E, K ⟩
(define (state? state)
  (match state
    [`(,(? expr?) ,(? env?) ,(? kont?)) #t]
    [else #f]))

;; Create a CEK state from a term e
;; 
;; TODO: You should specify how to construct a sensible initial state
;; for the machine.
(define (inject e)
  'TODO)

;; Examples
(define id-id '(let ([x (lambda (x) x)]) (x x)))
(define omega '(let ([x (lambda (x) (x x))]) (x x)))
(define id-id-id-id '(let ([x (lambda (x) x)]) (let ([y (x x)]) (y y))))

;; TODO: You should define the atomic expression evaluator. This is
;; AEval(ae,ρ) : AExpr × Env → Value.
(define (aeval ae ρ)
  (match ae
    [(? number? n) 'todo]
    [(? boolean? b) 'todo]
    [(? symbol? x) 'todo]
    [`(lambda (,x) ,e) 'todo]))

;; Step relation: Σ → Σ
;; TODO: Complete the implementation for each case.
(define (step state)
  (displayln "hasdf")
  (pretty-print state)
  (match state
    ;; Handle a let: step into `e` while remembering (via the `let`
    ;; continuation) to go back and bind x within ρ before proceeding
    ;; to body and continuation k.
    [`((let ([,x ,e]) ,body) ,ρ ,k)
     'todo]

    ;; Returns. You should specify how to handle when an atomic
    ;; expression is in return position. Because this is an A-Normal
    ;; Form language, the only return position will be the `let`
    ;; continuation.
    [`(,(? aexpr? ae) ,ρ (let ,x ,ρ-prime ,e ,k))
     'todo]
    
    ;; Application. Each is an atomic expression. Assume that ae0 will
    ;; evaluate to a closure. This means that `(aeval ae0 ρ) will
    ;; evaluate to something that matches something like `(clo (lambda
    ;; (,xs ...) ,e-body) ,ρ-prime).
    [`((,ae0 ,aes ...) ,ρ ,k)
     'todo]))

;; TODO: Is this state at a done configuration?
(define (done? state)
  #t)

;; Iterate the state to a final answer, build up a transition graph.
(define (iterate state)
  (define (h state state-graph last-state)
    (if (done? state)
        (begin (displayln "Done!") (pretty-print state) state-graph)
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

;; Part 3:
;; 
;; Answer the following questions in as much detail as you think is
;; helpful to you understanding. These questions are meant to address
;; broad knowledge about the project and its relation to other
;; material in the course. Please try to give precise examples in your
;; explanations.
;; 
;; - Compared to Continuation-Passing Style, A-Normal Form allows us
;; just *one* continuation, a `let` continuation. Why might this be
;; desirable, in terms of using CPS vs. ANF as an intermediate
;; language for compilation or analysis of (say) Scheme? Hint: there's
;; no single correct answer here, but when you code things in CPS
;; every call is a tail call. This is a little silly. Think about CPS
;; as an intermediate language versus, say, basic-block-based
;; representations from C compilers or disassemblers. Viewed through
;; that lens, CPS is roughly analogous to having every instruction
;; labeled and manually go to the next one, which defeats the entire
;; point of having basic blocks.
;; 
;; - Sketch out, either via modifying the predicates above or in
;; ASCII/Unicode-based math how the machine would look if it were to
;; handle the CPS lambda calculus.
;; 
;; - If you were to convert the language to CPS, your interpreter
;; rules would change. How would they change? Hint: think about
;; whether you truly need the continuation in that case.
;; 
;; - Elaborate upon the following observation with a few examples: in
;; CPS, every expression is a tail-call, while in ANF every expression
;; is either a tail call or a return.
