#lang racket

(require racket/trace)

(provide execute)

;; Function to evaluate numeric expressions
(define (eval-numeric-exp exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((eq? '+ (car exp)) (+ (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? '- (car exp)) (- (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? '* (car exp)) (* (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? '/ (car exp)) (/ (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? 'floor (car exp)) (floor (eval-numeric-exp (cadr exp) env)))
    ((eq? 'cos (car exp)) (cos (eval-numeric-exp (cadr exp) env)))
    ((eq? 'sin (car exp)) (sin (eval-numeric-exp (cadr exp) env)))))
(trace eval-numeric-exp)

;; Function to evaluate boolean expressions
(define (eval-bool-exp exp env)
  (cond
    ((eq? '=' (car exp)) (= (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? '< (car exp)) (< (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))
    ((eq? '> (car exp)) (> (eval-numeric-exp (cadr exp) env) (eval-numeric-exp (caddr exp) env)))))
(trace eval-bool-exp)

;; Function to evaluate expressions
(define (eval-exp exp env)
  (cond
    ((symbol? exp) (lookup exp env))
    ((number? exp) exp)
    ((string? exp) exp)
    ((eq? 'if (car exp)) 
     (if (eval-bool-exp (cadr exp) env) (eval-exp (caddr exp) env) (eval-exp (cadddr exp) env)))
    ((eq? 'when (car exp)) 
     (if (eval-bool-exp (cadr exp) env) 
         (apply string-append (map (lambda (e) (eval-exp e env)) (cddr exp))) ""))
    ((eq? 'circle (car exp)) (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>"
                                      (eval-numeric-exp (cadr exp) env)
                                      (eval-numeric-exp (caddr exp) env)
                                      (eval-numeric-exp (cadddr exp) env)
                                      (eval-exp (car (cddddr exp)) env)))
    ((eq? 'rect (car exp)) (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>"
                                    (eval-numeric-exp (cadr exp) env)
                                    (eval-numeric-exp (caddr exp) env)
                                    (eval-numeric-exp (cadddr exp) env)
                                    (eval-numeric-exp (car (cddddr exp)) env)
                                    (eval-exp (cadr (cddddr exp)) env)))
    ((eq? 'line (car exp)) (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>"
                                    (eval-numeric-exp (cadr exp) env)
                                    (eval-numeric-exp (caddr exp) env)
                                    (eval-numeric-exp (cadddr exp) env)
                                    (eval-numeric-exp (car (cddddr exp)) env)
                                    (eval-exp (cadr (cddddr exp)) env)))
    ((list? exp)
     (let ((proc (car exp))
           (args (cdr exp)))
       (if (eq? proc (caar env))
           (apply string-append (map (lambda (e) (eval-exp e env)) (cadar env)))
           (error "Invalid procedure" proc))))
    (else (error "Invalid expression"))))
(trace eval-exp)

;; Function to lookup values in the environment
(define (lookup id env)
  (define (lookup-in-frame id frame)
    (cond
      ((null? frame) (error "Undefined identifier" id))
      ((eq? (car frame) id) (cadr frame))
      (else (lookup-in-frame id (cdr frame)))))
  (trace lookup-in-frame)
  (define (lookup-in-env id env)
    (cond
      ((null? env) (error "Undefined identifier" id))
      ((lookup-in-frame id (car env)))
      (else (lookup-in-env id (cdr env)))))
  (trace lookup-in-env)
  (lookup-in-env id env))
(trace lookup)

;; Function to execute SVGen program
(define (execute width height prg expr)
  (define (eval-program program env)
    (for-each (lambda (def)
                (define (add-binding id val env)
                  (cons (list id val) (car env)))
                (trace add-binding)
                (define (add-binding2 id val env)
                  (cons (list id val) (car env)))
                (trace add-binding2)
                (cond
                  ((symbol? (cadr def)) ; constant definition
                   (set! env (add-binding (cadr def) (caddr def) env)))
                  ((list? (cadr def)) ; function definition
                   (set! env (add-binding2 (caadr def) (cddr def) env)))
                  (else (error "Invalid definition" def))))
              program)
    (eval-exp expr env))
  (trace eval-program)
  (define initial-env (cons '() (cons '() '()))) ; initial environment with empty frames
  (define result (eval-program prg initial-env))
  (format "<svg width=\"~a\" height=\"~a\">~a</svg>" width height result))
(trace execute)

(define test1
  '((define (start)
      (rect 0 0 100 100 "fill:red")
      (rect 100 0 100 100 "fill:green")
      (rect 200 0 100 100 "fill:blue"))))

(define test2
  '((define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
    (define START 195)
    (define END 10)
    (define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2)))))))
