#lang racket

(require racket/trace)

(define (interleave el lst)
  (if (null? lst)
      ; there is only a single way one can insert el into '()
      (list (list el))                          
      ; otherwise one possibility is to prepend el to lst
      (cons (cons el lst)
            ; for the rest take all possible insertions of el into (cdr lst) 
            (map (curry cons (car lst))
                 ; and prepend (car lst) to each of them
                 (interleave el (cdr lst))))))

(trace interleave)

(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append 
             ; into each permutation of (cdr lst) interleave (car last) 
             (map (curry interleave (car lst)) (permutations (cdr lst))))))

(trace permutations)

(struct node (var left right) #:transparent)

(struct assignment (var val) #:transparent)

(define bool-tree
  (node 'x1
        (node 'x2
              (node 'x3 1 0)
              (node 'x3 0 1))
        (node 'x2
              (node 'x3 0 0)
              (node 'x3 1 1))))

(define (evaluate tree vals)
  (match vals
    [(list) tree]
    [(list 0 vs ...) (evaluate (node-left tree) vs)]
    [(list 1 vs ...) (evaluate (node-right tree) vs)]))

(trace evaluate)

(define (evaluate2 tree vals)
  (define (left-right x)                         ; define function 0 -> node-left, 1 -> node-right
    (if (zero? x) node-left node-right))
  (trace left-right)
  ((apply compose (map left-right (reverse vals))) tree))  ; map it over vals, compose the resulting functions and apply to tree

(trace evaluate2)

(define (satisficing-evaluations tree [ev '()])
  (match tree
    [1 (list (reverse ev))]        ; we reverse the evaluation so that the root variable comes first
    [0 '()]                        
    [(node v l r)
     (append (satisficing-evaluations l (cons (assignment v 0) ev))      
             (satisficing-evaluations r (cons (assignment v 1) ev)))]))

(trace satisficing-evaluations)

(define (sub-seq lst)
  (if (null? lst)
      '(())
      (let ([el (car lst)]
            [rest-sub-seq (sub-seq (cdr lst))])
        (append rest-sub-seq
                (map ((curry cons) el) rest-sub-seq)))))

(trace sub-seq)

(struct mtch (winner left right) #:transparent)

(define tour
  (mtch 'F
        (mtch 'D
              (mtch 'A 'A 'B)
              (mtch 'D 'C 'D))
        (mtch 'F
              (mtch 'F 'E 'F)
              (mtch 'G 'G 'H))))

(define (beaten-teams tree [acc '()])
  (match tree
    [(mtch win win r) (cons r acc)]
    [(mtch win l win) (cons l acc)]
    [(mtch win (mtch win l r) (mtch los _ _)) (beaten-teams (mtch win l r) (cons los acc))]
    [(mtch win (mtch los _ _) (mtch win l r)) (beaten-teams (mtch win l r) (cons los acc))]))

(trace beaten-teams)

(define tournament '(F (D (A (A) (B)) (D (C) (D))) (F (F (E) (F)) (G (G) (H)))))

(define (beaten-teams2 tree)
  (define (winner tr) (car tr))
  (trace winner)
  (define (is-leaf? tr) (= (length tr) 1))
  (trace is-leaf?)
  (define (left-subtree tr) (cadr tr))
  (trace left-subtree)
  (define (right-subtree tr) (caddr tr))
  (trace right-subtree)
  (define (iter tr acc)
    (cond
      ([is-leaf? tr] acc)
      ([eqv? (winner (left-subtree tr)) (winner tree)]
       (iter (left-subtree tr)
             (cons (winner (right-subtree tr)) acc)))
      (else
       (iter (right-subtree tr)
             (cons (winner (left-subtree tr)) acc)))))
  (trace iter)
  (iter tree '()))

(trace beaten-teams2)
