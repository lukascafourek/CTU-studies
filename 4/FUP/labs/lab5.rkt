#lang racket

(require racket/trace)

(define (ints-from n)
  (stream-cons n (ints-from (+ n 1))))

(define (stream-add s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2))
               (stream-add (stream-rest s1) (stream-rest s2))))

(define nats (ints-from 0))

(define fibs
  (stream* 0 1 (stream-add fibs (stream-rest fibs))))

(define fib-stream
  (stream-cons 0
               (stream-cons 1
                            (stream-add fib-stream
                                        (stream-rest fib-stream)))))

(define fib-stream2
  (stream* 0 1 (stream-add fib-stream2
                           (stream-rest fib-stream2))))

(struct graph (nodes edges))

(define gr
  (graph
    '(1 2 3 4 5 6)
    '((1 2) (1 5) (2 3) (2 5) (3 4) (4 5) (4 6))))

(define ((edge? gr) pair)
  (define edges (graph-edges gr))
  (or (member pair edges) (member (reverse pair) edges)))

;(define lst '(1 2 3 4))
;(map list
;     (take lst (- (length lst) 1))
;     (cdr lst))

(define ((check-path gr) lst)
  (define but-last (take lst (- (length lst) 1)))
  (define pairs (map list but-last (cdr lst)))
  (if (andmap (edge? gr) pairs)
      lst
      #f))

;(define perms (permutations (graph-nodes gr)))
;(filter identity (map (check-path gr) perms))

(define (find-hamiltonian-path g)
  (define nodes (get-nodes g))
  (define perms (permutations nodes))
  (let ([paths (filter identity (map (check-path2 g) perms))])
    (if (null? paths)
        #f
        (car paths))))

(define (find-hamiltonian-path-lazy g)
  (define perms (permutations (graph-nodes g)))
  (let ([paths (filter identity (map (check-path g) perms))])
    (if (null? paths)
        #f
        (car paths))))

(define (make-graph nodes edges)
  (list nodes edges))
(define (get-nodes g) (car g))
(define (get-edges g) (cadr g))

(define gr2 (make-graph '(1 2 3 4 5 6) '((1 2) (1 5) (2 3) (2 5) (3 4) (4 5) (4 6))))

; test whether a pair of nodes is connected
(define (edge2? g)
  (lambda (p)
    (define edges (get-edges g))
    (or (member p edges) (member (reverse p) edges))))

(define (check-path2 g)
  (lambda (lst)
    (define but-last (take lst (- (length lst) 1)))
    (if (andmap (edge2? g) (map list but-last (cdr lst)))
        lst
        #f)))

(define (find-hamiltonian-path2 g)
  (define nodes (get-nodes g))
  (define perms (permutations nodes))
  (let ([paths (filter identity (map (check-path2 g) perms))])
    (if (null? paths)
        #f
        (car paths))))

(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (stream-mul (stream-rest s1) (stream-rest s2))))
 
(define factorial-stream (stream* 1 (stream-mul (in-naturals 1) factorial-stream)))
;(stream->list (stream-take factorial-stream 10))
 
(define (exp-stream x)
  (define recipr (stream-map ((curry /) 1) factorial-stream))
  (define powers (stream-map ((curry expt) x) (in-naturals)))
  (stream-mul powers recipr))

; lazy subsequences
(define (stream-merge s1 s2 cmp)
  (cond
    ([stream-empty? s1] s2)
    ([stream-empty? s2] s1)
    ([cmp (stream-first s1) (stream-first s2)]
     (stream-cons (stream-first s1) (stream-merge (stream-rest s1) s2 cmp)))
    (else (stream-cons (stream-first s2) (stream-merge s1 (stream-rest s2) cmp)))))
 
(define (sub-seq lst)
  (if (null? lst)
      (stream '())
      (let ([el (car lst)]
            [rest-sub-seq (sub-seq (cdr lst))])
        (stream-merge rest-sub-seq
                (stream-map ((curry cons) el) rest-sub-seq)
                (lambda (x y) (< (length x) (length y)))))))

; minimum vertex cover = smallest subset of nodes such that each edge has one of its node in it
(define (check-cover g)
  (define edges (get-edges g))
  (lambda (lst)
    (if (andmap (lambda (e) (or (member (car e) lst) (member (cadr e) lst))) edges)
        lst
        #f)))
 
(define (min-vertex-cover g)
  (define nodes (get-nodes g))
  (stream-first (stream-filter identity (stream-map (check-cover g) (sub-seq nodes)))))
