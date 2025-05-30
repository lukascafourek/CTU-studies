#lang racket

(provide node node-v node-left node-right is-leaf? build-heap)

(struct node (v l r) #:transparent)

(define (is-leaf? nd)
  (eq? 'leaf nd))

(define (show-tree tree [depth 0])
  (define (offset d)
    (if (= d 0) ""
        (string-append "---" (offset (- d 1)))))
  (if (is-leaf? tree) tree
      (begin
        (show-tree (node-left tree) (+ depth 1))
        (displayln (string-append (offset depth) (number->string (node-v tree))))
        (show-tree (node-right tree) (+ depth 1))
        tree)))

(define (min-depth tree)
  (cond ((is-leaf? tree) 0)
        (#t (+ 1 (min (min-depth (node-left tree)) (min-depth (node-right tree)))))))

(define (enforce tree)
  (cond ((is-leaf? tree) 'leaf)
        ((and (is-leaf? (node-left tree)) (is-leaf? (node-right tree))) tree)
        ((is-leaf? (node-right tree))
         (let* ([x (node-v tree)]
                [l (enforce (node-left tree))]
                [ll (node-left l)]
                [lr (node-right l)]
                [lv (node-v l)]
                [nlv (min x lv)]
                [nv (max x lv)])
           (node nv (node nlv ll lr) 'leaf) ))
        (#t
         (let* ([x (node-v tree)]
                [l (enforce (node-left tree))]
                [ll (node-left l)]
                [lr (node-right l)]
                [lv (node-v l)]
                [r (enforce (node-right tree))]
                [rl (node-left r)]
                [rr (node-right r)]
                [rv (node-v r)]
                [nlv (min x lv)]
                [nrv (min x rv)]
                [nv (max x lv rv)])
           (node nv (node nlv ll lr) (node nrv rl rr))))))

(define (insert v tree)
  (define (dfs tr)
    (cond ((is-leaf? tr) (node v 'leaf 'leaf))
          ((< (min-depth (node-right tr)) (min-depth (node-left tr))) (node (node-v tr) (node-left tr) (dfs (node-right tr))))
          (#t (node (node-v tr) (dfs (node-left tr)) (node-right tr)))))
  (cond ((is-leaf? tree) (node v 'leaf 'leaf))
        (#t (dfs tree))))

(define (build-heap ls)
  (if (null? ls) 'leaf
      (enforce (insert (car ls) (build-heap (cdr ls))))))
