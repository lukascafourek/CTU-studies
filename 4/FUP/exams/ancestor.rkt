#lang racket

(provide find-path common-ancestor (struct-out node) (struct-out leaf))

(struct node (val left right) #:transparent)
(struct leaf (val) #:transparent)

(define tree (node 1 (node 2 (leaf 5) (leaf 6))
                     (node 3 (leaf 4) (leaf 7))))

(define tree2 (node 1 (node 2 (leaf 3)
                              (node 4 (leaf 5)
                                      (leaf 6)))
                      (node 7 (leaf 8)
                              (leaf 9))))

(define (find-path x tree)
  (match tree
    [(leaf lf) (if (equal? x lf) (list x) '())]
    [(node n l r) (if (equal? x n) (list x)
                      (let* ([lchild (find-path x l)]
                             [rchild (find-path x r)]
                             [children (append lchild rchild)])
                        (if (null? children) '() (cons n children))))]))

(define (common-ancestor x y tree)
  (define left (find-path x tree))
  (define right (find-path y tree))
  (define common (take-common-prefix left right))
  (if (null? common) #f (last common)))
