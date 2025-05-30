#lang racket

(provide add-edge build-tree (struct-out node) (struct-out leaf))

(struct node (val kids) #:transparent)
(struct leaf (val) #:transparent)

(define (get-value v)
  (match v
    [(leaf lf) lf]
    [(node n children) n]))

(define (add-edge edge tree)
  (define from (car edge))
  (define to (cadr edge))
  (match tree
    [(leaf lf) (if (= lf from)
                   (node lf (list (leaf to)))
                   (leaf lf))]
    [(node n children) (if (= n from)
                           (node n (sort (cons (leaf to) children) (lambda (v c) (< (get-value v) (get-value c)))))
                           (node n (map (curry add-edge edge) children)))]))

(define (build-tree init edges)
  (foldl add-edge init edges))
