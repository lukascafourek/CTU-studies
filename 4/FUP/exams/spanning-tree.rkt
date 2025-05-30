#lang racket

(provide minimum-spanning-tree graph edge)

(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)

(define g (graph '(A B C D E F)
                   (list (edge 'A 'B 1)
                         (edge 'D 'E 4)
                         (edge 'E 'F 7)
                         (edge 'A 'D 5)
                         (edge 'B 'E 2)
                         (edge 'C 'F 5)
                         (edge 'D 'B 6)
                         (edge 'E 'C 4)
                         (edge 'A 'E 3))))

(define (reverse-edge e)
  (match e
    [(edge u v w) (edge v u w)]))

(define (reverse-edges g)
  (graph (graph-nodes g) (append (graph-edges g) (map reverse-edge (graph-edges g)))))

(define (find egs covered uncovered)
  (car (sort (filter (lambda (e) (and (member (edge-u e) covered) (member (edge-v e) uncovered))) egs) (lambda (edge1 edge2) (< (edge-weight edge1) (edge-weight edge2))))))

(define (iter g covered uncovered tree)
  (if (null? uncovered) tree
      (let* ([e (find (graph-edges g) covered uncovered)]
             [v (edge-v e)])
        (iter g (cons v covered) (remove v uncovered) (cons e tree)))))

(define (minimum-spanning-tree g)
  (let* ([bigger-g (reverse-edges g)]
         [nodes (graph-nodes g)]
         [covered (list (car nodes))]
         [uncovered (cdr nodes)])
    (iter bigger-g covered uncovered '())))
