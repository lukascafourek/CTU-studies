#lang racket

(provide cheap-flight)

; list of nodes
(define ns '(1 2 3 4 5 6)) ; listofnodes
; list of edges where each edge contains (start end cost)
(define es '((1 2 0.5) (1 3 1.0) (2 3 2.0) (2 5 1.0) (3 4 4.0) (4 5 1.0)))
; the graph; a list of nodes and edges
(define gr (list ns es))

; some convenience functions
(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))

(define (neighbors n gr)
  (define (iter n es ns)
    (if (empty? es) ns
      (let* ([e (car es)]
             [a (car e)]
             [b (cadr e)]
             [rest-es (cdr es)])
        (cond
          ((= n a) (iter n rest-es (cons b ns)))
          ((= n b) (iter n rest-es (cons a ns)))
          (else (iter n rest-es ns))
          ))))
  (iter n (edges gr) '()))

(define (extend path gr)
  (if (empty? path) '()
    (map (lambda (n) (cons n path)) (neighbors (car path) gr))))

(define (unvisited path gr)
  (define (f ns) (not (member (car ns) (cdr ns))))
  (filter f (extend path gr)))

(define (split f lst)
  (define (iter f lst r1 r2)
    (if (empty? lst) (list r1 r2)
      (if (f (car lst))
        (iter f (cdr lst) (cons (car lst) r1) r2)
        (iter f (cdr lst) r1 (cons (car lst) r2)))))
  (iter f lst '() '()))

(define (search paths goal gr res)
  (if (empty? paths) res
    (let* ([p (car paths)]
           [ps (cdr paths)]
           [rps-eps (split (lambda (x) (= goal (car x))) (unvisited p gr))]
           [rps (car rps-eps)]
           [eps (cadr rps-eps)])
      (search (append ps eps) goal gr (append res (map reverse rps))))))

(define (find-edge xy gr)
  (define (f edge)
    (let ([a (car edge)]
          [b (cadr edge)]
          [c (cost edge)]
          [x (car xy)]
          [y (cadr xy)])
      (or (and (= x a) (= y b))
          (and (= x b) (= y a)))))
  (filter f (edges gr)))

(define (zip l1 l2) (map list l1 l2))
(define (sum xs) (foldl + 0 xs))
(define (init-list lst) (reverse (cdr (reverse lst))))

(define (total-cost path gr)
  (define es (zip (init-list path) (cdr path)))
  (define cs (map (lambda (e) (cost (car (find-edge e gr)))) es))
  (sum cs))

(define (connections a b gr)
  (define ps (search (list (list a)) b gr '()))
  (define cs (map (lambda (p) (total-cost p gr)) ps))
  (zip ps cs))

(define (cheap-flight a b gr)
  (define (cheaper? x y) (< (cadr x) (cadr y)))
  (define fs (sort (connections a b gr) cheaper?))
  (if (> (length fs) 0) (car fs) #f))
