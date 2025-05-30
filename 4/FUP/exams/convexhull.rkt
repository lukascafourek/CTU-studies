#lang racket

(provide convex-hull)

(define points '((-2 3) (2 2) (-1 1) (-2 -1.5) (4 -1) (1 -3)))

(define (get-x p) (car p))

(define (get-y p) (cadr p))

(define (polar-a a b)
  (define x1 (get-x a))
  (define y1 (get-y a))
  (define x2 (get-x b))
  (define y2 (get-y b))
  (define angle (atan (- y2 y1) (- x2 x1)))
  (if (< angle 0) (+ angle (* 2 pi)) angle))

(define (sort-a point points)
  (define (f a b) (< (polar-a point a) (polar-a point b)))
  (define filtered-points (filter (lambda (p) (not (equal? p point))) points))
  (cons point (sort filtered-points f)))

(define (find points)
  (define (f a b)
    (define x1 (get-x a))
    (define y1 (get-y a))
    (define x2 (get-x b))
    (define y2 (get-y b))
    (if (= y1 y2) (if (> x1 x2) a b) (if (< y1 y2) a b)))
  (foldl f (car points) (cdr points)))

(define (is-left a b c)
  (define x1 (get-x a))
  (define y1 (get-y a))
  (define x2 (get-x b))
  (define y2 (get-y b))
  (define x3 (get-x c))
  (define y3 (get-y c))
  (define cross (- (* (- x2 x1) (- y3 y1)) (* (- y2 y1) (- x3 x1))))
  (> cross 0))

(define (scan points hull)
  (define p (car points))
  (define ps (cdr points))
  (define h1 (car hull))
  (define h2 (cadr hull))
  (define hs (cddr hull))
  (if (is-left h2 h1 p)
      (graham ps (cons p hull))
      (scan points (cons h2 hs))))

(define (graham points hull)
  (if (empty? points) hull
      (if (< (length hull) 2)
          (graham (cdr points) (cons (car points) hull))
          (scan points hull))))

(define (convex-hull points)
  (define sorted (sort-a (find points) points))
  (reverse (graham sorted '())))
