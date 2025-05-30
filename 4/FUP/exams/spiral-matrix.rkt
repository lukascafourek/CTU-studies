#lang racket

(provide spiral-matrix)

(define (add small n)
  (map (curry map (curry + n)) small))

(define (wrap mat first last)
  (cons first (append mat (list last))))

(define (horizontal mat n)
  (map wrap mat (range (- (* 4 n) 4) (- (* 3 n) 2) -1) (range (+ 1 n) (- (* 2 n) 1))))

(define (vertical mat n)
  (wrap mat (range 1 (+ 1 n)) (range (- (* 3 n) 2) (- (* 2 n) 2) -1)))

(define (spiral-matrix n)
  (cond
    [(equal? 1 n) '((1))]
    [(even? n) "Error: only odd numbers allowed"]
    [else (let ((small (spiral-matrix (- n 2))))
            (vertical (horizontal (add small (- (* 4 n) 4)) n) n))]))
