#lang racket

(provide is_valid?)

(define (gen-rows row col idx)
  (if (null? row) '()
      (if (eq? (car row) 1)
          (cons (cons idx col) (gen-rows (cdr row) (+ col 1) idx))
          (gen-rows (cdr row) (+ col 1) idx))))

(define (gen-board board row)
  (if (null? board) '()
      (append (gen-rows (car board) 0 row) (gen-board (cdr board) (+ row 1)))))

(define (valid-pair? coord1 coord2)
  (let ((absx (abs (- (car coord1) (car coord2))))
        (absy (abs (- (cdr coord1) (cdr coord2)))))
    (cond ((and (eq? absx 1) (eq? absy 2)) #f)
          ((and (eq? absx 2) (eq? absy 1)) #f)
          (#t #t))))

(define (valid-coord? coord coords)
  (cond ((null? coords) #t)
        ((valid-pair? coord (car coords)) (valid-coord? coord (cdr coords)))
        (#t #f)))

(define (valid-coords? coords)
  (cond ((null? coords) #t)
        ((valid-coord? (car coords) (cdr coords)) (valid-coords? (cdr coords)))
        (#t #f)))

(define (is_valid? board)
  (let ((coords (gen-board board 0)))
    (valid-coords? coords)))
