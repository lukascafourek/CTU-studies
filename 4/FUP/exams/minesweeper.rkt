#lang racket

(define (int->digit i) (integer->char (+ 48 i)))

(define (print-square count square)
  (match (cons count square)
    ((cons _ #\*) #\*)
    ((cons 0 _) #\.)
    ((cons count _) (int->digit count))))

(define (neighbors board mx my x y)
  (for*/list ((i (range (max 0 (- x 1)) (min mx (+ x 2))))
              (j (range (max 0 (- y 1)) (min my (+ y 2)))))
    (list-ref (list-ref board i) j)))

(define (num_mines n_proc x y square)
  (let* ((ns (n_proc x y))
         (filter_ns (filter (curry equal? #\*) ns))
         (mine_count (length filter_ns)))
    (print-square mine_count square)))

(define (sweep board)
  (define mx (length board))
  (define my (length (car board)))
  (define n (curry neighbors board mx my))
  (define (inner x row) (map (curry num_mines n x) (range 0 my) row))
  (map inner (range mx) board))

(let* ((input-string (port->lines))
       (ls (map string->list input-string))
       (counted-board (sweep ls))
       (sn (map list->string counted-board)))
  (for ((l sn))
    (display l)
    (newline)))
