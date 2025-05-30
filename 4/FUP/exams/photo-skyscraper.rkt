#lang racket

(provide best-view)

(define (transpose matrix)
  (apply map list matrix))

(define (roofs-row row [height 0] [acc 0])
  (if (empty? row) acc
      (if (< height (car row))
          (roofs-row (cdr row) (car row) (+ acc 1))
          (roofs-row (cdr row) height acc))))

(define (roofs city direction)
  (define mat (match direction
                ['W city]
                ['N (transpose city)]
                ['E (map reverse city)]
                ['S (map reverse (transpose city))]))
  (apply + (map roofs-row mat)))

(define (best-view city)
  (define views (list (list 'N (roofs city 'N))
                      (list 'S (roofs city 'S))
                      (list 'E (roofs city 'E))
                      (list 'W (roofs city 'W))))
  (define (compare prev-max next)
    (if (< (cadr prev-max) (cadr next)) next prev-max))
  (define dir-roofs (foldl compare (list 'None 0) views))
  (cons (car dir-roofs) (cadr dir-roofs)))
