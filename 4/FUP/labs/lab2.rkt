#lang racket

(require racket/trace)

(define (my-reverse lst [acc '()])
  (if (empty? lst)
      acc
      (my-reverse (cdr lst) (cons (car lst) acc))))

(trace my-reverse)

(define (group-same lst)
  (define (iter l gr)
    (cond [(empty? l) (list gr)]
          [(equal? (car gr) (car l))
           (iter (cdr l) (cons (car l) gr))]
          [else
           (cons gr (iter (cdr l) (list (car l))))]))
  (if (empty? lst)
      '()
      (iter (cdr lst) (list (car lst)))))

(define (join-lengths grs)
  (map (lambda (g) (cons (car g) (length g))) grs))


(define (letter-frequencies str)
  (sort
   (join-lengths
    (group-same
     (sort
      (filter char-alphabetic? (string->list (string-downcase str)))
      char<?)))
   >
   #:key cdr))

(define (letter-frequencies-2 str)
  (let* [(lowercase (string-downcase str))
         (listified (string->list lowercase))
         (alphabetic (filter char-alphabetic? listified))
         (sorted-chars (sort alphabetic char<?))
         (grouped (group-same sorted-chars))
         (joined (join-lengths grouped))
         (sorted-occurs (sort joined > #:key cdr))]
    sorted-occurs))

(define (average-list lst)
  (define (sum-list lst [acc 0])
    (if (empty? lst)
        acc
        (sum-list (cdr lst) (+ (car lst) acc))))
  (trace sum-list)
  (/ (sum-list lst) (length lst)))

(trace average-list)

(define (split-list n lst)
  (define (iter l k segment)
    (cond [(empty? l) (list segment)]
          [(zero? k)
           (cons segment (iter l n '()))]
          [else
           (iter (cdr l) (- k 1) (append segment (list (car l))))]))
  (trace iter)
  (iter lst n '()))

(define (n-block-average n lst)
  (map average-list (split-list n lst)))

(trace split-list)
