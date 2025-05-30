#lang racket

(provide encode)

(define (transpose mat)
  (apply map list mat))

(define (str-mat str)
  (filter char-alphabetic? (string->list (string-downcase str))))

(define (num-of-cols n)
  (exact-ceiling (sqrt n)))

(define (rows mat n)
  (let ((len (length mat)))
    (cond [(null? mat) '()]
          [(< len n) (list (append mat (make-list (- n len) #\space)))]
          [else (cons (take mat n) (rows (drop mat n) n))])))

(define (encode str)
  (let* ((mat (str-mat str))
         (n (num-of-cols (length mat)))
         (r (transpose (rows mat n))))
    (string-join (map list->string r))))
