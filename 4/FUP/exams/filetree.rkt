#lang racket

(provide parse exists)

(define files
  (list "src/tree.hs"
        "src/complex.hs"
        "scripts/ex1/test.ss"
        "scripts/ex1/eval.ss"
        "scripts/emptydir"
        "scripts/ex2/test.ss"
        "tests/test_tree.hs"))

(define (split file)
  (string-split file "/"))

(define (insert lst tree)
  (if (empty? lst) tree
      (let* ((key (car lst))
             (value (if (dict-has-key? tree key) (dict-ref tree key) #hash()))
             (new-value (insert (cdr lst) value)))
        (dict-set tree key new-value))))

(define (parse files)
  (foldl (lambda (file tree) (insert (split file) tree)) #hash() files))

(define (exists file tree)
  (define (helper lst tree)
    (cond [(empty? lst) #t]
          [(empty? tree) #f]
          [(dict-has-key? tree (car lst)) (helper (cdr lst) (dict-ref tree (car lst)))]
          [else #f]))
    (helper (split file) tree))
