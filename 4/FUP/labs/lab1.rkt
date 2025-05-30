#lang racket

(require racket/trace)

(define (fact n)
  (if (< n 1)
    1
    (* n (fact (- n 1)))))

(trace fact)

(define (my-even? n)
  (cond [(< n 0) (my-even? (- n))]
        [(= n 0) #t]
        [(= n 1) #f]
        [else (my-even? (- n 2))]))

(trace my-even?)

(define (copy-str n str)
  (if (= n 0)
      ""
      (string-append str (copy-str (- n 1) str))))

(trace copy-str)

(define (copy-str2 n str [acc ""])
  (if (= n 0)
      acc
      (copy-str2 (- n 1) str (string-append str acc))))

(trace copy-str2)

(define (integer->string i)
  (string (integer->char i)))

(define (consecutive-chars fst lst)
  (define first-index (char->integer  fst))
  (define last-index (char->integer lst))
  (define step (if (< first-index last-index) 1 -1))
  (define (iter k acc)
      (if (= k last-index)
          (string-append acc (integer->string k))
          (iter (+ k step)
                (string-append acc (integer->string k)))))
  (iter first-index ""))

(define (char+1 c) (integer->char (add1 (char->integer c))))

(define (char-1 c) (integer->char (sub1 (char->integer c))))

(define (consecutive-chars2 fst lst [acc ""])
  (cond [(char=? fst lst)
         (string-append acc (string fst))]
        [(char<? fst lst)
         (consecutive-chars2 (char+1 fst) lst (string-append acc (string fst)))]
        [(char>? fst lst)
         (consecutive-chars2 (char-1 fst) lst (string-append acc (string fst)))]))

(define (num-of-digits n [acc 1])
  (cond [(< n 0) (num-of-digits (- n) acc)]
        [(< n 10) acc]
        [else (num-of-digits (quotient n 10) (add1 acc))]))

(trace num-of-digits)

(define (num->str n [radix 10])
  (define symbols "0123456789ABCDEF")
  (define (convert n)
    (if (< n radix)
        (string (string-ref symbols n))
        (string-append (convert (quotient n radix))
                       (string (string-ref symbols (remainder n radix))))))
  (if (= n 0)
      "0"
      (if (< n 0)
          (string-append "-" (convert (abs n)))
          (convert n))))
