#lang racket

(provide justify)

(define words1 '("This" "is" "an" "example" "of" "text" "justification."))

(define words2 '("What" "must" "be" "acknowledgment" "shall" "be"))

(define words3 '("Science" "is" "what" "we"
                 "understand" "well" "enough"
                 "to" "explain" "to" "a" "computer."
                 "Art" "is" "everything" "else" "we" "do"))

(define (space words max-width)
  (let ((spaces (- max-width (foldl + 0 (map string-length words)))))
    (cond [(null? words) null]
          [(= 1 (length words)) (string-append (car words) (make-string (- max-width (string-length (car words))) #\space))]
          [else (let*-values (((words-to-space) (length (cdr words)))
                              ((left-space-length) (quotient spaces words-to-space))
                              ((left-space) (make-string left-space-length #\space))
                              ((before-space-length) (ceiling (/ spaces words-to-space)))
                              ((before-space) (make-string before-space-length #\space))
                              ((count) (- spaces (* words-to-space left-space-length)))
                              ((before-word left-word) (split-at words (add1 count))))
                  (string-append (string-join before-word before-space)
                                 (if (null? left-word) "" (string-join left-word left-space #:before-first left-space))))])))

(define (cut words max-width [curr-width 0] [acc '()])
  (if (empty? words) (list (reverse acc) words)
      (let* ((word-length (string-length (car words))))
        (if (> (+ curr-width word-length) max-width) (list (reverse acc) words)
            (cut (cdr words) max-width (+ 1 curr-width word-length) (cons (car words) acc))))))

(define (cut-all words max-width)
  (define word (cut words max-width))
  (define row (car word))
  (define word-line (cadr word))
  (cons row (if (empty? word-line) word-line (cut-all word-line max-width))))

(define (justify words max-width)
  (define (spaces n) (make-string n #\space))
  (define lines (cut-all words max-width))
  (define last-word (take lines (- (length lines) 1)))
  (define partial (map (lambda (r) (space r max-width)) last-word))
  (define last-line (string-join (last lines)))
  (define last-fill (string-append last-line (spaces (- max-width (string-length last-line)))))
  (append partial (list last-fill)))
