#lang racket

(require "lambda-calculus.rkt")

(define zero '(λ s : (λ z : z)))
(define one '(λ s : (λ z : (s z))))
(define M '(λ a : (λ b : (λ c : (a (b c))))))

(define T '(λ x : (λ y : x)))
(define F '(λ x : (λ y : y)))
 
(define CONS 
  '(λ a : (λ b : (λ z : ((z a) b))))
  )

(define SWAP `(λ p : (λ z : ((z (p ,F)) (p ,T)))))

(define lst `((,CONS a) ((,CONS b) ,F)))

(define neg `(λ x : ((x ,F) ,T)))
(define NULL? `(λ p : ((p (λ a : (λ b : ,neg))) ,T)))

(define len 
  (lambda (p) (if (null? p)
                    0
                    (+ (len (cdr p)) 1))))

(define S '(λ w : (λ y : (λ x : (y ((w y) x))))))
(define Y '(λ y : ((λ x : (y (x x))) (λ x : (y (x x))))))

(define LEN
  `(λ r :
     (λ lst : (
               ((,NULL? lst) ,zero)
               (,S (r (lst ,F)))))))
