#lang racket

(provide rps)

(define (game-finished? strats) (or (null? strats) (ormap null? strats)))
(define (strats-current strats) (map car strats))
(define (strats-future strats) (map cdr strats))

(define (view players bools)
  (for/list ((player players) (bool bools) #:when bool) player))

(define (eliminate strats)
  (match (remove-duplicates (sort strats symbol<?))
    ('(r s) 's)
    ('(p r) 'r)
    ('(p s) 'p)
    (_ '())))

(define (remaining strats)
  (let* ((elim (eliminate strats)))
    (map (curry (compose not equal?) elim) strats)))

(define (rps players strategies)
  (if (game-finished? strategies)
      players
      (let* ((current (strats-current strategies))
             (future (strats-future strategies))
             (round (remaining current)))
        (rps (view players round) (view future round)))))
