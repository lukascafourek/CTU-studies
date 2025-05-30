#lang racket

(define (make-tape2 left val right)
  (list left val right))
 
(define (fresh-tape2 size2)
  (make-tape2 '() 0 (make-list (- size2 1) 0)))
 
(define (get2 msg2)
  (cond
    ([eqv? msg2 'left] car)
    ([eqv? msg2 'val] cadr)
    ([eqv? msg2 'right] caddr)
    (else (error "unknown message"))
    )
  )

(define (change2 op2 tape2)
  (make-tape2 ((get2 'left) tape2)
             (op2 ((get2 'val) tape2) 1)
             ((get2 'right) tape2))  
  )
 
(define (move2 dir2 tape2)
   (let ([left ((get2 'left) tape2)]
        [val ((get2 'val) tape2)]
        [right ((get2 'right) tape2)])
    (cond
      ([null? ((get2 dir2) tape2)] (error "Outside tape"))
      ([eqv? dir2 'left] (make-tape2 (cdr left) (car left) (cons val right)))
      (else (make-tape2 (cons val left) (car right) (cdr right)))
    ))  
  )

(define tape2 (make-tape2 '(3 2 1) 4 '(5 6 7)))

;;; Lab 6 - imperative racket

;;; Task 1 - make purely functional tape
(define (make-tape left val right)
  (list left val right))

(define (fresh-tape size)
  (make-tape '() 0 (make-list (- size 1) 0)))

(define (get msg)
  (cond
    ([eqv? msg 'left] car)
    ([eqv? msg 'val] cadr)
    ([eqv? msg 'right] caddr)
    (else (error "unknown message"))
    )
  )

(define (change op tape)
  (make-tape ((get 'left) tape)
             (op ((get 'val) tape) 1)
             ((get 'right) tape)))

(define (move dir tape)
  (let ([left ((get 'left) tape)]
        [val ((get 'val) tape)]
        [right ((get 'right) tape)])
    (cond
      ([null? ((get dir) tape)] (error "Outside tape"))
      ([eqv? dir 'left] (make-tape (cdr left) (car left) (cons val right)))
      (else (make-tape (cons val left) (car right) (cdr right))))
    ))

;;; Task 2 - modify the interpreter of Brainf*ck from the lecture to be purely functional
; Sample program adding two numbers
(define add-prg
  '(@ > @ [- < + >] < *)
  )

; Sample program multiplying two numbers
(define mul-prg
  '(@ > @ < [- > [- > + > + < <] > [- < + >] < <] > > > *)
  )

; constant for the size of the tape
(define SIZE 10)

(define (eval-comma prg input tape)
  (cond
    ([null? input] (error "Empty input"))
    (else (eval-prg prg
                    (cdr input)
                    (make-tape ((get 'left) tape)
                               (car input)
                               ((get 'right) tape))))))
  
(define (eval-cmd cmd prg input tape)
  (let ([new-tape
         (cond
           ([eqv? cmd '+] (change + tape)) 
           ([eqv? cmd '-] (change - tape)) 
           ([eqv? cmd '<] (move 'left tape)) 
           ([eqv? cmd '>] (move 'right tape)) 
           ([eqv? cmd '*] (printf "~a " ((get 'val) tape)) tape) 
           (else (error "Unknown command")))])
    (eval-prg prg input new-tape))
  )

(define (eval-cycle cycle prg input tape)
  (if (= ((get 'val) tape) 0)
      (eval-prg prg input tape)
      (begin
        ;(displayln "Entering a cycle")
        (let* ([cycle-result (eval-prg cycle input tape)]
               [new-input (car cycle-result)]
               [new-tape (cadr cycle-result)])
          (eval-cycle cycle prg new-input new-tape)))))

(define (eval-prg prg input tape)
  ;(displayln tape)
  (if (null? prg)
      (list input tape)
      (let ([cmd (car prg)]
            [rest (cdr prg)])
        (cond
          ([eqv? cmd '@] (eval-comma rest input tape))
          ([list? cmd] (eval-cycle cmd rest input tape))
          (else (eval-cmd cmd rest input tape))))
      ))

(define (run-prg prg input)
  (eval-prg prg input (fresh-tape SIZE))
  (printf "done~n")
  )

(run-prg add-prg '(12 5))
