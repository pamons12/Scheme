#lang racket
;#1
(display "#1\n")

(define (make_list_size n e)
  (cond
    ((eqv? n 0) '())
    ((eqv? n 1) (cons e '()))
    (else(cons e (make_list_size (- n 1) e)))
    )
  )
(make_list_size 4 2)
(make_list_size 3 '(1 a))
(make_list_size 3 (+ 3 2))


;#2
(display "\n#2\n")

(define (zeros lst)
  (cond
    ((null? lst) 0)
    ((eqv? 0 (car lst)) (+ 1 (zeros(cdr lst))))
    (else(+ 0 (zeros(cdr lst))))
    )
  )
(zeros '())
(zeros '(1 2))
(zeros '(0 0))

;#3
(display "\n#3\n")

(define (remove lst atm)
  (cond
    ((null? lst) '())
    ((eqv? atm (car lst)) (remove (cdr lst) atm))
    (else (cons (car lst) (remove (cdr lst) atm)))
    )
  )
(remove '(1 2 a (a b) a 3) 'a)

;#4
(display "\n#4\n")

(define (largest lst)
  (cond
    ((null? lst) '())
    (else(largestH lst (car lst)))
    )
  )

(define (largestH lst max)
  (cond
    ((null? lst) max)
    ((< max (car lst)) (largestH (cdr lst) (car lst)))
    (else (largestH (cdr lst) max))
    )
  )
(largest '())
(largest '(1))
(largest '(1 2 3 4))
(largest '(4 3 2 1))
(largest '(2 4 10 -1))

(display "Done")