#lang racket
;Patrick Amons
;CSCI320-S20-Assign1-PA.rkt

;#1
(caddr '(a b x d))
(car(cadadr '(a (b (x d)))))
(car(cadadr(caar '(((a (b (x) d)))))))

;#2
(cons 'a (cons 'b(cons 'x (cons 'd '()))))
(cons 'a (cons (cons 'b (cons (cons 'x (cons 'd '())) '())) '()))
(cons (cons (cons 'a (cons (cons 'b (cons (cons 'x '()) (cons 'd '()))) '())) '())'())

;#3
(define (head lst) (car lst))
(head '(a b))

(define (tail lst) (cdr lst))
(tail '(a b))

;#4a
(define (switch-list lst)
  (list (cadr lst) (car lst))
 )
(switch-list '(a b))

;#4b
(define (switch-cons lst)
  (cons (cadr lst) (cons (car lst) '()))
)  
(switch-cons '(a b))

;#5
(define (addlength lst)
  (cond
    ((null? lst) '(0))
    (else (cons (length lst) lst))
    )
  )
(addlength '(a b c d))

;#6
(define (distance p1 p2)
  (sqrt (+ (sqr(- (car p2) (car p1))) (sqr(- (cadr p2) (cadr p1)))))
  )
(distance '(3 -1) '(5 6))