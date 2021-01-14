#lang racket
(define (f s1 s2)
  (cond     ((atom? s1) s2)
            ((atom? s2) s1)
            (else (f (car s1) (cdr s2)))))

(define (atom? x)
  (and (not (list? x))
       (not (null? x))))
(define (atoms s)
  (cond
    ((null? s) 0)
    ((atom? (car s)) (+ 1 (atoms (cdr s))))
    ((+ 0 (atoms (car s))))))

(define (nodup lst)
  (cond
    ((null? lst) #t)
    ((member (car lst) (cdr lst)) #f)
    ((nodup (cdr lst)))))