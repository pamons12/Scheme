#lang racket
;#1
(display "#1 apply_to_all function") (newline)
(define(apply_to_all op_name lst)
  (cond
    ((null? lst) '())
    (else (cons (op_name(car lst)) (apply_to_all op_name (cdr lst))))
    )
  )
(apply_to_all sqr '(1 2 3 4))
(apply_to_all car '((1 2) (a b) (x y z) (a 6)))
(apply_to_all + '(1 2 3 4))
(apply_to_all - '(1 2 3 4))