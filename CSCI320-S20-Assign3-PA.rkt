#lang racket
(display "#1 Comlex Numbers\n")
;Gets the real part  in the complex #
(define (real  num) (car num))
(display "Real part in (2.5 3.7): ")
(real '(2.5 3.7))

;Gets the complex part  in the complex #
(define (complex  num) (cadr num))
(display "Complex part in (2.5 3.7): ")
(complex '(2.5 3.7))

;Compliment
(define (compl num)
  (list (real num) (- 0 (complex num)))
  )
(display "\nCompliment of (2.5 3.7): ")
(compl '(2.5 3.7))

;Absolute Value
(define (abs num)
  (sqrt (+ (sqr (real num)) (sqr (complex num))))
  )
(display "\nabs of (2.5 3.7): ")
(abs '(2.5 3.7))

;Comparing two complex numbers
(define (equal? num1 num2)
  (cond
    ((and (eqv? (real num1) (real num2)) (eqv? (complex num1) (complex num2))) #t)
    (else #f)
    )
  )
(display "\nComparison of '(2.5 3.7) '(2.5 3.7): ")
(equal? '(2.5 3.7) '(2.5 3.7))
(display "Comparison of '(2.5 3.7) '(1.4 2.4): ")
(equal? '(2.5 3.7) '(1.4 2.4))

;Addition
(define (plus num1 num2)
  (list (+ (real num1) (real num2)) (+ (complex num1) (complex num2)))
  )
(display "\nAddition of '(2.5 3.7) '(2.5 3.7): ")
(plus '(2.5 3.7) '(2.5 3.7))

;Subtraction
(define (minus num1 num2)
  (list (- (real num1) (real num2)) (- (complex num1) (complex num2)))
  )
(display "\nSubtraction of '(2.5 3.7) '(2.5 3.7): ")
(minus '(2.5 3.7) '(2.5 3.7))

;Multiplication
(define (prod num1 num2)
  (list (- (* (real num1) (real num2)) (* (complex num1) (complex num2))) (+ (* (real num1) (complex num2)) (* (real num2) (complex num1))))
  )
(display "\nProduct of '(2.5 3.7) '(2.5 3.7): ")
(prod '(2.5 3.7) '(2.5 3.7))

;Division
(define (quotient num1 num2)
  (list (/ (+ (* (real num1) (real num2)) (* (complex num1) (complex num2))) (+ (sqr (real num2)) (sqr (complex num2))))
        (/ (- (* (real num2) (complex num1)) (* (real num1) (complex num2))) (+ (sqr (real num2)) (sqr (complex num2))))
        )
  )
(display "\nQuotient of '(6 -7) '(1 -2): ")
(quotient '(6 -7) '(1 -2))
(display "Quotient of '(2.5 3.7) '(2.5 3.7): ")
(quotient '(2.5 3.7) '(2.5 3.7))

;Binary Trees
(display "\n#2 Binary Trees\n")

;Gets atom of tree
(define (tatom tree)
  (cond
    ((null? tree) '())
    (else (car tree))
    )
  )
(display "Atom of '(): ")
(tatom '())
(display "Atom of '(A '() '()): ")
(tatom '(A '() '()))

;Gets left subtree
(define (leftSubTree tree)
  (cond
    ((null? tree) '())
    (else (cadr tree))
    )
  )
(display "Left subtree of '(): ")
(leftSubTree '())
(display "Left subtree of ex: ")
(leftSubTree '(a (b () (c () ()))(d () (e (f () ()) ()))))

;Gets right subtree
(define (rightSubTree tree)
  (cond
    ((null? tree) '())
    (else (caddr tree))
    )
  )
(display "Right subtree of '(): ")
(rightSubTree '())
(display "Right subtree of ex: ")
(rightSubTree '(a (b () (c () ()))(d () (e (f () ()) ()))))

;Determines if a tree is a leaf
(define (leaf? tree)
  (cond
    ((null? tree) #t)
    ((and (null? (leftSubTree tree)) (null? (rightSubTree tree))) #t)
    (else #f)
    )
  )
(display "leaf? of '(): ")
(leaf? '())
(display "leaf? of '(a () ()): ")
(leaf? '(a () ()))
(display "leaf? of '(a (b () ()) ()): ")
(leaf? '(a (b () ()) ()))

;Appends lst1 to lst2
(define (append lst1 lst2)
  (cond
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    (else(cons(car lst1)(append(cdr lst1)lst2)))
    )
  )

;Determines if list is a tree
(define (tree? lst)
  (cond
    ((null? (tatom lst)) #f)
    (else #t)
    )
  )
(display "tree? of '(): ")
(tree? '())
(display "tree? of ex: ")
(tree? '(a (b () (c () ()))(d () (e (f () ()) ()))))

;Returns the a list with tree in preorder
(define (preorder tree)
  (cond
    ((leaf? tree) (cons (tatom tree) '()))
    ((null? (leftSubTree tree)) (cons (tatom tree) (preorder (rightSubTree tree))))
    ((null? (rightSubTree tree)) (cons (tatom tree) (preorder (leftSubTree tree))))
    (else(append (cons (tatom tree) (preorder (leftSubTree tree))) (preorder (rightSubTree tree))))
    )
  )
(display "Tree (a (b () ()) (c () ())) in preorder: ")
(preorder '(a (b () ()) (c () ())))
(display "Ex tree in preorder: ")
(preorder '(a (b () (c () ()))(d () (e (f () ()) ()))))

;Returns a list with tree in inorder order
(define (inorder tree)
  (cond
    ((leaf? tree) (cons (tatom tree) '()))
    ((null? (leftSubTree tree)) (cons (tatom tree) (inorder (rightSubTree tree))))
    ((null? (rightSubTree tree)) (append (inorder (leftSubTree tree)) (cons (tatom tree) '())))
    (else(append (append (inorder (leftSubTree tree)) (cons (tatom tree) '())) (inorder (rightSubTree tree))))
    )
  )
(display "Tree (a (b () ()) (c () ())) in inorder: ")
(inorder '(a (b () ()) (c () ())))
(display "Ex tree in inorder: ")
(inorder '(a (b () (c () ()))(d () (e (f () ()) ()))))

;Returns a list with tree in postorder
(define (postorder tree)
  (cond
    ((leaf? tree) (cons (tatom tree) '()))
    ((null? (leftSubTree tree)) (append (postorder (rightSubTree tree)) (cons (tatom tree) '())))
    ((null? (rightSubTree tree)) (append (postorder (leftSubTree tree)) (cons (tatom tree) '())))
    (else(append (append (postorder (leftSubTree tree)) (postorder (rightSubTree tree))) (cons (tatom tree) '())))
    )
  )
(display "Tree (a (b () ()) (c () ())) in postorder: ")
(postorder '(a (b () ()) (c () ())))
(display "Ex tree in postorder: ")
(postorder '(a (b () (c () ()))(d () (e (f () ()) ()))))

