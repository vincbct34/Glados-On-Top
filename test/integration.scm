; Complex integration tests

; Fibonacci sequence
(define fib (lambda (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(fib 0)
(fib 1)
(fib 5)
(fib 8)

; List processing functions
(define length (lambda (lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst))))))

(define mylist (list 1 2 3 4 5))
(length mylist)
(length ())

; Map-like function
(define map-square (lambda (lst)
  (if (null? lst)
      ()
      (cons (* (car lst) (car lst)) (map-square (cdr lst))))))

(map-square (list 1 2 3 4))

; Nested data structures
(define matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
matrix
(car (car matrix))
(car (cdr (car matrix)))

; Higher-order function composition
(define compose (lambda (f g) (lambda (x) (f (g x)))))
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define add1-then-double (compose double add1))
(add1-then-double 5)

((lambda (a b) (+ a b)) 3 4)

; Lisp function without lambda
(define (truc a b)
(+ a b))

(truc 3 4)
