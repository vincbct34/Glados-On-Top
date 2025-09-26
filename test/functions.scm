; Variable definition and lookup
(define x 10)
x
(define y 20)
(+ x y)

; Function definition with lambda
(define square (lambda (n) (* n n)))
(square 5)
(square 10)

; Recursive function - factorial
(define factorial (lambda (n) 
  (if (= n 0) 
      1 
      (* n (factorial (- n 1))))))
(factorial 0)
(factorial 1)
(factorial 5)

; Higher-order functions
(define apply-twice (lambda (f x) (f (f x))))
(apply-twice square 3)

; Conditional expressions
(define abs (lambda (x) 
  (if (< x 0) (- x) x)))
(abs -5)
(abs 5)
(abs 0)
