; List construction and manipulation
()
(list)
(list 1)
(list 1 2 3)
(cons 1 ())
(cons 1 (cons 2 (cons 3 ())))

; List access
(define mylist (list 1 2 3 4 5))
(car mylist)
(cdr mylist)
(car (cdr mylist))

; Nested lists
(define nested (list (list 1 2) (list 3 4) 5))
nested
(car nested)
(car (car nested))
(cdr (car nested))

; Type predicates
(null? ())
(null? (list 1))
(list? ())
(list? (list 1 2))
(number? 42)
(number? "hello")
(atom? hello)
(atom? 42)
