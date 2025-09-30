; Error cases - these should fail gracefully

; Undefined variable
; undefined_var

; Division by zero  
; (/ 10 0)

; Car of empty list
; (car ())

; Cdr of empty list
; (cdr ())

; Type errors
; (+ 1 "hello")
; (car 42)

; //TODO: decide how to handle non-boolean conditions in if
;(if 123 1 2)

; Arity errors

; //TODO: decide how to handle these errors
; (+)
; (- )
; (* 1)
; (car 1 2)
; (cons 1)
