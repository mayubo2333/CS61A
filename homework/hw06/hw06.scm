(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define (sign val) (
    cond
    ((< val 0) -1)
    ((> val 0) 1)
    (else 0)
))

(define (square x) (* x x))

(define (pow base exp) (
    cond
    ((= exp 1) base)
    ((= exp 2) (square base))
    ((even? exp) (square (pow base (quotient exp 2))))
    ((odd? exp) (* base (square (pow base (quotient (- exp 1) 2)))))
))
