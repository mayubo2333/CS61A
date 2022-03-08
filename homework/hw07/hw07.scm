(define (filter-lst fn lst) (cond
  ((null? lst) lst) 
  (else 
    (if (fn (car lst)) (cons (car lst) (filter-lst fn (cdr lst))) (filter-lst fn (cdr lst)) )
  )
))

; ;; Tests
(define (even? x) (= (modulo x 2) 0))

(filter-lst even? '(0 1 1 2 3 5 8))

; expect (0 2 8)
(define (interleave first second) (cond
  ((and (null? first) (null? second)) nil)
  ((null? first) (interleave second first))
  (else (cons (car first) (interleave second (cdr first))))
))

(interleave (list 1 5 3) (list 2 4 6))

; expect (1 2 5 4 3 6)
(interleave (list 1 3 5) nil)

; expect (1 3 5)
(interleave (list 1 3 5) (list 2 4))

; expect (1 2 3 4 5)
(define (accumulate combiner start n term) (cond
  ((= n 0) start)
  (else (combiner (term n) (accumulate combiner start (- n 1) term))) 
))

(define (without-duplicates lst) (cond
  ((null? lst) nil)
  (else (cons (car lst) (without-duplicates (filter-lst (lambda (x) (not (= x (car lst)))) (cdr lst)))))
))
