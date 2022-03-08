(define (tail-replicate x n) 
  (define (run L-sofar curr) (
    if (= curr n) 
    L-sofar
    (run (cons x L-sofar) (+ curr 1))))
  (run '() 0))

(define-macro (def func args body)
    `(define ,func (lambda ,args ,body)))

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let (
        (y (repeatedly-cube (- n 1) x))
      )
    (* y y y))))
