(define (split-at lst n) 
    (define (split-at-tail lst-a lst-b remain-n) (
        if (or (= remain-n 0) (null? lst-b))
            (append (list lst-a) lst-b)
            (split-at-tail (append lst-a (list (car lst-b))) (cdr lst-b) (- remain-n 1))))
    (split-at-tail '() lst n))


(define (compose-all funcs)
    (define (compose-all-tail remain-funcs curr-compose) (
        if (null? remain-funcs)
            curr-compose
            (compose-all-tail (cdr remain-funcs) (lambda (x) ((car remain-funcs) (curr-compose x))))))
    (compose-all-tail funcs (lambda (x) x)))
