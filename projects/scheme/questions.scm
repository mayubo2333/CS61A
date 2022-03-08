(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (enumerate-tail remain-s curr-s cnt)
    (if (null? remain-s) 
      curr-s 
      (enumerate-tail (cdr remain-s) (append curr-s (list (list cnt (car remain-s)))) (+ 1 cnt))))
  (enumerate-tail s '() 0))
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to INORDER? and return
;; the merged lists.
(define (merge inorder? list1 list2)
  ; BEGIN PROBLEM 16
  (define (merge-tail remain-list1 remain-list2 curr-list) (cond 
    ((and (null? remain-list1) (null? remain-list2)) curr-list)
    ((null? remain-list2) (merge-tail (cdr remain-list1) remain-list2 (append curr-list (list (car remain-list1)))))
    ((null? remain-list1) (merge-tail remain-list1 (cdr remain-list2) (append curr-list (list (car remain-list2)))))
    ((inorder? (car remain-list1) (car remain-list2)) (merge-tail (cdr remain-list1) remain-list2 (append curr-list (list (car remain-list1)))))
    (else (merge-tail remain-list1 (cdr remain-list2) (append curr-list (list (car remain-list2)))))))

  (merge-tail list1 list2 '()))


  ; END PROBLEM 16


;; Optional Problem 1

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (append (list form params) (map let-to-lambda body))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
            (let ((params (map car values))
                    (nums (map cadr values)))
                (append (list (append (list 'lambda params) (map let-to-lambda body))) (map let-to-lambda nums)))
           ; END PROBLEM 17
           ))
        (else
         ; BEGIN PROBLEM 17
         (map let-to-lambda expr)
         ; END PROBLEM 17
         )))



;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'replace-this-line
  )
  ; END Question 21