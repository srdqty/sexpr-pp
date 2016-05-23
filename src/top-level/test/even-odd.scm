(define even-odd
  (letrec
    ((even? (lambda (x) (if (= 0 x) #t (odd? (- x 1)))))
     (odd? (lambda (x) (if (= 0 x) #f (even? (- x 1))))))
    (cons even? odd?)))
(define even? (car even-odd))
(define odd? (cdr even-odd))
