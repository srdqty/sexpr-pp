; (and expr1 expr2 ... exprN) converts to
; (if expr1
;   (if expr2
;     ... (if expr<N-1>
;           exprN
;           #f)
;     #f)
;   #f)
(define-syntax and
  (lambda (stx)
    (letrec
    (
      (process-expr
        (lambda (e then-rest)
          (if then-rest
            (mk-stx (list (syntax if)
                          e
                          then-rest
                          (syntax #f))
                    stx)
            e)))
      (loop
        (lambda (rest)
          (if (pair? rest)
            (process-expr (car rest)
                          (loop (cdr rest)))
            #f)))
      (exprs (cdr (stx-e stx)))
    )
      (if (null? exprs)
        (syntax #t)
        (loop exprs))
    )))

; (or expr1 expr2 ... expr <N-1> exprN) converts to
; (letrec ((it0 expr1))
;     (if it0
;       it0
;       (letrec ((it1 expr2))
;         (if it1
;           it1
;           ...
;           (letrec ((it<N-1> expr<N-1>))
;             (if it<N-1>
;               it<N-1>
;               exprN))))))
(define-syntax or
  (lambda (stx)
    (letrec
    (
      (exprs (cdr (stx-e stx)))
      (ids (generate-ids (syntax it) (length exprs)))

      (make-letrec
        (lambda (id expr body)
          (mk-stx (list (syntax letrec)
                        (mk-stx (list (mk-stx (list id expr)
                                               stx))
                                stx)
                        body)
                  stx)))
      
      (process-expr
        (lambda (id expr else-rest)
          (if else-rest
            (make-letrec id expr
                         (mk-stx (list (syntax if)
                                       expr
                                       id
                                       else-rest)
                                 stx))
            expr)))
      (loop
        (lambda (ids exprs)
          (if (pair? exprs)
            (process-expr (car ids) (car exprs)
                     (loop (cdr ids) (cdr exprs)))
            #f)))
    )
      (if (null? exprs)
        (syntax #f)
        (loop ids exprs)))
      ))
