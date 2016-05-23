; Implemenations of let and let* forms
; Error-handling of bad syntax is not implemented

; (let ((id1 expr) ... (idN exprN)) body ...)
; converts to
; ((lambda (id1 ... idN) body ...) expr1 ... exprN)
(define-syntax let
  (lambda (stx)
    (letrec
    (
      (map (lambda (f lst)
              (if (pair? lst)
                (cons (f (car lst)) (map f (cdr lst)))
                (quote ()))))
      (params (mk-stx
                (map (lambda (x) (car (stx-e x)))
                     (stx-e (car (cdr (stx-e stx)))))
                stx))
      (args (map (lambda (x) (car (cdr (stx-e x))))
                   (stx-e (car (cdr (stx-e stx))))))
      (body (cdr (cdr (stx-e stx))))
      (fun (mk-stx (cons (syntax lambda) (cons params body))
                   stx))
    )
    (mk-stx (cons fun args) stx))))

; (let* ((id1 expr1) (id2 expr2) ... (idN exprN)) body ...)
; converts to
; ((lambda (id1) ((lambda (id2) ... ((lambda (idN) body) exprN)) expr2)) expr1)
(define-syntax let*
  (lambda (stx)
    (letrec
    (
      (bindings (stx-e (car (cdr (stx-e stx)))))
      (body (cdr (cdr (stx-e stx))))

      (process-binding
        (lambda (binding body)
          (letrec
          (
            (id (car (stx-e binding)))
            (expr (car (cdr (stx-e binding))))
            (fun (mk-stx (cons (syntax lambda)
                            (cons (mk-stx (list id) stx)
                                  body))
                         stx))
          )
            (list (mk-stx (list fun expr) stx)))))

      (process-bindings 
        (lambda (rest)
          (if (pair? rest)
            (process-binding (car rest)
                             (process-bindings (cdr rest)))
            body)))
    )
      (car (process-bindings bindings))
      )))
