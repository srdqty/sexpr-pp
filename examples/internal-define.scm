; This is an example of using local-expand to
; define a macro. Local-expand is used to find
; the internal defines in the function body.

; (fun (param0 ...) 
;   (define id<0> expr<0>)
;   expr<1> ... expr<i>
;   (define id<1> expr<i+1>)
;   expr<j> ...)
;
;   expands to
;
;   (lambda (param0 ...)
;     ((lambda (id<0>)
;        expr<1> .. expr<i>
;        ((lambda (id<1>)
;           expr<j> ...)
;         <expr<i+1>))
;      expr<0>))
;
; So basically a (define id expr) form
; binds id to expr for the remainder of
; the function body.
(define-syntax fun
  (letrec
  (
    (stopSet (list (syntax define)))
    (starts-with-define?
      (lambda (stx)
        (if (error? stx)
          #f
          (if (pair? (stx-e stx))
            (if (identifier? (car (stx-e stx)))
              (symbol=? (stx-e (car (stx-e stx)))
                        (quote define))
            #f)
          #f))))
    (is-define?
      (lambda (stx)
        (letrec ((result (local-expand stx stopSet)))
          (if (starts-with-define? result)
            result
            #f))))

  )
    (lambda (stx)
      (letrec
      (
        (params (car (cdr (stx-e stx))))
        (body (cdr (cdr (stx-e stx))))
        (process-expr
          (lambda (expr body)
            (cons expr
                  body)))
        (process-define
          (lambda (def body)
            (letrec
            (
              (id (car (cdr (stx-e def))))
              (expr (car (cdr (cdr (stx-e def)))))
              (fun (mk-stx (cons (syntax lambda)
                              (cons (mk-stx (list id) stx)
                                    body))
                           stx))
            )
              (list (mk-stx (list fun expr) stx)))))
        (process-body 
          (lambda (rest)
            (if (pair? rest)
              (letrec ((define-stx (is-define? (car rest))))
                (if define-stx
                  (process-define define-stx
                                  (process-body (cdr rest)))
                  ; do not use the partial expansion for an expression
                  ; let the expander expand the expression in the output
                  ; syntax object
                  (process-expr (car rest)
                                (process-body (cdr rest)))))
                       (list))))
      )
        (mk-stx (cons (syntax lambda)
                      (cons params
                            (process-body body)))
                stx)
      ))))

; This version of define should still work with the
; the (fun () ...) form because local-expand will expand
; it and find the define.
(define-syntax my-define
  (lambda (stx)
    (mk-stx (cons (syntax define)
                  (cdr (stx-e stx)))
            stx)))

;;; TODO?: add support for internal begin forms that are spliced into
;;; the function body so that the macro will find the define in an
;;; expression like this:
;;; (fun () (begin (define x 5) 3) (+ x 1))
