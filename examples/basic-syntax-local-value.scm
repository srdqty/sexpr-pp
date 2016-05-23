; This is a simple example demonstrating general compile-time
; bindings.
;
;
;
;
; Example run:
;
; >> (set-compile-type-ref (a big long expression))
;
; >> (get-sompile-time-ref)
; (a big long expression)
;
; Notice the expression is returned as a quoted list and NOT
; executed.

(define-syntax compile-time-ref (make-ref 0))

(define-syntax set-compile-time-ref!
  (lambda (stx)
    ; Strip the syntax object off the argument expression and set
    ; the ref to it.
    (ref-set! (syntax-local-value (syntax compile-time-ref))
              (stx-e (car (cdr (stx-e stx)))))
    ; Have to return a syntax object in a macro
    (syntax (void))))

(define-syntax get-compile-time-ref
  (lambda (stx)
    (letrec ((the-ref (syntax-local-value (syntax compile-time-ref)))
             (value (ref-get the-ref)))
      ; macros can only return syntax objects, so we have to wrap
      ; the value as a syntax object. We quote it as well so 
      ; the interpreter does not try to execute the value as an
      ; expression.
      (mk-stx (list (syntax quote)
                    (if (syntax? value)
                      value
                      (mk-stx value stx)))
              stx))))
