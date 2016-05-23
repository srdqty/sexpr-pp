(define reverse
  (letrec
    ((rev (lambda (lst out)
            (if (pair? lst)
              (rev (cdr lst) (cons (car lst) out))
              out))))
    (lambda (lst) (rev lst (quote ())))))

(define map
  (letrec 
    ((map (lambda (f lst out) 
            (if (pair? lst)
              (map f (cdr lst) (cons (f (car lst)) out))
              (reverse out)))))
    (lambda (f lst) (map f lst (quote ())))))

(define foldl
  (letrec
    ((foldl (lambda (f lst init)
              (if (pair? lst)
                (foldl f (cdr lst) (f (car lst) init))
                init))))
    foldl))

(define foldr
  (letrec
    ((foldr (lambda (f lst init)
              (if (pair? lst)
                (f (car lst) (foldr f (cdr lst) init))
                init))))
    foldr))

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
