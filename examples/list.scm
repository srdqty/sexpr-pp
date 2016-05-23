; Implementations of list processing functions.

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
