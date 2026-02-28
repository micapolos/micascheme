(import (scheme) (check) (identifier) (symbol) (syntax))

(check (equal? (identifier-named? #`foo foo) #t))
(check (equal? (identifier-named? #`foo bar) #f))

(check
  (equal?
    (syntax->datum
      (build-identifier ($string #'dupa) (string-append $string "-jasiu")))
    'dupa-jasiu))

(check
  (free-identifier=?
    (identifier-append #'+ #'string #'- #'append)
    #'string-append))

(check
  (equal?
    (syntax->datum
      (syntax-case #'(inc x) ()
        ((inc x)
          (datum? inc)
          #`(+ x 1))
        ((dec x)
          (datum? dec)
          #`(- x 1))
        ((other x)
          #`(* x 1))))
    `(+ x 1)))

(check
  (equal?
    (syntax->datum
      (syntax-case #'(dec x) ()
        ((inc x)
          (datum? inc)
          #`(+ x 1))
        ((dec x)
          (datum? dec)
          #`(- x 1))
        ((other x)
          #`(* x 1))))
    `(- x 1)))

(check
  (equal?
    (syntax->datum
      (syntax-case #'(boo x) ()
        ((inc x)
          (datum? inc)
          #`(+ x 1))
        ((dec x)
          (datum? dec)
          #`(- x 1))
        ((other x)
          #`(* x 1))))
    `(* x 1)))

(check
  (equal?
    (syntax->datum
      (syntax-case #'(print something new x) ()
        ((print something new b)
          (and
            (identifiers print something new)
            #`(displayln x)))))
    `(displayln x)))

(check (syntax=? (identifier id) #'id))
