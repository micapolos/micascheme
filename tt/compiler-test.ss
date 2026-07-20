(import (scheme) (check) (tt compiler))

(define fixnum-type-declaration (type-declaration (gensym) 'fixnum 0))
(define fixnum-type (declared-type fixnum-type-declaration (list)))

(define list-type-declaration (type-declaration (gensym) 'list 1))
(define (list-type $type) (declared-type list-type-declaration (list $type)))

(define (lookup $identifier)
  (cond
    ((free-identifier=? $identifier #'fixnum) fixnum-type-declaration)
    ((free-identifier=? $identifier #'list) list-type-declaration)))

(check
  (equal?
    (syntax->type-declaration lookup #'fixnum)
    fixnum-type-declaration))

(check
  (equal?
    (syntax->type-declaration lookup #'list)
    list-type-declaration))

(check
  (equal?
    (syntax->type lookup #'fixnum)
    fixnum-type))

(check
  (equal?
    (syntax->type lookup #'(list fixnum))
    (list-type fixnum-type)))
