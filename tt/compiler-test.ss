(import (scheme) (check) (tt compiler))

(define boolean-type-declaration (type-declaration (gensym) 'boolean 0))
(define boolean-type (declared-type boolean-type-declaration (list)))

(define fixnum-type-declaration (type-declaration (gensym) 'fixnum 0))
(define fixnum-type (declared-type fixnum-type-declaration (list)))

(define string-type-declaration (type-declaration (gensym) 'string 0))
(define string-type (declared-type string-type-declaration (list)))

(define list-type-declaration (type-declaration (gensym) 'list 1))
(define (list-type $type) (declared-type list-type-declaration (list $type)))

(define (lookup $identifier)
  (cond
    ((free-identifier=? $identifier #'boolean) boolean-type-declaration)
    ((free-identifier=? $identifier #'fixnum) fixnum-type-declaration)
    ((free-identifier=? $identifier #'string) string-type-declaration)
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

(check
  (equal?
    (syntax->type lookup #'(lambda (fixnum boolean) string))
    (lambda-type (list fixnum-type boolean-type) string-type)))
