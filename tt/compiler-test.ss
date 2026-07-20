(import (scheme) (check) (tt compiler))

(define boolean-type-declaration (type-declaration 'boolean 'boolean 0))
(define boolean-type (declared-type boolean-type-declaration (list)))

(define fixnum-type-declaration (type-declaration 'fixnum 'fixnum 0))
(define fixnum-type (declared-type fixnum-type-declaration (list)))

(define number-type-declaration (type-declaration 'number 'number 0))
(define number-type (declared-type number-type-declaration (list)))

(define string-type-declaration (type-declaration 'string 'string 0))
(define string-type (declared-type string-type-declaration (list)))

(define list-type-declaration (type-declaration 'list 'list 1))
(define (list-type $type) (declared-type list-type-declaration (list $type)))

(define (lookup $identifier)
  (cond
    ((free-identifier=? $identifier #'boolean) boolean-type-declaration)
    ((free-identifier=? $identifier #'fixnum) fixnum-type-declaration)
    ((free-identifier=? $identifier #'number) number-type-declaration)
    ((free-identifier=? $identifier #'string) string-type-declaration)
    ((free-identifier=? $identifier #'list) list-type-declaration)
    ((free-identifier=? $identifier #'string-append)
      (typed
        (lambda-type (list string-type string-type) string-type)
        #'string-append))))

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
    (type->datum (syntax->type lookup #'fixnum))
    'fixnum))

(check
  (equal?
    (type->datum (syntax->type lookup #'(list fixnum)))
    '(list fixnum)))

(check
  (equal?
    (type->datum (syntax->type lookup #'(lambda (fixnum boolean) string)))
    '(lambda (fixnum boolean) string)))

(check
  (equal?
    (typed-syntax->datum (syntax->typed-param lookup #'(s string)))
    '(typed string s)))

(check
  (equal?
    (typed-syntax->datum (syntax->typed lookup #'(lambda ((s string) (n number)) s)))
    '(typed
      (lambda (string number) string)
      (lambda (s n) s))))

(check
  (equal?
    (typed-syntax->datum (syntax->typed lookup #'(string-append "foo" "bar")))
    '(typed string (string-append "foo" "bar"))))
