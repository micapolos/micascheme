(import
  (scheme)
  (check)
  (tt type)
  (tt compile-type))

(define boolean-type-declaration (type-declaration 'boolean 0))
(define number-type-declaration (type-declaration 'number 0))
(define string-type-declaration (type-declaration 'string 0))
(define list-type-declaration (type-declaration 'list 1))
(define pair-type-declaration (type-declaration 'pair 2))

(define boolean-type (declared-type boolean-type-declaration (list)))
(define number-type (declared-type number-type-declaration (list)))
(define string-type (declared-type string-type-declaration (list)))
(define (list-type $element) (declared-type list-type-declaration (list $element)))
(define (pair-type $car $cdr) (declared-type pair-type-declaration (list $car $cdr)))

(define test-lookup
  (lambda ($id)
    (cond
      ((free-identifier=? $id #'boolean) boolean-type-declaration)
      ((free-identifier=? $id #'number) number-type-declaration)
      ((free-identifier=? $id #'string) string-type-declaration)
      ((free-identifier=? $id #'list) list-type-declaration)
      ((free-identifier=? $id #'pair) pair-type-declaration)
      (else (syntax-error $id "unbound")))))

(check
  (equal?
    (compile-type test-lookup #'type)
    type-type))

(check
  (equal?
    (compile-type test-lookup #'number)
    number-type))

(check
  (equal?
    (compile-type test-lookup #'(list number))
    (list-type number-type)))

(check
  (equal?
    (compile-type test-lookup #'(lambda number string boolean))
    (lambda-type (list number-type string-type) (list boolean-type))))

(check
  (equal?
    (compile-type test-lookup #'(lambda number string void))
    (lambda-type (list number-type string-type) (list))))

(check
  (equal?
    (compile-type test-lookup #'(lambda number string (values boolean number)))
    (lambda-type (list number-type string-type) (list boolean-type number-type))))

(check
  (equal?
    (compile-type test-lookup #'(lambda number (vararg string) void))
    (lambda-type (list* number-type string-type) (list))))

(check
  (equal?
    (type->datum (compile-type test-lookup #'(forall car cdr (pair car cdr))))
    '(forall t1 t2 (pair t1 t2))))
