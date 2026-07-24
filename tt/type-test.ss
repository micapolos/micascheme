(import (scheme) (check) (tt type))

(define boolean-type (symbol->type 'boolean))
(define number-type (symbol->type 'number))
(define string-type (symbol->type 'string))

(check
  (equal?
    (type->datum
      (declared-type
        (type-declaration (gensym) 'foo 0)
        (list)))
    'foo))

(check
  (equal?
    (type->datum
      (declared-type
        (type-declaration (gensym) 'list 1)
        (list string-type)))
    '(list string)))

(check
  (equal?
    (type->datum (lambda-type (list string-type number-type) (list)))
    '(lambda string number void)))

(check
  (equal?
    (type->datum (lambda-type (list string-type number-type) (list boolean-type)))
    '(lambda string number boolean)))

(check
  (equal?
    (type->datum (lambda-type (list string-type number-type) (list boolean-type number-type)))
    '(lambda string number (values boolean number))))

(check
  (equal?
    (type->datum
      (forall-type 2
        (lambda ($car $cdr)
          (declared-type
            (type-declaration (gensym) 'pair 2)
            (list $car $cdr)))))
    '(forall t1 t2 (pair t1 t2))))


