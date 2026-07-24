(import (scheme) (check) (tt type))

(define boolean-type (symbol->type 'boolean))
(define number-type (symbol->type 'number))
(define string-type (symbol->type 'string))

(check
  (equal?
    (type->datum (hole-type 'foo))
    '(hole foo)))

(check
  (equal?
    (type->datum (symbol->type 'foo))
    'foo))

(check
  (equal?
    (type->datum (symbol->type 'list string-type))
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
          (symbol->type 'pair $car $cdr))))
    '(forall t1 t2 (pair t1 t2))))


