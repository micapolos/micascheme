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

; === resolve-hole-type

(check
  (equal?
    (resolve-hole-type
      (hole-type 'bar)
      (list (cons 'foo boolean-type)))
    (hole-type 'bar)))

(check
  (equal?
    (resolve-hole-type
      (hole-type 'bar)
      (list
        (cons 'foo boolean-type)
        (cons 'bar string-type)))
    string-type))

(check
  (equal?
    (resolve-hole-type
      (hole-type 'bar)
      (list
        (cons 'foo boolean-type)
        (cons 'bar (hole-type 'foo))))
    boolean-type))

; type=?

(check
  (type=?
    (forall-type 2
      (lambda ($car $cdr)
        (symbol->type 'pair $car $cdr)))
    (forall-type 2
      (lambda ($car $cdr)
        (symbol->type 'pair $car $cdr)))))

(check
  (not
    (type=?
      (forall-type 2
        (lambda ($car $cdr)
          (symbol->type 'pair $car $cdr)))
      (forall-type 2
        (lambda ($car $cdr)
          (symbol->type 'not-pair $car $cdr))))))

(check
  (not
    (type=?
      (forall-type 2
        (lambda ($car $cdr)
          (symbol->type 'pair $cdr $car)))
      (forall-type 2
        (lambda ($car $cdr)
          (symbol->type 'pair $car $cdr))))))

(check
  (not
    (type=?
      (forall-type 1
        (lambda ($element)
          (symbol->type 'list $element)))
      (forall-type 2
        (lambda ($car $cdr)
          (symbol->type 'pair $car $cdr))))))

