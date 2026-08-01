(import
  (scheme)
  (check)
  (tt primitive)
  (tt hoas))

(define boolean-class (class (declaration 'boolean 0) (list)))
(define number-class (class (declaration 'number 0) (list)))
(define string-class (class (declaration 'string 0) (list)))

(define (list-class $element) (class (declaration 'list 1) (list $element)))
(define (pair-class $car $cdr) (class (declaration 'pair 2) (list $car $cdr)))

(check
  (equal?
    (primitive->datum 0 (atomic #'123 123))
    123))

(check
  (equal?
    (primitive->datum 0 (list-class boolean-class))
    '(list boolean)))

(check
  (equal?
    (primitive->datum 0 (pair-class boolean-class number-class))
    '(pair boolean number)))
