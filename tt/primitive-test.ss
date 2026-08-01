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

; primitive->datum

(check
  (equal?
    (primitive->datum 0 universe)
    'type))

(check
  (equal?
    (primitive->datum 0 (arrow (list boolean-class number-class) string-class))
    '(pi boolean number string)))

(check
  (equal?
    (primitive->datum 0 (list-class boolean-class))
    '(list boolean)))

(check
  (equal?
    (primitive->datum 0 (pair-class boolean-class number-class))
    '(pair boolean number)))
