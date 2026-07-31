(import
  (scheme)
  (check)
  (tt primitive)
  (tt hoas))

(define boolean-class (class 'boolean (list)))
(define number-class (class 'number (list)))
(define string-class (class 'string (list)))

(define boolean-term boolean-class)
(define number-term number-class)
(define string-term string-class)

(define (list-class $element) (class 'list (list $element)))
(define (pair-class $car $cdr) (class 'pair (list $car $cdr)))

(check
  (equal?
    (primitive->datum 0 (atomic #'123 123))
    123))

(check
  (equal?
    (primitive->datum 0 (list-class boolean-term))
    '(list boolean)))

(check
  (equal?
    (primitive->datum 0 (pair-class boolean-term number-term))
    '(pair boolean number)))
