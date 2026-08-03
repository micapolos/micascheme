(import
  (scheme)
  (check)
  (boolean)
  (tt primitive)
  (tt hoas))

(define boolean-class (class (declaration 'boolean 0 #'boolean=? #'identity) (list)))
(define number-class (class (declaration 'number 0 #'= #'identity) (list)))
(define string-class (class (declaration 'string 0 #'string=? #'identity) (list)))

(define (list-class $element) (class (declaration 'list 1 #'equal? #'identity) (list $element)))
(define (pair-class $car $cdr) (class (declaration 'pair 2 #'equal? #'identity) (list $car $cdr)))

; primitive->datum

(check
  (equal?
    (primitive->datum 0 (arrow (list boolean-class number-class) string-class))
    '(pi (boolean number) string)))

(check
  (equal?
    (primitive->datum 0 (arrow (list* boolean-class number-class) string-class))
    '(pi (boolean number ...) string)))

(check
  (equal?
    (primitive->datum 0 (list-class boolean-class))
    '(list boolean)))

(check
  (equal?
    (primitive->datum 0 (pair-class boolean-class number-class))
    '(pair boolean number)))

; --- primitive->syntax

(check
  (equal?
    (syntax->datum (primitive->syntax 0 (arrow (list (hole 0) (hole 1)) (hole 2))))
    '(arrow (list $0 $1) $2)))

(check
  (equal?
    (syntax->datum (primitive->syntax 0 (arrow (list* (hole 0) (hole 1)) (hole 2))))
    '(arrow (list* $0 $1) $2)))

; --- primitive-unify

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f #f)
      (arrow (list (hole 0) (hole 1)) (hole 2))
      (arrow (list boolean-class number-class) string-class))
    (list string-class number-class boolean-class)))

(check
  (false?
    (term-unify primitive-unify
      (list #f #f #f)
      (arrow (list (hole 0) (hole 1)) (hole 2))
      (arrow (list* boolean-class number-class) string-class))))

(check
  (false?
    (term-unify primitive-unify
      (list #f #f #f)
      (arrow (list* (hole 0) (hole 1)) (hole 2))
      (arrow (list boolean-class number-class) string-class))))

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f #f)
      (arrow (list* (hole 0) (hole 1)) (hole 2))
      (arrow (list* boolean-class number-class) string-class))
    (list string-class number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f)
      (arrow (list (hole 0) (hole 1)) (hole 0))
      (arrow (list boolean-class number-class) boolean-class))
    (list number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f)
      (arrow (list (hole 0) (hole 1)) (hole 0))
      (arrow (list boolean-class number-class) string-class))
    #f))

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f)
      (pair-class (hole 0) (hole 1))
      (pair-class boolean-class number-class))
    (list number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list #f #f)
      (pair-class (hole 0) (hole 1))
      (pair-class (hole 1) (hole 0)))
    (list #f (hole 1))))

