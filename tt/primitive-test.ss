(import
  (scheme)
  (check)
  (boolean)
  (procedure)
  (tt lookup)
  (tt primitive)
  (tt term))

(define boolean-class (class 'boolean))
(define number-class (class 'number))
(define string-class (class 'string))

(define (list-class $element) (application (class 'list) $element))
(define (pair-class $car $cdr) (application* (class 'pair) $car $cdr))

(define add-class (generate-class "+"))

(define (add $a $b)
  (cond
    ((and (number? $a) (number? $b)) (+ $a $b))
    (else (application* add-class $a $b))))

; primitive->datum

(check
  (equal?
    (primitive->datum 0 (arrow (list boolean-class number-class) #f string-class))
    '(pi (boolean number) string)))

(check
  (equal?
    (primitive->datum 0 (arrow (list boolean-class) number-class string-class))
    '(pi (boolean number ...) string)))

(check
  (equal?
    (term->datum primitive->datum 0 (list-class boolean-class))
    '(list boolean)))

(check
  (equal?
    (term->datum primitive->datum 0 (pair-class boolean-class number-class))
    '(pair boolean number)))

(check
  (equal?
    (term->datum primitive->datum 0 add-class)
    '+))

(check
  (equal?
    (term->datum primitive->datum 0 (add 10 20))
    30))

(check
  (equal?
    (term->datum primitive->datum 0 (add (variable 0) (variable 1)))
    '(+ $0 $1)))

; --- primitive->syntax

(check
  (equal?
    (syntax->datum (primitive->syntax 0 (arrow (list (variable 0) (variable 1)) #f (variable 2))))
    '(arrow (list $0 $1) #f $2)))

(check
  (equal?
    (syntax->datum (primitive->syntax 0 (arrow (list (variable 0)) (variable 1) (variable 2))))
    '(arrow (list $0) $1 $2)))

(check
  (equal?
    (syntax->datum (term->syntax primitive->syntax 0 (term-apply (abstraction* a b (add a b)) 10 20)))
    '30))

; --- primitive-unify

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank blank)
      (arrow (list (hole 0) (hole 1)) #f (hole 2))
      (arrow (list boolean-class number-class) #f string-class))
    (list string-class number-class boolean-class)))

(check
  (false?
    (term-unify primitive-unify
      (list blank blank blank)
      (arrow (list (hole 0) (hole 1)) #f (hole 2))
      (arrow (list boolean-class) number-class string-class))))

(check
  (false?
    (term-unify primitive-unify
      (list blank blank blank)
      (arrow (list (hole 0)) (hole 1) (hole 2))
      (arrow (list boolean-class number-class) #f string-class))))

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank blank)
      (arrow (list (hole 0)) (hole 1) (hole 2))
      (arrow (list boolean-class) number-class string-class))
    (list string-class number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank)
      (arrow (list (hole 0) (hole 1)) #f (hole 0))
      (arrow (list boolean-class number-class) #f boolean-class))
    (list number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank)
      (arrow (list (hole 0) (hole 1)) #f (hole 0))
      (arrow (list boolean-class number-class) #f string-class))
    #f))

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank)
      (pair-class (hole 0) (hole 1))
      (pair-class boolean-class number-class))
    (list number-class boolean-class)))

(check
  (equal?
    (term-unify primitive-unify
      (list blank blank)
      (pair-class (hole 0) (hole 1))
      (pair-class (hole 1) (hole 0)))
    (list blank (hole 1))))

; === syntax->primitive

(check
  (term=? primitive=? 0
    (syntax->primitive
      (partial syntax->term syntax->primitive)
      (lookup (+ ($prim +)))
      #'(call + 1 2 3))
    6))

(check
  (term=? primitive=? 0
    (syntax->primitive
      (partial syntax->term syntax->primitive)
      (lookup (+ ($prim +)))
      #'(call + 1 2 (hole 0)))
    (application* ($prim +) 1 2 (hole 0))))
