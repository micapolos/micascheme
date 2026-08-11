(import
  (scheme)
  (check)
  (boolean)
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
    (primitive->datum 0 (arrow (list boolean-class number-class) #f string-class))
    '(pi (boolean number) string)))

(check
  (equal?
    (primitive->datum 0 (arrow (list boolean-class) number-class string-class))
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
    (syntax->datum (primitive->syntax 0 (arrow (list (variable 0) (variable 1)) #f (variable 2))))
    '(arrow (list $0 $1) #f $2)))

(check
  (equal?
    (syntax->datum (primitive->syntax 0 (arrow (list (variable 0)) (variable 1) (variable 2))))
    '(arrow (list $0) $1 $2)))

; --- primitive-dynamic?

(define static (tuple (list)))
(define dynamic (tuple (list (variable 0))))

(check (primitive-dynamic? 0 dynamic))
(check (not (primitive-dynamic? 0 static)))

(check (not (primitive-dynamic? 0 (arrow (list) #f static))))
(check (primitive-dynamic? 0 (arrow (list dynamic) #f static)))
(check (primitive-dynamic? 0 (arrow (list) dynamic static)))
(check (primitive-dynamic? 0 (arrow (list) #f dynamic)))

(check (primitive-dynamic? 0 number-class))

(check (not (primitive-dynamic? 0 (tuple (list)))))
(check (not (primitive-dynamic? 0 (tuple (list static)))))
(check (primitive-dynamic? 0 (tuple (list dynamic))))

(check (not (primitive-dynamic? 0 (choice (list)))))
(check (not (primitive-dynamic? 0 (choice (list static)))))
(check (primitive-dynamic? 0 (choice (list dynamic))))
(check (primitive-dynamic? 0 (choice (list static static))))

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
