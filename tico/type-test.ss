(import (micascheme) (tico type))

(check (equal? (type-dynamic? (value-type "foo")) #f))
(check (equal? (type-dynamic? (native-type)) #t))
(check (equal? (type-dynamic? (type-type)) #t))
(check (equal? (type-dynamic? (struct `foo (list (value-type "foo") (value-type "bar")))) #f))
(check (equal? (type-dynamic? (struct `foo (list (value-type "foo") (number-type)))) #t))
(check (equal? (type-dynamic? (arrow (list (value-type "foo")) (value-type "bar"))) #t))
(check (raises? (lambda () (type-dynamic? `not-type))))

; --- types-arity

(check (equal? (types-arity (list (native-type) (value-type "foo") (native-type))) 2))

; --- type-matches?

(check
  (type-matches? 
    (value-type "foo") 
    (any-type)))

(check
  (type-matches? 
    (value-type "foo") 
    (value-type "foo")))

(check
  (not
    (type-matches? 
      (value-type "bar") 
      (value-type "foo"))))

(check
  (not
    (type-matches? 
      (native-type) 
      (value-type "foo"))))

(check
  (type-matches? 
    (native-type)
    (native-type)))

(check
  (not
    (type-matches? 
      (value-type "foo")
      (native-type))))

(check
  (type-matches? 
    (type-type)
    (type-type)))

(check
  (not
    (type-matches? 
      (value-type "foo")
      (type-type))))

(check
  (type-matches? 
    (struct 'foo (list (any-type) (native-type)))
    (struct 'foo (list (any-type) (native-type)))))

(check
  (type-matches? 
    (struct 'foo (list (value-type "foo") (native-type)))
    (struct 'foo (list (any-type) (native-type)))))

(check
  (not
    (type-matches? 
      (struct 'bar (list (any-type) (native-type)))
      (struct 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type)))
      (struct 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type) (native-type)))
      (struct 'foo (list (any-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type) (value-type "foo")))
      (struct 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (native-type)
      (struct 'foo (list (any-type) (native-type))))))

(check
  (type-matches? 
    (arrow (list (any-type) (native-type)) (value-type "foo"))
    (arrow (list (any-type) (native-type)) (value-type "foo"))))

(check
  (type-matches? 
    (arrow (list (value-type "foo") (native-type)) (value-type "foo"))
    (arrow (list (any-type) (native-type)) (value-type "foo"))))

(check
  (type-matches? 
    (arrow (list (any-type) (native-type)) (any-type))
    (arrow (list (any-type) (native-type)) (value-type "foo"))))

(check
  (not
    (type-matches? 
      (arrow (list (any-type) (value-type "foo")) (value-type "foo"))
      (arrow (list (any-type) (native-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (arrow (list (any-type) (native-type)) (value-type "bar"))
      (arrow (list (any-type) (native-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (native-type)
      (arrow (list (any-type) (native-type)) (value-type "foo")))))
