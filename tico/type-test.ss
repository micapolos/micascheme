(import (micascheme) (tico type))

(check (equal? (type-dynamic? (native-type)) #t))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (value-type "bar")))) #f))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (number-type)))) #t))
(check (equal? (type-dynamic? (lambda-type (list (value-type "foo")) (value-type "bar"))) #t))
(check (raises? (lambda () (type-dynamic? `not-type))))

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
    (struct-type 'foo (list (any-type) (native-type)))
    (struct-type 'foo (list (any-type) (native-type)))))

(check
  (type-matches? 
    (struct-type 'foo (list (value-type "foo") (native-type)))
    (struct-type 'foo (list (any-type) (native-type)))))

(check
  (not
    (type-matches? 
      (struct-type 'bar (list (any-type) (native-type)))
      (struct-type 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (struct-type 'foo (list (any-type)))
      (struct-type 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (struct-type 'foo (list (any-type) (native-type)))
      (struct-type 'foo (list (any-type))))))

(check
  (not
    (type-matches? 
      (struct-type 'foo (list (any-type) (value-type "foo")))
      (struct-type 'foo (list (any-type) (native-type))))))

(check
  (not
    (type-matches? 
      (native-type)
      (struct-type 'foo (list (any-type) (native-type))))))

(check
  (type-matches? 
    (lambda-type (list (any-type) (native-type)) (value-type "foo"))
    (lambda-type (list (any-type) (native-type)) (value-type "foo"))))

(check
  (type-matches? 
    (lambda-type (list (value-type "foo") (native-type)) (value-type "foo"))
    (lambda-type (list (any-type) (native-type)) (value-type "foo"))))

(check
  (type-matches? 
    (lambda-type (list (any-type) (native-type)) (any-type))
    (lambda-type (list (any-type) (native-type)) (value-type "foo"))))

(check
  (not
    (type-matches? 
      (lambda-type (list (any-type) (value-type "foo")) (value-type "foo"))
      (lambda-type (list (any-type) (native-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (lambda-type (list (any-type) (native-type)) (value-type "bar"))
      (lambda-type (list (any-type) (native-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (native-type)
      (lambda-type (list (any-type) (native-type)) (value-type "foo")))))
