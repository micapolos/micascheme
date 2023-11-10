(import (micascheme) (tico type))

(check (equal? (type-dynamic? (value-type "foo")) #f))
(check (equal? (type-dynamic? (native-type)) #t))
(check (equal? (type-dynamic? (type-type)) #t))

(check (equal? (type-dynamic? (struct `foo (list (value-type "foo") (value-type "bar")))) #f))
(check (equal? (type-dynamic? (struct `foo (list (value-type "foo") (number-type)))) #t))

(check (equal? (type-dynamic? (arrow (list (value-type "foo")) (value-type "bar"))) #f))
(check (equal? (type-dynamic? (arrow (list (value-type "foo")) (native-type))) #t))

(check (equal? (type-dynamic? (property (string-type) (number-type))) #t))
(check (equal? (type-dynamic? (property (string-type) (value-type 'empty))) #f))

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
    (struct 'foo (list (any-type) (value-type "foo")))
    (struct 'foo (list))))

(check
  (not
    (type-matches?
      (struct 'foo (list (any-type) (value-type "foo")))
      (struct 'bar (list)))))

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
    (arrow (list (any-type) (native-type)) (value-type "foo"))
    (arrow (list (value-type "foo") (native-type)) (value-type "foo"))))

(check
  (type-matches? 
    (arrow (list (any-type) (native-type)) (value-type "foo"))
    (arrow (list (any-type) (native-type)) (any-type))))

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

; --- indexed-type-matching

(let
  (($types
    (list
      (native-type)
      (struct 'x (list))
      (struct 'x (list (native-type))))))
  (check
    (equal?
      (indexed-type-matching $types (native-type))
      (indexed (native-type) 0)))
  (check
    (equal?
      (indexed-type-matching $types (struct 'x (list)))
      (indexed (struct 'x (list)) #f)))
  (check
    (equal?
      (indexed-type-matching $types (struct 'x (list (native-type))))
      (indexed (struct 'x (list (native-type))) 1)))
  (check
    (equal?
      (indexed-type-matching $types (any-type))
      (indexed (native-type) 0)))
  (check
    (equal?
      (indexed-type-matching $types (struct 'x (list (any-type))))
      (indexed (struct 'x (list (native-type))) 1))))

; --- type-application

(check
  (equal?
    (type-application
      (arrow (list (string-type) (number-type)) (boolean-type))
      (list (string-type) (number-type)))
    (boolean-type)))

(check
  (equal?
    (type-application
      (arrow (list (any-type) (number-type)) (boolean-type))
      (list (string-type) (number-type)))
    (boolean-type)))

(check
  (raises?
    (lambda ()
      (type-application
        (arrow (list (string-type) (number-type)) (boolean-type))
        (list (string-type))))))

(check
  (raises?
    (lambda ()
      (type-application
        (arrow (list (string-type) (number-type)) (boolean-type))
        (list (string-type) (any-type))))))

; --- type-ref

(check
  (equal?
    (type-ref
      (struct 'foo
        (list
          (number-type)
          (value-type 'empty)
          (string-type)))
      (string-type))
    (indexed (string-type) 2)))

; --- type-value

(check
  (equal?
    (type-value (value-type (number-type)))
    (number-type)))

(check
  (equal?
    (type-value
      (struct 'foo
        (list
          (value-type (number-type))
          (value-type (string-type)))))
    (struct 'foo
      (list
        (number-type)
        (string-type)))))

(check
  (equal?
    (make-list-of 3 (string-type))
    (arrow
      (list (string-type) (string-type) (string-type))
      (list-of (string-type)))))
