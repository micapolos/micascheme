(import (micascheme) (tico type))

(check
  (equal?
    (test-type a)
    (struct 'a (list (unchecked-type)))))

(check
  (equal?
    (static-test-type a)
    (struct 'a (list))))

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
      (unchecked-type)
      (value-type "foo"))))

(check
  (type-matches? 
    (unchecked-type)
    (unchecked-type)))

(check
  (not
    (type-matches? 
      (value-type "foo")
      (unchecked-type))))

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
    (struct 'foo (list (any-type) (unchecked-type)))
    (struct 'foo (list (any-type) (unchecked-type)))))

(check
  (type-matches? 
    (struct 'foo (list (value-type "foo") (unchecked-type)))
    (struct 'foo (list (any-type) (unchecked-type)))))

(check
  (not
    (type-matches? 
      (struct 'bar (list (any-type) (unchecked-type)))
      (struct 'foo (list (any-type) (unchecked-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type)))
      (struct 'foo (list (any-type) (unchecked-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type) (unchecked-type)))
      (struct 'foo (list (any-type))))))

(check
  (not
    (type-matches? 
      (struct 'foo (list (any-type) (value-type "foo")))
      (struct 'foo (list (any-type) (unchecked-type))))))

(check
  (not
    (type-matches? 
      (unchecked-type)
      (struct 'foo (list (any-type) (unchecked-type))))))

(check
  (type-matches? 
    (arrow (list (any-type) (unchecked-type)) (value-type "foo"))
    (arrow (list (any-type) (unchecked-type)) (value-type "foo"))))

(check
  (type-matches? 
    (arrow (list (any-type) (unchecked-type)) (value-type "foo"))
    (arrow (list (value-type "foo") (unchecked-type)) (value-type "foo"))))

(check
  (type-matches? 
    (arrow (list (any-type) (unchecked-type)) (value-type "foo"))
    (arrow (list (any-type) (unchecked-type)) (any-type))))

(check
  (not
    (type-matches? 
      (arrow (list (any-type) (value-type "foo")) (value-type "foo"))
      (arrow (list (any-type) (unchecked-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (arrow (list (any-type) (unchecked-type)) (value-type "bar"))
      (arrow (list (any-type) (unchecked-type)) (value-type "foo")))))

(check
  (not
    (type-matches? 
      (unchecked-type)
      (arrow (list (any-type) (unchecked-type)) (value-type "foo")))))

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

; --- type-line

(check (equal? (type-line (boolean-type)) 'boolean))
(check (equal? (type-line (number-type)) 'number))
(check (equal? (type-line (string-type)) 'string))
(check (equal? (type-line (char-type)) 'char))
(check (equal? (type-line (symbol-type)) 'symbol))

(check (equal? (type-line (struct 'foo (list))) 'foo))
(check 
  (equal? 
    (type-line 
      (struct 'foo 
        (list
          (string-type) 
          (number-type))))
    '(foo string number)))
