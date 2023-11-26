(import
  (micascheme)
  (tico typing)
  (tico type)
  (tico layment)
  (tico layout)
  (tico compilation)
  (tico variable)
  (tico datum)
  (leo writing-reader))

(check
  (equal?
    (test-typing foo)
    (type-datum->typing
      (test-type foo)
      (test-datum foo))))

(check
  (equal?
    (test-static-typing foo)
    (type-datum->typing
      (static-test-type foo)
      (test-datum foo))))

(check
  (equal?
    (static-typing (value-type "foo"))
    (typing
      (value-type "foo")
      (layment (empty-layout) #f))))

(check
  (equal?
    (literal->typing "foo")
    (typing
      (literal->type "foo")
      (literal->layment "foo"))))

(check
  (equal?
    (variable-typing (boolean-type) 'foo 1)
    (typing (boolean-type)
      (variable-layment (type->layout (boolean-type))
        'foo 1))))

(check
  (equal?
    (type->typing (number-type))
    (static-typing (value-type (number-type)))))

(check (equal? (boolean-typing) (type->typing (boolean-type))))
(check (equal? (number-typing) (type->typing (number-type))))
(check (equal? (string-typing) (type->typing (string-type))))

(check
  (equal?
    (type-datum->typing
      (string-type)
      '(string-append "foo" "bar"))
    (typing
      (string-type)
      (layout-datum->layment
        (type->layout (string-type))
        '(string-append "foo" "bar")))))

(check
  (equal?
    (typing-datum
      (type-datum->typing (number-type) '(+ 1 2)))
    '(+ 1 2)))

(check
  (equal?
    (typing-value
      (type-datum->typing (number-type) '(+ 1 2)))
    3))

(check
  (equal?
    (typing-application
      (type-datum->typing
        (arrow
          (list
            (number-type)
            (struct 'foo (list)))
          (list
            (string-type)))
        'number->string)
      (list
        (literal->typing 10)
        (typing-struct 'foo (list))))
    (typing
      (type-application
        (arrow
          (list
            (number-type)
            (struct 'foo (list)))
          (list
            (string-type)))
        (list
          (literal->type 10)
          (struct 'foo (list))))
      (layment-application
        (layout-datum->layment
          (type->layout
            (arrow
              (list
                (number-type)
                (struct 'foo (list)))
              (list
                (string-type))))
          'number->string)
        (list
          (literal->layment 10)
          (layment-struct 'foo (list)))))))

(check
  (equal?
    (typing-abstraction
      (list
        (parameter-typing (string-type) '$string)
        (parameter-typing (struct 'exclamate (list)) '$empty))
      (typing-struct 'exclamated
        (list
          (typing-variable (parameter-typing (string-type) '$string) 1))))
    (typing
      (type-abstraction
        (list
          (typing-type (parameter-typing (string-type) '$string))
          (typing-type (parameter-typing (struct 'exclamate (list)) '$empty)))
        (list
          (typing-type
            (typing-struct 'exclamated
              (list
                (typing-variable (parameter-typing (string-type) '$string) 1))))))
      (layment-abstraction
        (list
          (typing-layment (parameter-typing (string-type) '$string))
          (typing-layment (parameter-typing (struct 'exclamate (list)) '$empty)))
        (list
          (typing-layment
            (typing-struct 'exclamated
              (list
                (typing-variable (parameter-typing (string-type) '$string) 1)))))))))

(check
  (equal?
    (typing-prepare
      (type-datum->typing
        (number-type)
        '(+ 1 2)))
    (type-datum->typing
      (number-type)
      3)))

(check
  (equal?
    (typing-ref-index
      (typing-struct 'foo
        (list
          (literal->typing "foo")
          (static-typing (value-type 'empty))
          (literal->typing 10)))
      2)
    (typing
      (number-type)
      (layment-ref
        (layment-struct 'foo
          (list
            (literal->layment "foo")
            (empty-layment)
            (literal->layment 10)))
        2))))

(check
  (equal?
    (typing-ref
      (typing-struct 'foo
        (list
          (literal->typing "foo")
          (static-typing (value-type 'empty))
          (literal->typing 10)))
      (number-type))
    (typing
      (number-type)
      (layment-ref
        (layment-struct 'foo
          (list
            (literal->layment "foo")
            (empty-layment)
            (literal->layment 10)))
        2))))

(check
  (equal?
    (typing-get
      (typing-struct 'point
        (list
          (typing-struct 'x (list (literal->typing 10)))
          (typing-struct 'y (list (literal->typing 20)))))
      (list
        (struct 'x (list))
        (number-type)))
    (typing-ref
      (typing-ref
        (typing-struct 'point
          (list
            (typing-struct 'x (list (literal->typing 10)))
            (typing-struct 'y (list (literal->typing 20)))))
        (struct 'x (list)))
      (number-type))))

; --- typing-resolve

(check
  (equal?
    (typing-resolve (static-typing (struct 'boolean (list))))
    (boolean-typing)))

(check
  (equal?
    (typing-resolve (static-typing (struct 'number (list))))
    (number-typing)))

(check
  (equal?
    (typing-resolve (static-typing (struct 'string (list))))
    (string-typing)))

(check
  (equal?
    (typing-resolve (literal->typing 10))
    (literal->typing 10)))

(check
  (equal?
    (typing-as
      (native->typing '(+ 1 2))
      (number-typing))
    (type-datum->typing
      (number-type)
      '(+ 1 2))))

(check
  (equal?
    (typing-promising
      (list
        (number-typing)
        (string-typing))
      (boolean-typing))
    (type->typing
      (arrow
        (list
          (number-type)
          (string-type))
        (list
          (boolean-type))))))

(check
  (equal?
    (typing-offering
      (number-typing)
      (boolean-typing))
    (type->typing
      (property
        (number-type)
        (boolean-type)))))

(check
  (equal?
    (typing-argument
      (type->typing (struct 'foo (list)))
      (literal->typing 123))
    (typing
      (argument-type
        (struct 'foo (list))
        (number-type))
      (typing-layment
        (literal->typing 123)))))

(check
  (equal?
    (typing-being
      (type->typing (struct 'foo (list)))
      (type->typing (number-type)))
    (type->typing
      (argument-type
        (struct 'foo (list))
        (number-type)))))

(check
  (equal?
    (typing-argument-access
      (typing-argument
        (type->typing (struct 'foo (list)))
        (literal->typing 123))
      (typing-struct 'foo (list)))
    (literal->typing 123)))

(check
  (raises?
    (lambda ()
      (typing-argument-access
        (typing-argument
          (type->typing (struct 'foo (list)))
          (literal->typing 123))
        (typing-struct 'bar (list))))))

(check
  (equal?
    (typing->type-typing
      (typing-struct 'foo (list (string-typing))))
    (type-datum->typing
      (type-type)
      (value->datum
        (struct 'foo (list (string-type)))))))

(check
  (equal?
    (make-list-typing 3 (string-type))
    (type-datum->typing
      (make-list-of 3 (string-type))
      'list)))

(check
  (equal?
    (make-struct-typing)
    (type-datum->typing
      (make-struct-type)
      'struct)))

; --- typing-assert

(check
  (equal?
    (typing-assert (type-datum->typing (boolean-type) '(= 1 1)))
    (void)))

; not boolean
(check
  (raises?
    (lambda ()
      (typing-assert (type-datum->typing (string-type) "foo")))))

; not true
(check
  (raises?
    (lambda ()
      (typing-assert (type-datum->typing (boolean-type) '(= 1 2))))))

; non value
(check
  (raises?
    (lambda ()
      (typing-assert (variable-typing (boolean-type) 'foo 1)))))

; --- typings-resolve-get

(check
  (equal?
    (typings-resolve-get
      (stack
        (typing-struct 'foo
          (list
            (literal->typing "foo")
            (typing-struct 'x (list (literal->typing 10)))))
        (typing-struct 'get
          (list
            (type->typing (string-type))))))
    (typing-ref
      (typing-struct 'foo
        (list
          (literal->typing "foo")
          (typing-struct 'x (list (literal->typing 10)))))
      (string-type))))

(check
  (equal?
    (typings-resolve-get
      (stack
        (typing-struct 'foo
          (list
            (literal->typing "foo")
            (typing-struct 'x (list (literal->typing 10)))))
        (typing-struct 'get
          (list
            (typing-struct 'x (list))))))
    (typing-ref
      (typing-struct 'foo
        (list
          (literal->typing "foo")
          (typing-struct 'x (list (literal->typing 10)))))
      (struct 'x (list)))))

; --- typing-line

(check
  (equal?
    (typing-line (literal->typing 10))
    10))

(check
  (equal?
    (typing-line (literal->typing "foo"))
    "foo"))

(check
  (equal?
    (typing-line (typing-struct 'foo (list)))
    'foo))

; --- typings-string

(check
  (equal?
    (typing-string
      (typing-struct 'foo
        (list
          (literal->typing 20)
          (literal->typing 30))))
    (script-string (foo 20 30))))

; --- stack-typing

(lets
  ($scope
    (stack-typing
      (test-parameter-typing t1)
      (test-typing t2)
      (test-static-typing t3)
      (test-parameter-typing t4)))
  (do
    (check
      (equal?
        (stack-typing-ref $scope 0)
        (typing-variable (test-parameter-typing t4) 0))))
  (do
    (check
      (equal?
        (stack-typing-ref $scope 1)
        (typing-variable (test-static-typing t3) #f))))
  (do
    (check
      (equal?
        (stack-typing-ref $scope 2)
        (typing-variable (test-typing t2) 1))))
  (do
    (check
      (equal?
        (stack-typing-ref $scope 3)
        (typing-variable (test-parameter-typing t1) 2))))

  (do
    (check
      (equal?
        (stack-typing-type-ref $scope (test-type t2))
        (stack-typing-ref $scope 2))))
  (void))
