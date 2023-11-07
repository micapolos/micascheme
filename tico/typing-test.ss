(import
  (micascheme)
  (tico typing)
  (tico type)
  (tico layment)
  (tico layout)
  (tico compilation)
  (tico variable))

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
        (arrow (list (number-type)) (string-type))
        'number->string)
      (list
        (literal->typing 10)))
    (typing
      (type-application
        (arrow (list (number-type)) (string-type))
        (list (literal->type 10)))
      (layment-application
        (layout-datum->layment
          (type->layout (arrow (list (number-type)) (string-type)))
          'number->string)
        (list (literal->layment 10))))))

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
    (typings-get
      (stack
        (typing-struct 'point
          (list
            (typing-struct 'x (list (literal->typing 10)))
            (typing-struct 'y (list (literal->typing 20))))))
      (stack
        (typing-struct 'x (list))
        (static-typing (value-type (number-type)))))
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
    (typings-promising
      (stack
        (number-typing)
        (string-typing))
      (boolean-typing))
    (type->typing
      (arrow
        (list
          (number-type)
          (string-type))
        (boolean-type)))))

(check
  (equal?
    (typings-offering
      (stack (number-typing) (string-typing))
      (stack (number-typing) (boolean-typing)))
    (stack
      (type->typing (property (number-type) (number-type)))
      (type->typing (property (number-type) (boolean-type)))
      (type->typing (property (string-type) (number-type)))
      (type->typing (property (string-type) (boolean-type))))))
