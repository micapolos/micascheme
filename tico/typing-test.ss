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
