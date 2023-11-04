(import
  (micascheme)
  (tico typing))

(check
  (equal?
    (literal->typing "foo")
    (typing
      (literal-type "foo")
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
