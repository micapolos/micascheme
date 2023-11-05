(import
  (micascheme)
  (tico layout)
  (tico layment)
  (tico compilation))

(check
  (equal?
    (empty-layment)
    (layment (empty-layout) #f)))

(check
  (equal?
    (literal->layment "foo")
    (layment
      (simple-layout)
      (literal->compilation "foo"))))

(check
  (equal?
    (layout-datum->layment (simple-layout) "foo")
    (layment (simple-layout) (datum->compilation "foo"))))

(check
  (equal?
    (make-layment (empty-layout)
      (throw error))
    (layment (empty-layout) #f)))

(check
  (equal?
    (make-layment (simple-layout)
      (literal->compilation "foo"))
    (layment
      (simple-layout)
      (literal->compilation "foo"))))

(check
  (equal?
    (layment-datum
      (layout-datum->layment
        (type->layout (number-type))
        '(+ 1 2)))
    '(+ 1 2)))

(check
  (equal?
    (layment-value
      (layout-datum->layment
        (type->layout (number-type))
        '(+ 1 2)))
    3))

; --- layment-application

(check
  (equal?
    (layment-application
      (layout-datum->layment
        (lambda-layout (list (simple-layout) (simple-layout)) (simple-layout))
        'string-append)
      (list
        (literal->layment "foo")
        (literal->layment "bar")))
    (layout-datum->layment
      (simple-layout)
      '(string-append "foo" "bar"))))

; --- layment-struct

(check
  (equal?
    (layment-struct 'foo (list))
    (make-layment (empty-layout) #f)))

(check
  (equal?
    (layment-struct 'foo
      (list
        (literal->layment 128)
        (literal->layment "foo")))
    (make-layment
      (layout-struct 'foo
        (list
          (literal->layout 128)
          (literal->layout "foo")))
      (compilation-struct 'foo
        (list
          (literal->compilation 128)
          (literal->compilation "foo"))))))
