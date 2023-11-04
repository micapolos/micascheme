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
    (make-layment ($layout (empty-layout))
      (throw error))
    (layment (empty-layout) #f)))

(check
  (equal?
    (make-layment ($layout (simple-layout))
      (literal->compilation "foo"))
    (layment
      (simple-layout)
      (literal->compilation "foo"))))

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
