(import
  (micascheme)
  (tico layout)
  (tico type))

(check (equal? (layout-not-empty? (empty-layout)) #f))
(check (equal? (layout-not-empty? (simple-layout)) #t))
(check (equal? (layout-not-empty? (tuple-layout (list))) #t))

; --- literal->layout

(check
  (equal?
    (literal->layout "foo")
    (simple-layout)))

; --- make-struct-layout

(check
  (equal?
    (make-struct-layout
      (stack
        (simple-layout)
        (empty-layout)
        (simple-layout)
        (simple-layout)))
    (struct-layout
      (stack
        (layout-field (simple-layout) 0)
        (layout-field (empty-layout) #f)
        (layout-field (simple-layout) 1)
        (layout-field (simple-layout) 2))
      3)))

; --- layout-abstraction

(check
  (equal?
    (layout-abstraction
      (list (empty-layout) (simple-layout))
      (tuple-layout (list (simple-layout))))
    (lambda-layout
      (list (empty-layout) (simple-layout))
      (tuple-layout (list (simple-layout))))))

(check
  (equal?
    (layout-abstraction
      (list (empty-layout) (simple-layout))
      (empty-layout))
    (empty-layout)))

; --- layout-application

(check
  (equal?
    (layout-application
      (lambda-layout
        (list (empty-layout) (simple-layout))
        (tuple-layout (list (simple-layout))))
      (list (empty-layout) (simple-layout)))
    (tuple-layout (list (simple-layout)))))

(check
  (equal?
    (layout-application
      (empty-layout)
      (list (empty-layout) (simple-layout)))
    (empty-layout)))

; --- type->layout

(check
  (equal?
    (type->layout (value-type "foo"))
    (empty-layout)))

(check
  (equal?
    (type->layout (native-type))
    (simple-layout)))

(check
  (equal?
    (type->layout (type-type))
    (simple-layout)))

(check
  (equal?
    (type->layout (struct 'foo (list)))
    (empty-layout)))

(check
  (equal?
    (type->layout (struct 'foo (list (value-type "foo") (value-type "bar"))))
    (empty-layout)))

(check
  (equal?
    (type->layout (struct 'foo (list (native-type) (value-type "foo") (native-type))))
    (tuple-layout (list (simple-layout) (empty-layout) (simple-layout)))))

(check
  (equal?
    (type->layout (arrow (list (native-type) (value-type "foo") (native-type)) (native-type)))
    (lambda-layout (list (simple-layout) (empty-layout) (simple-layout)) (simple-layout))))

(check
  (equal?
    (type->layout (arrow (list (native-type) (value-type "foo") (native-type)) (value-type "foo")))
    (empty-layout)))
