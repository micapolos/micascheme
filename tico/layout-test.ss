(import
  (micascheme)
  (tico layout)
  (tico type))

(check (equal? (layout-empty? (empty-layout)) #t))
(check (equal? (layout-empty? (simple-layout)) #f))
(check (equal? (layout-empty? (native-layout)) #f))
(check (equal? (layout-empty? (struct-layout (stack (empty-layout)) 0)) #t))
(check (equal? (layout-empty? (struct-layout (stack (simple-layout)) 1)) #f))
(check (equal? (layout-empty? (lambda-layout (stack) (empty-layout))) #t))
(check (equal? (layout-empty? (lambda-layout (stack) (simple-layout))) #f))

(check (equal? (layout-not-empty? (empty-layout)) #f))
(check (equal? (layout-not-empty? (simple-layout)) #t))

; --- literal->layout

(check
  (equal?
    (literal->layout "foo")
    (type->layout (string-type))))

; --- make-struct-layout

(check
  (equal?
    empty-struct-layout
    (struct-layout (stack) 0)))

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
      (simple-layout))
    (lambda-layout
      (make-struct-layout (list (empty-layout) (simple-layout)))
      (simple-layout))))

; --- layout-application

(check
  (equal?
    (layout-application
      (lambda-layout
        (list (empty-layout) (simple-layout))
        (make-struct-layout (stack (simple-layout))))
      (list (empty-layout) (simple-layout)))
    (make-struct-layout (stack (simple-layout)))))

; --- type->layout

(check
  (equal?
    (type->layout (value-type "foo"))
    (empty-layout)))

(check
  (equal?
    (type->layout (native-type))
    (native-layout)))

(check
  (equal?
    (type->layout (type-type))
    (simple-layout)))

(check
  (equal?
    (type->layout (struct 'foo (list)))
    (layout-struct 'foo (list))))

(check
  (equal?
    (type->layout (struct 'foo (list (value-type "foo") (value-type "bar"))))
    (layout-struct 'foo (list (empty-layout) (empty-layout)))))

(check
  (equal?
    (type->layout (struct 'foo (list (native-type) (value-type "foo") (native-type))))
    (layout-struct 'foo (list (native-layout) (empty-layout) (native-layout)))))

(check
  (equal?
    (type->layout
      (arrow
        (list
          (value-type "foo")
          (native-type))
        (native-type)))
    (lambda-layout
      (make-struct-layout
        (list
          (empty-layout)
          (native-layout)))
      (native-layout))))

(check
  (equal?
    (layout-ref
      (layout-struct 'foo
        (list
          (literal->layout "foo")
          (empty-layout)
          (literal->layout 10)))
      0)
    (layout-field
      (literal->layout "foo")
      0)))

(check
  (equal?
    (layout-ref
      (layout-struct 'foo
        (list
          (literal->layout "foo")
          (empty-layout)
          (literal->layout 10)))
      1)
    (layout-field
      (empty-layout)
      #f)))

(check
  (equal?
    (layout-ref
      (layout-struct 'foo
        (list
          (literal->layout "foo")
          (empty-layout)
          (literal->layout 10)))
      2)
    (layout-field
      (literal->layout 10)
      1)))
