(import
  (micascheme)
  (tico arity)
  (tico layout)
  (tico type))

; --- layout-empty?

(check (equal? (layout-empty? (empty-layout)) #t))
(check (equal? (layout-empty? (simple-layout)) #f))
(check (equal? (layout-empty? (native-layout)) #f))

(check (equal? (layout-empty? (struct-layout (stack (empty-layout)) 0)) #t))
(check (equal? (layout-empty? (struct-layout (stack (simple-layout)) 1)) #f))

(check (equal? (layout-empty? (lambda-layout (stack) (empty-layout))) #t))
(check (equal? (layout-empty? (lambda-layout (stack) (simple-layout))) #f))

(check (equal? (layout-not-empty? (empty-layout)) #f))
(check (equal? (layout-not-empty? (simple-layout)) #t))

; --- layout-arity

(check (equal? (layout-arity (empty-layout)) (arity 0)))
(check (equal? (layout-arity (simple-layout)) (arity 1)))
(check (equal? (layout-arity (native-layout)) (arity 1)))

(check (equal? (layout-arity (struct-layout (stack (empty-layout)) 0)) (arity 0)))
(check (equal? (layout-arity (struct-layout (stack (simple-layout)) 1)) (arity 1)))
(check (equal? (layout-arity (struct-layout (stack (simple-layout) (simple-layout)) 2)) (arity 2)))

(check (equal? (layout-arity (lambda-layout (stack) (empty-layout))) (arity 0)))
(check (equal? (layout-arity (lambda-layout (stack) (simple-layout))) (arity 1)))


; --- literal->layout

(check
  (equal?
    (literal->layout "foo")
    (type->layout (string-type))))

; --- layout-args

(check
  (equal?
    (layout-args (list))
    empty-struct-layout))

(check
  (equal?
    (layout-args
      (list
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
      (list (simple-layout)))
    (lambda-layout
      (make-struct-layout (list (empty-layout) (simple-layout)))
      (make-struct-layout (list (simple-layout))))))

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
    (type->layout (unchecked-type))
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
    (type->layout (struct 'foo (list (unchecked-type) (value-type "foo") (unchecked-type))))
    (layout-struct 'foo (list (native-layout) (empty-layout) (native-layout)))))

(check
  (equal?
    (type->layout
      (arrow
        (list
          (value-type "foo")
          (unchecked-type))
        (list
          (unchecked-type))))
    (layout-abstraction
      (list
        (empty-layout)
        (native-layout))
      (list
        (native-layout)))))

(check
  (equal?
    (type->layout
      (property
        (string-type)
        (number-type)))
    (layout-abstraction
      (list (type->layout (string-type)))
      (list (type->layout (number-type))))))

(check
  (equal?
    (type->layout
      (constant-type
        (struct 'foo (list))
        (number-type)))
    (type->layout (number-type))))

; --- layout-ref

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

; --- stack-layout

(lets
  ($scope (empty-stack-layout))
  ($scope (stack-layout-push $scope (simple-layout)))
  ($scope (stack-layout-push $scope (empty-layout)))
  ($scope (stack-layout-push $scope (native-layout)))
  (run
    (check
      (equal?
        (stack-layout-ref $scope 0)
        (layout-field (native-layout) 1)))
    (check
      (equal?
        (stack-layout-ref $scope 1)
        (layout-field (empty-layout) #f)))
    (check
      (equal?
        (stack-layout-ref $scope 2)
        (layout-field (simple-layout) 0)))))

; --- list-layout

(check
  (equal?
    (list-layout
      (list
        (simple-layout)
        (empty-layout)
        (simple-layout)
        (simple-layout)))
    (struct-layout
      (list
        (layout-field (simple-layout) 0)
        (layout-field (empty-layout) #f)
        (layout-field (simple-layout) 1)
        (layout-field (simple-layout) 2))
      3)))
