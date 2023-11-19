(import
  (micascheme)
  (tico layout)
  (tico layment)
  (tico compilation)
  (tico parameter)
  (tico type)
  (tico variable)
  (tico constant))

(check
  (equal?
    (test-layment foo)
    (make-layment
      (simple-layout)
      (test-compilation foo))))

(check
  (equal?
    (test-parameter-layment foo)
    (make-layment
      (simple-layout)
      (test-parameter-compilation foo))))

(check
  (equal?
    (empty-layment)
    (layment (empty-layout) #f)))

(check
  (equal?
    (literal->layment "foo")
    (layment
      (literal->layout "foo")
      (literal->compilation "foo"))))

(check
  (equal?
    (layout-datum->layment (simple-layout) "foo")
    (layment (simple-layout) (datum->compilation "foo"))))

(check
  (equal?
    (bindings-layout-datum->layment
      (stack
        (make-layment (simple-layout) (compilation 'foo (constant "foo")))
        (make-layment (empty-layout) (compilation 'foo (constant "foo")))
        (make-layment (simple-layout) (compilation 'foo (parameter)))
        (make-layment (simple-layout) (compilation 'bar (constant "bar"))))
      (simple-layout)
      '(string-append foo bar))
    (layment
      (simple-layout)
      (bindings-datum->compilation
        (stack
          (compilation 'foo (constant "foo"))
          (compilation 'foo (parameter))
          (compilation 'bar (constant "bar")))
        '(string-append foo bar)))))

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
        (lambda-layout
          (list
            (simple-layout)
            (empty-layout)
            (simple-layout))
          (simple-layout))
        'string-append)
      (list
        (literal->layment "foo")
        (empty-layment)
        (literal->layment "bar")))
    (layout-datum->layment
      (simple-layout)
      '(string-append "foo" "bar"))))

; --- layment-abstraction

(check
  (equal?
    (layment-application
      (layment-abstraction
        (list
          (make-layment (simple-layout) (compilation 'v1 (parameter)))
          (make-layment (simple-layout) (compilation 'v2 (parameter))))
        (make-layment
          (simple-layout)
          (compilation 'v1 (variable 0))))
      (list
        (literal->layment "foo")
        (literal->layment "bar")))
    (make-layment
      (layout-application
        (layout-abstraction
          (list (simple-layout) (simple-layout))
          (simple-layout))
        (list (simple-layout) (simple-layout)))
      (compilation-application
        (compilation-abstraction
          (empty-compilation-scope)
          (list
            (compilation 'v1 (parameter))
            (compilation 'v2 (parameter)))
          (compilation 'v1 (variable 0)))
        (list
          (literal->compilation "foo")
          (literal->compilation "bar"))))))

; --- layment-struct

(check
  (equal?
    (layment-struct 'foo (list))
    (make-layment
      (layout-struct 'foo (list))
      (throw error))))

(check
  (equal?
    (layment-struct 'foo
      (list
        (literal->layment 128)
        (empty-layment)
        (literal->layment "foo")))
    (make-layment
      (layout-struct 'foo
        (list
          (literal->layout 128)
          (empty-layout)
          (literal->layout "foo")))
      (compilation-struct 'foo
        (list
          (literal->compilation 128)
          (literal->compilation "foo"))))))

; --- layment-ref

; (check
;   (equal?
;     (layment-ref
;       (layment-struct 'foo
;         (list
;           (literal->layment "foo")
;           (empty-layment)
;           (literal->layment "bar")))
;       2)
;     (lets
;       ($layout-field
;         (layout-ref
;           (layout-struct 'foo
;             (list
;               (literal->layout "foo")
;               (empty-layout)
;               (literal->layout "bar")))
;           2))
;       (make-layment
;         (layout-field-layout $layout-field)
;         (compilation-ref
;           2
;           (compilation-struct 'foo
;             (list
;               (literal->compilation "foo")
;               (literal->compilation "bar")))
;           (layout-field-index-opt $layout-field))))))

; --- layment parameter

(check
  (equal?
    (with-tmps
      (layment-parameter (literal->layment "foo")))
    (with-tmps
      (make-layment
        (layment-layout (literal->layment "foo"))
        (compilation-parameter
          (layment-compilation (literal->layment "foo")))))))

; --- layment-scope

(lets
  ($scope (empty-layment-scope))
  ($scope (layment-scope-push $scope (layment (simple-layout) (compilation 'v1 (parameter)))))
  ($scope (layment-scope-push $scope (layment (simple-layout) (compilation 'v2 (constant "foo")))))
  ($scope (layment-scope-push $scope (layment (empty-layout) #f)))
  ($scope (layment-scope-push $scope (layment (simple-layout) (compilation 'v3 (constant "bar")))))
  (do
    (check
      (equal?
        (layment-scope-ref $scope 0)
        (layment (simple-layout) (compilation 'v3 (constant "bar"))))))
  (do
    (check
      (equal?
        (layment-scope-ref $scope 1)
        (layment (empty-layout) #f))))
  (do
    (check
      (equal?
        (layment-scope-ref $scope 2)
        (layment (simple-layout) (compilation 'v2 (constant "foo"))))))
  (do
    (check
      (equal?
        (layment-scope-ref $scope 3)
        (layment (simple-layout) (compilation 'v1 (variable 2))))))
  (void))

