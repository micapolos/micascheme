(import
  (micascheme)
  (tico arity)
  (tico compilation)
  (tico argument)
  (tico variable)
  (tico datum)
  (tico evaluation)
  (tico variable)
  (tico definition)
  (tico parameter)
  (tico bimbing)
  (tico paco))

(check
  (equal?
    (test-compilation foo)
    (datum->compilation ''foo)))

(check
  (equal?
    (compilation-value (argument-compilation 'foo (argument 3)))
    3))

(check
  (raises?
    (lambda ()
      (compilation-value (variable-compilation 'foo 1)))))

(check
  (raises?
    (lambda ()
      (compilation-value (parameter-compilation 'foo)))))

(check
  (equal?
    (datum->compilation '(string-append "foo" "bar"))
    (compilation
      (arity 1)
      '(string-append "foo" "bar")
      (datum->argument '(string-append "foo" "bar")))))

(check
  (equal?
    (bindings-datum->compilation
      (stack
        (argument-compilation 'foo (argument "foo"))
        (parameter-compilation 'goo)
        (argument-compilation 'bar (argument "bar")))
      '(string-append "foo" "bar"))
    (compilation
      (arity 1)
      '(string-append "foo" "bar")
      (bindings-datum->argument
        (stack
          (cons 'foo "foo")
          (cons 'bar "bar"))
        '(string-append "foo" "bar")))))

(check
  (equal?
    (scope-datum->compilation
      (stack-compilation
        (argument-compilation 'foo (argument "foo"))
        (parameter-compilation 'goo)
        (argument-compilation 'bar (argument "bar")))
      '(string-append foo bar))
    (compilation
      (arity 1)
      '(string-append foo bar)
      (bindings-datum->argument
        (stack
          (cons 'foo "foo")
          (cons 'bar "bar"))
        '(string-append foo bar)))))

(check
  (equal?
    (literal->compilation "foo")
    (compilation (arity 1) "foo" (argument "foo"))))

; --- compilation-application-datum

(check
  (equal?
    (compilation-arity-datum
      (argument-compilation '(foo) (argument)))
    '(0 (foo))))

(check
  (equal?
    (compilation-arity-datum
      (argument-compilation '(foo) (argument "foo")))
    '(1 (foo))))

(check
  (equal?
    (compilation-arity-datum
      (argument-compilation '(foo) (argument "foo" "bar")))
    '(2 (foo))))

; --- compilation-application

(check
  (equal?
    (compilation-application
      (arity 1)
      (datum->compilation 'string-append)
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (datum->compilation
      (datum-application
        'string-append
        (list "foo" "bar")))))

(check
  (equal?
    (compilation-application
      (arity 1)
      (datum->compilation 'string-append)
      (list
        (datum->compilation '(values))
        (datum->compilation "foo")
        (datum->compilation '(values "bar" "gar"))))
    (datum->compilation
      (datum-values-application
        (compilation-datum (datum->compilation 'string-append))
        (list
          (compilation-arity-datum (datum->compilation '(values)))
          (compilation-arity-datum (datum->compilation "foo"))
          (compilation-arity-datum (datum->compilation '(values "bar" "gar"))))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-application
        (arity 1)
        (datum->compilation 'string-append)
        (list
          (variable-compilation 'foo 1)
          (literal->compilation "bar"))))
    (variable-compilation
      (datum-application
        'string-append
        (list 'foo "bar"))
      1)))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-application
        (arity 1)
        (parameter-compilation 'string-append)
        (list
          (variable-compilation 'foo 1)
          (literal->compilation "bar"))))
    (parameter-compilation
      (datum-application
        'string-append
        (list 'foo "bar")))))

; --- compilation-abstraction

(check
  (equal?
    (compilation-application
      (arity 1)
      (compilation-abstraction
        (stack-compilation
          (argument-compilation 'excl (argument "!")))
        (list
          (parameter-compilation 'foo)
          (parameter-compilation 'bar))
        (list
          (compilation-struct 'foo
            (list
              (variable-compilation 'foo 1)
              (variable-compilation 'bar 0)
              (argument-compilation 'excl (argument "!"))))))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (scope-datum->compilation
      (stack-compilation
        (argument-compilation 'excl (argument "!")))
      (datum-application
        (datum-abstraction
          (list 'foo 'bar)
          (datum-struct 'foo (list 'foo 'bar 'excl)))
        (list "foo" "bar")))))

(check
  (equal?
    (compilation-application
      (arity 1)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (literal->compilation "foobar")))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (datum->compilation
      (datum-application
        (datum-abstraction
          (list 'v1 'v2)
          "foobar")
        (list "foo" "bar")))))

(check
  (equal?
    (compilation-application
      (arity 1)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (variable-compilation '(string-append v1 v2) 1)))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (compilation-application
      (arity 1)
      (datum->compilation
        (datum-abstraction
          (list 'v1 'v2)
          '(string-append v1 v2)))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))))

(check
  (equal?
    (compilation-abstraction
      (empty-stack-compilation)
      (list
        (parameter-compilation 'v1)
        (parameter-compilation 'v2))
      (list
        (variable-compilation '(string-append v1 v2) 3)))
    (variable-compilation
      (datum-abstraction
        (list 'v1 'v2)
        '(string-append v1 v2))
      1)))

(check
  (equal?
    (compilation-abstraction
      (empty-stack-compilation)
      (list
        (parameter-compilation 'v1)
        (parameter-compilation 'v2))
      (list
        (variable-compilation '(string-append v1 v2) 3)))
    (variable-compilation
      (datum-abstraction
        (list 'v1 'v2)
        '(string-append v1 v2))
      1)))

(check
  (equal?
    (compilation-application
      (arity 2)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (literal->compilation "foo")
          (literal->compilation "bar")))
      (list
        (literal->compilation "v1")
        (literal->compilation "v2")))
    (compilation
      (arity 2)
      '((lambda (v1 v2) (values "foo" "bar")) "v1" "v2")
      (argument "foo" "bar"))))

(check
  (equal?
    (compilation-application
      (arity 2)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (variable-compilation 'v1 1)
          (variable-compilation 'v2 0)))
      (list
        (literal->compilation "v1")
        (literal->compilation "v2")))
    (compilation
      (arity 2)
      '((lambda (v1 v2) (values v1 v2)) "v1" "v2")
      (argument "v1" "v2"))))

(check
  (equal?
    (compilation-application
      (arity 2)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (variable-compilation 'v3 3)
          (variable-compilation 'v4 2)))
      (list
        (literal->compilation "v1")
        (literal->compilation "v2")))
    (compilation
      (arity 2)
      '((lambda (v1 v2) (values v3 v4)) "v1" "v2")
      (variable 1))))

(check
  (equal?
    (compilation-application
      (arity 2)
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (parameter-compilation 'v1)
          (parameter-compilation 'v2))
        (list
          (parameter-compilation 'v3)
          (parameter-compilation 'v4)))
      (list
        (literal->compilation "v1")
        (literal->compilation "v2")))
    (compilation
      (arity 2)
      '((lambda (v1 v2) (values v3 v4)) "v1" "v2")
      (parameter))))

; --- compilation-args

(check
  (equal?
    (compilation-args
      (list
        (argument-compilation 'foo (argument "foo"))
        (argument-compilation 'bar (argument "bar"))))
    (argument-compilation
      (datum-args (list 'foo 'bar))
      (argument (list "foo" "bar")))))

(check
  (equal?
    (compilation-args
      (list
        (argument-compilation 'foo (argument "foo"))
        (variable-compilation 'bar 1)
        (parameter-compilation 'goo)))
    (parameter-compilation
      (datum-args (list 'foo 'bar 'goo)))))

(check
  (equal?
    (compilation-args
      (list
        (argument-compilation 'foo (argument "foo"))
        (variable-compilation 'bar 1)
        (variable-compilation 'goo 2)))
    (variable-compilation
      (datum-args (list 'foo 'bar 'goo))
      2)))

; --- compilation-struct

(check
  (equal?
    (compilation-struct 'x
      (list
        (argument-compilation "foo" (argument "foo"))
        (argument-compilation "bar" (argument "bar"))))
    (argument-compilation
      (datum-struct 'x (list "foo" "bar"))
      (argument-struct 'x (list (argument "foo") (argument "bar"))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-struct 'x
        (list
          (variable-compilation 'foo 1)
          (argument-compilation '(identity "foo") (argument "foo"))
          (variable-compilation 'bar 2)
          (argument-compilation '(identity "bar") (argument "bar")))))
    (variable-compilation
      (datum-struct 'x
        (list 'foo '(identity "foo") 'bar '(identity "bar")))
      (variable-index-flatten (list 1 2)))))

; --- generate-parameter-compilation

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (literal->compilation "foo")))
    (with-tmps
      (compilation
        (arity 1)
        (generate-symbol)
        (compilation-evaluation (literal->compilation "foo"))))))

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (datum->compilation 'string-append)))
    (with-tmps
      (compilation
        (arity 1)
        (generate-symbol)
        (compilation-evaluation (datum->compilation 'string-append))))))

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (variable-compilation 'foo 3)))
    (with-tmps
      (generate-parameter-compilation))))

; --- stack-compilation

(check
  (equal?
    (test-stack-compilation foo bar)
    (stack-compilation
      (test-compilation foo)
      (test-compilation bar))))

(lets
  ($scope
    (stack-compilation
      (parameter-compilation 'v1)
      (argument-compilation 'v2 (argument "foo"))
      (argument-compilation 'v3 (argument "bar"))))
  (run
    (check
      (equal?
        (stack-compilation-ref $scope 0)
        (argument-compilation 'v3 (argument "bar"))))
    (check
      (equal?
        (stack-compilation-ref $scope 1)
        (argument-compilation 'v2 (argument "foo"))))
    (check
      (equal?
        (stack-compilation-ref $scope 2)
        (variable-compilation 'v1 2)))
    (check
      (equal?
        (stack-compilation-bindings $scope)
        (stack
          (cons 'v2 "foo")
          (cons 'v3 "bar"))))))

(check
  (equal?
    (compilation-definitions-do
      (list
        (definition
          (parameter-compilation 'foo)
          (datum->compilation "foo"))
        (definition
          (parameter-compilation 'bar)
          (datum->compilation "bar")))
      (variable-compilation 'foo 1))
    (variable-compilation
      '(let ((foo "foo") (bar "bar")) foo)
      1)))

; --- compilation-datum-bimbing

(check
  (equal?
    (compilation-datum-bimbing
      (bimbing
        (compilation
          (arity 3)
          '(values a b c)
          (parameter))
        (compilation
          (arity 3)
          '(three-values)
          (variable 1))))
    (bimbing
      '(values a b c)
      '(three-values))))
