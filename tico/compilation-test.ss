(import
  (micascheme)
  (tico compilation)
  (tico argument)
  (tico variable)
  (tico datum)
  (tico evaluation)
  (tico variable)
  (tico definition)
  (tico parameter))

(check
  (equal?
    (test-compilation foo)
    (datum->compilation ''foo)))

(check
  (equal?
    (compilation-value (compilation 'foo (argument 3)))
    3))

(check
  (raises?
    (lambda ()
      (compilation-value (compilation 'foo (variable 1 (stack)))))))

(check
  (raises?
    (lambda ()
      (compilation-value (compilation 'foo (parameter))))))

(check
  (equal?
    (datum->compilation '(string-append "foo" "bar"))
    (compilation
      '(string-append "foo" "bar")
      (datum->argument '(string-append "foo" "bar")))))

(check
  (equal?
    (bindings-datum->compilation
      (stack
        (compilation 'foo (argument "foo"))
        (compilation 'goo (parameter))
        (compilation 'bar (argument "bar")))
      '(string-append "foo" "bar"))
    (compilation
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
        (compilation 'foo (argument "foo"))
        (compilation 'goo (parameter))
        (compilation 'bar (argument "bar")))
      '(string-append foo bar))
    (compilation
      '(string-append foo bar)
      (bindings-datum->argument
        (stack
          (cons 'foo "foo")
          (cons 'bar "bar"))
        '(string-append foo bar)))))

(check
  (equal?
    (literal->compilation "foo")
    (compilation "foo" (argument "foo"))))

; --- compilation-args-application

(check
  (equal?
    (compilation-args-application
      (empty-stack-compilation)
      (compilation 'string-append (variable 1))
      (compilation-args
        (list
          (literal->compilation "foo")
          (literal->compilation "bar"))))
    (compilation
      (datum-args-application
        'string-append
        (datum-args (list "foo" "bar")))
      (variable 1))))

; --- compilation-application

(check
  (equal?
    (compilation-application
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
    (with-generate-temporary-seed $tmp
      (compilation-application
        (datum->compilation 'string-append)
        (list
          (compilation 'foo (variable 1))
          (literal->compilation "bar"))))
    (compilation
      (datum-application
        'string-append
        (list 'foo "bar"))
      (variable 1))))

; --- compilation-abstraction

(check
  (equal?
    (compilation-application
      (compilation-abstraction
        (stack-compilation
          (compilation 'excl (argument "!")))
        (list
          (compilation 'foo (parameter))
          (compilation 'bar (parameter)))
        (compilation-struct 'foo
          (list
            (compilation 'foo (variable 1))
            (compilation 'bar (variable 0))
            (compilation 'excl (argument "!")))))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (scope-datum->compilation
      (stack-compilation
        (compilation 'excl (argument "!")))
      (datum-application
        (datum-abstraction
          (list 'foo 'bar)
          (datum-struct 'foo (list 'foo 'bar 'excl)))
        (list "foo" "bar")))))

(check
  (equal?
    (compilation-application
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (compilation 'v1 (parameter))
          (compilation 'v2 (parameter)))
        (literal->compilation "foobar"))
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
      (compilation-abstraction
        (empty-stack-compilation)
        (list
          (compilation 'v1 (parameter))
          (compilation 'v2 (parameter)))
        (compilation
          '(string-append v1 v2)
          (variable 1)))
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (compilation-application
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
        (compilation 'v1 (parameter))
        (compilation 'v2 (parameter)))
      (compilation
        '(string-append v1 v2)
        (variable 3)))
    (compilation
      (datum-abstraction
        (list 'v1 'v2)
        '(string-append v1 v2))
      (variable 1))))

; --- compilation-args

(check
  (equal?
    (compilation-args
      (list
        (compilation 'foo (argument "foo"))
        (compilation 'bar (argument "bar"))))
    (compilation
      (datum-args (list 'foo 'bar))
      (argument (list "foo" "bar")))))

(check
  (equal?
    (compilation-args
      (list
        (compilation 'foo (argument "foo"))
        (compilation 'bar (variable 1))
        (compilation 'goo (parameter))))
    (compilation
      (datum-args (list 'foo 'bar 'goo))
      (parameter))))

(check
  (equal?
    (compilation-args
      (list
        (compilation 'foo (argument "foo"))
        (compilation 'bar (variable 1))
        (compilation 'goo (variable 2))))
    (compilation
      (datum-args (list 'foo 'bar 'goo))
      (variable 2))))

; --- compilation-struct

(check
  (equal?
    (compilation-struct 'x
      (list
        (compilation "foo" (argument "foo"))
        (compilation "bar" (argument "bar"))))
    (compilation
      (datum-struct 'x (list "foo" "bar"))
      (argument-struct 'x (list (argument "foo") (argument "bar"))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-struct 'x
        (list
          (compilation 'foo (variable 1))
          (compilation '(identity "foo") (argument "foo"))
          (compilation 'bar (variable 2))
          (compilation '(identity "bar") (argument "bar")))))
    (compilation
      (datum-struct 'x
        (list 'foo '(identity "foo") 'bar '(identity "bar")))
      (variable
        (variable-index-flatten (list 1 2))))))

; --- generate-parameter-compilation

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (literal->compilation "foo")))
    (with-tmps
      (compilation (generate-symbol)
        (compilation-evaluation (literal->compilation "foo"))))))

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (datum->compilation 'string-append)))
    (with-tmps
      (compilation (generate-symbol)
        (compilation-evaluation (datum->compilation 'string-append))))))

(check
  (equal?
    (with-tmps
      (compilation-parameter
        (compilation 'foo (variable 3))))
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
      (compilation 'v1 (parameter))
      (compilation 'v2 (argument "foo"))
      (compilation 'v3 (argument "bar"))))
  (do
    (check
      (equal?
        (stack-compilation-ref $scope 0)
        (compilation 'v3 (argument "bar")))))
  (do
    (check
      (equal?
        (stack-compilation-ref $scope 1)
        (compilation 'v2 (argument "foo")))))
  (do
    (check
      (equal?
        (stack-compilation-ref $scope 2)
        (compilation 'v1 (variable 2)))))
  (do
    (check
      (equal?
        (stack-compilation-bindings $scope)
        (stack
          (cons 'v2 "foo")
          (cons 'v3 "bar")))))
  (void))

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
      (compilation 'foo (variable 1)))
    (compilation
      '(let ((foo "foo") (bar "bar")) foo)
      (variable 1))))
