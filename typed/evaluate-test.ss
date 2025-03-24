(import
  (micascheme)
  (typed lang)
  (typed typed)
  (typed evaluate)
  (typed scope)
  (typed thunk)
  (typed compiled)
  (typed hole)
  (typed combo)
  (any))

(data a-procedure)
(define-aux-keyword fixnum)

(define $environment (environment '(scheme)))

(define (test-evaluate-typed $environment $scope $syntax)
  (syntax-case $syntax (any-type any-boolean any-char any-string any-number)
    (any-type
      (typed any-type (combo any-type 'any-type)))
    (any-boolean
      (typed any-type (combo any-boolean 'any-boolean)))
    (any-char
      (typed any-type (combo any-char 'any-char)))
    (any-string
      (typed any-type (combo any-string 'any-string)))
    (any-number
      (typed any-type (combo any-number 'any-number)))
    (x
      (boolean? (datum x))
      (typed any-boolean (combo (datum x) (datum x))))
    (x
      (char? (datum x))
      (typed any-char (combo (datum x) (datum x))))
    (x
      (string? (datum x))
      (typed any-string (combo (datum x) (datum x))))
    (x
      (number? (datum x))
      (typed any-number (combo (datum x) (datum x))))
    (other
      (evaluate-typed test-evaluate-typed $environment $scope #'other))))

(define (datumize $typed)
  (typed
    (typed-type $typed)
    (switch (typed-value $typed)
      ((thunk? $thunk)
        $thunk)
      ((combo? $combo)
        (combo
          (switch (combo-value $combo)
            ((procedure? $procedure)
              a-procedure)
            ((else $other)
              $other))
          (combo-datum $combo))))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 'any-string)
    (typed any-type (combo any-string 'any-string))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 'any-number)
    (typed any-type (combo any-number 'any-number))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) '(any-lambda (any-number any-string) any-boolean))
    (typed any-type
      (combo
        (any-lambda (any-number any-string) any-boolean)
        '(any-lambda (any-number any-string) any-boolean)))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) "foo")
    (typed any-string (combo "foo" "foo"))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 123)
    (typed any-number (combo 123 123))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) '(expect any-string "foo"))
    (typed any-string (combo "foo" "foo"))))

(check
  (raises
    (test-evaluate-typed $environment (scope) '(expect any-number "foo"))))

(check
  (equal?
    (test-evaluate-typed $environment (scope (x (typed any-string (combo "foo" "foo")))) 'x)
    (typed any-string (combo "foo" "foo"))))

(check
  (equal?
    (datumize (test-evaluate-typed $environment (scope (x (typed any-string hole))) 'x))
    (typed
      any-string
      (thunk 0 (compiled (scope) 'x)))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) '(expect any-string "foo"))
    (typed any-string (combo "foo" "foo"))))

(check
  (equal?
    (datumize
      (test-evaluate-typed $environment (scope)
        '(lambda () "foo")))
    (typed
      (any-lambda () any-string)
      (combo
        a-procedure
        '(lets (lambda () "foo"))))))

(check
  (equal?
    (datumize
      (test-evaluate-typed $environment
        (scope (x (typed any-string (combo "foo" "foo"))))
        '(lambda () x)))
    (typed
      (any-lambda () any-string)
      (combo
        a-procedure
        '(lets (lambda () "foo"))))))

(check
  (equal?
    (datumize
      (test-evaluate-typed $environment
        (scope (x (typed any-string hole)))
        '(lambda () x)))
    (typed
      (any-lambda () any-string)
      (thunk 0
        (compiled (scope)
          '(lambda () x))))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope
        (foo (typed any-string (combo "foo" "foo"))))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string
      (combo
        "foobar"
        '(lets (string-append "foo" "bar"))))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope (foo (typed any-string hole)))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string
      (thunk 0
        (compiled
          (scope)
          '(string-append foo "bar"))))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope)
      '(let ((foo "foo")) foo))
    (typed any-string (combo "foo" "foo"))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope
        (string-append
          (typed
            (any-lambda (any-string any-string) any-string)
            (combo string-append 'string-append))))
      '(let ((foo "foo")) (string-append foo "!")))
    (typed
      any-string
      (combo
        "foo!"
        '(lets (string-append "foo" "!"))))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope
        (foo (typed any-string hole))
        (bar (typed any-string hole))
        (string-append
          (typed
            (any-lambda (any-string any-string) any-string)
            (combo string-append 'string-append))))
      '(let
        ((foo! (string-append foo "!")))
        (string-append foo! bar)))
    (typed any-string
      (thunk 1
        (compiled
          (scope)
          '(string-append foo! bar))))))

(check
  (equal?
    (test-evaluate-typed
      $environment
      (scope
        (evaluate-fixnum
          (typed
            any-evaluate-typed-lambda
            (lambda ($recurse $environment $scope $syntax $discard)
              (syntax-case $syntax (fixnum)
                ((fixnum x)
                  (if (fixnum? (datum x))
                    (typed any-fixnum (datum x))
                    (syntax-error #'x "invalid fixnum")))
                (other
                  ($discard)))))))
      '(fixnum 123))
    (typed any-fixnum 123)))

