(import
  (micascheme)
  (typed lang)
  (typed typed)
  (typed evaluate)
  (typed scope)
  (typed thunk)
  (typed compiled)
  (typed hole)
  (any))

(data a-procedure)
(define-aux-keyword fixnum)

(define $environment (environment '(scheme)))

(define (test-evaluate-typed $environment $scope $syntax)
  (syntax-case $syntax (any-type any-boolean any-char any-string any-number)
    (any-type
      (typed any-type any-type))
    (any-boolean
      (typed any-type any-boolean))
    (any-char
      (typed any-type any-char))
    (any-string
      (typed any-type any-string))
    (any-number
      (typed any-type any-number))
    (x
      (boolean? (datum x))
      (typed any-boolean (datum x)))
    (x
      (char? (datum x))
      (typed any-char (datum x)))
    (x
      (string? (datum x))
      (typed any-string (datum x)))
    (x
      (number? (datum x))
      (typed any-number (datum x)))
    (other
      (evaluate-typed test-evaluate-typed $environment $scope #'other))))

(define (datumize $typed)
  (typed
    (typed-type $typed)
    (switch (typed-value $typed)
      ((procedure? $procedure)
        a-procedure)
      ((else $other)
        $other))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 'any-string)
    (typed any-type any-string)))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 'any-number)
    (typed any-type any-number)))

(check
  (equal?
    (test-evaluate-typed $environment (scope) "foo")
    (typed any-string "foo")))

(check
  (equal?
    (test-evaluate-typed $environment (scope) 123)
    (typed any-number 123)))

(check
  (equal?
    (test-evaluate-typed $environment (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (raises
    (test-evaluate-typed $environment (scope) '(expect any-number "foo"))))

(check
  (equal?
    (test-evaluate-typed $environment (scope (x (typed any-string "foo"))) 'x)
    (typed any-string "foo")))

(check
  (equal?
    (datumize (test-evaluate-typed $environment (scope (x (typed any-string hole))) 'x))
    (typed any-string (thunk 0 (compiled (scope) 'x)))))

(check
  (equal?
    (test-evaluate-typed $environment (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (equal?
    (datumize
      (test-evaluate-typed $environment (scope)
        '(lambda () "foo")))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (test-evaluate-typed $environment
        (scope (x (typed any-string "foo")))
        '(lambda () x)))
    (typed (any-lambda () any-string) a-procedure)))

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
      (scope (foo (typed any-string "foo")))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string "foobar")))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope (foo (typed any-string hole)))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string
      (thunk 0
        (compiled
          (scope (tmp_0 string-append))
          '(tmp_0 foo "bar"))))))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope)
      '(let ((foo "foo")) foo))
    (typed any-string "foo")))

(check
  (equal?
    (test-evaluate-typed $environment
      (scope
        (foo (typed any-string hole))
        (bar (typed any-string hole))
        (string-append (typed (any-lambda (any-string any-string) any-string) string-append)))
      '(let ((foo! (string-append foo "!"))) (string-append foo! bar)))
    (typed any-string
      (thunk 1
        (compiled
          (scope (tmp_1 string-append))
          '(tmp_1 foo! bar))))))

(check
  (equal?
    (test-evaluate-typed
      $environment
      (stack
        (cons evaluate-typed-gensym
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

