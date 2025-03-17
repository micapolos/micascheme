(import (micascheme) (typed lang) (typed typed) (typed evaluate) (typed scope) (typed thunk) (typed compiled) (typed hole) (any))

(data a-procedure)

(define env (environment '(scheme)))

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
    (evaluate-typed env (scope) 'any-string)
    (typed any-type any-string)))

(check
  (equal?
    (evaluate-typed env (scope) 'any-number)
    (typed any-type any-number)))

(check
  (equal?
    (evaluate-typed env (scope) "foo")
    (typed any-string "foo")))

(check
  (equal?
    (evaluate-typed env (scope) 123)
    (typed any-number 123)))

(check
  (equal?
    (evaluate-typed env (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (raises
    (evaluate-typed env (scope) '(expect any-number "foo"))))

(check
  (equal?
    (evaluate-typed env (scope (x (typed any-string "foo"))) 'x)
    (typed any-string "foo")))

(check
  (equal?
    (datumize (evaluate-typed env (scope (x (typed any-string hole))) 'x))
    (typed any-string (thunk 0 (compiled (scope) 'x)))))

(check
  (equal?
    (evaluate-typed env (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (equal?
    (datumize
      (evaluate-typed env (scope)
        '(lambda () "foo")))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-typed env
        (scope (x (typed any-string "foo")))
        '(lambda () x)))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-typed env
        (scope (x (typed any-string hole)))
        '(lambda () x)))
    (typed
      (any-lambda () any-string)
      (thunk 0
        (compiled (scope)
          '(lambda () x))))))

(check
  (equal?
    (evaluate-typed env
      (scope (foo (typed any-string "foo")))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string "foobar")))

(check
  (equal?
    (evaluate-typed env
      (scope (foo (typed any-string hole)))
      '((assume (any-lambda (any-string any-string) any-string) string-append) foo "bar"))
    (typed any-string
      (thunk 0
        (compiled
          (scope (tmp_0 string-append))
          '(tmp_0 foo "bar"))))))

(check
  (equal?
    (evaluate-typed env
      (scope)
      '(let ((foo "foo")) foo))
    (typed any-string "foo")))

(check
  (equal?
    (evaluate-typed env
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
