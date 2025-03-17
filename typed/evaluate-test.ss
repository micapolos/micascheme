(import (micascheme) (typed lang) (typed typed) (typed evaluate) (any))

(data a-procedure)

(define env (environment '(scheme)))

(define (datumize $typed)
  (typed
    (typed-type $typed)
    (switch (typed-value $typed)
      ((thunk? $thunk)
        (thunk
          (thunk-max-index $thunk)
          ((thunk-datum-proc $thunk))))
      ((procedure? $procedure)
        a-procedure)
      ((else $other)
        $other))))

(check
  (equal?
    (evaluate-syntax env (scope) 'any-string)
    (typed any-type any-string)))

(check
  (equal?
    (evaluate-syntax env (scope) 'any-number)
    (typed any-type any-number)))

(check
  (equal?
    (evaluate-syntax env (scope) "foo")
    (typed any-string "foo")))

(check
  (equal?
    (evaluate-syntax env (scope) 123)
    (typed any-number 123)))

(check
  (equal?
    (evaluate-syntax env (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (raises
    (evaluate-syntax env (scope) '(expect any-number "foo"))))

(check
  (equal?
    (evaluate-syntax env (scope (x (typed any-string "foo"))) 'x)
    (typed any-string "foo")))

(check
  (equal?
    (datumize (evaluate-syntax env (scope (x (typed any-string hole))) 'x))
    (typed any-string (thunk 0 'x))))

(check
  (equal?
    (evaluate-syntax env (scope) '(expect any-string "foo"))
    (typed any-string "foo")))

(check
  (equal?
    (datumize
      (evaluate-syntax env (scope)
        '(lambda () "foo")))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-syntax env
        (scope (x (typed any-string "foo")))
        '(lambda () x)))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-syntax env
        (scope (x (typed any-string hole)))
        '(lambda () x)))
    (typed
      (any-lambda () any-string)
      (thunk 0 '(lambda () x)))))
