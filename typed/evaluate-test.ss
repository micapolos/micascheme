(import (micascheme) (typed lang) (typed typed) (typed evaluate) (any))

(data a-procedure)

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
    (evaluate-syntax (scope) "foo")
    (typed any-string "foo")))

(check
  (equal?
    (evaluate-syntax (scope (x (typed any-string "foo"))) 'x)
    (typed any-string "foo")))

(check
  (equal?
    (datumize (evaluate-syntax (scope (x (typed any-string hole))) 'x))
    (typed any-string (thunk 0 'x))))

(check
  (equal?
    (datumize
      (evaluate-syntax (scope)
        '(lambda () "foo")))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-syntax
        (scope (x (typed any-string "foo")))
        '(lambda () x)))
    (typed (any-lambda () any-string) a-procedure)))

(check
  (equal?
    (datumize
      (evaluate-syntax
        (scope (x (typed any-string hole)))
        '(lambda () x)))
    (typed
      (any-lambda () any-string)
      (thunk 0 '(lambda () x)))))

