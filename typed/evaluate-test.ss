(import (micascheme) (typed lang) (typed typed) (typed evaluate) (any))

(define (datumize $typed)
  (typed
    (typed-type $typed)
    (switch (typed-value $typed)
      ((thunk? $thunk)
        (thunk
          (thunk-max-index $thunk)
          ((thunk-datum-proc $thunk))))
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
