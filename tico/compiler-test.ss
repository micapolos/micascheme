(import (micascheme) (tico compiler))

(check
  (equal?
    (term->compiled (native `string "foo"))
    (compiled (typed `string "foo") 0)))

(check
  (equal?
    (bindings-term->compiled
      (stack
        (binding `t1 `v1)
        (binding `t2 `v2))
      (variable `t1))
    (compiled (typed `t1 `v1) 2)))

(check
  (equal?
    (bindings-term->compiled
      (stack
        (binding `t1 `v1)
        (binding `t2 `v2))
      (variable `t2))
    (compiled (typed `t2 `v2) 1)))

(with-generate-temporary-seed tmp
  (check
    (equal?
      (term->compiled (abstraction (list `t1 `t2) (variable `t1)))
      (compiled
        (typed
          (function-type (list `t1 `t2) `t1)
          `(lambda (tmp-0 tmp-1) tmp-0))
        0))))

(check
  (equal?
    (term->compiled
      (application
        (native (function-type (list `t1 `t2) `t3) `fn)
        (list
          (native `t1 `a1)
          (native `t2 `a2))))
    (compiled
      (typed `t3 `(fn a1 a2))
      0)))

(check
  (equal?
    (term->compiled
      (struct `foo
        (list
          (native `t1 `v1)
          (native `t2 `v2))))
    (compiled
      (typed
        (struct `foo (list `t1 `t2))
        `(list v1 v2))
      0)))
