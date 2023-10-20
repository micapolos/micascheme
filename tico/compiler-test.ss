(import (micascheme) (tico compiler))

(check
  (equal?
    (term->compiled (native `string "foo"))
    (compiled `string (thunk "foo" 0))))

(check
  (equal?
    (bindings-term->compiled
      (stack
        (binding `foo `foo-type)
        (binding `bar `bar-type))
      (variable `foo-type))
    (compiled `foo-type (thunk `foo 2))))

(check
  (equal?
    (bindings-term->compiled
      (stack
        (binding `foo `foo-type)
        (binding `bar `bar-type))
      (variable `bar-type))
    (compiled `bar-type (thunk `bar 1))))

(with-generate-temporary-seed tmp
  (check
    (equal?
      (term->compiled (abstraction (list `t1 `t2) (variable `t1)))
      (compiled
        (function-type (list `t1 `t2) `t1)
        (thunk
          `(lambda (tmp-0 tmp-1) tmp-0)
          0)))))

(check
  (equal?
    (term->compiled
      (application
        (native (function-type (list `t1 `t2) `t3) `fn)
        (list
          (native `t1 `a1)
          (native `t2 `a2))))
    (compiled
      `t3
      (thunk `(fn a1 a2) 0))))

(check
  (equal?
    (term->compiled
      (struct `foo
        (list
          (native `t1 `v1)
          (native `t2 `v2))))
    (compiled
      (struct `foo (list `t1 `t2))
      (thunk `(list v1 v2) 0))))
