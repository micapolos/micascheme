(import (micascheme) (tico compiler))

(check
  (equal?
    (term->typed-thunk (native `string "foo"))
    (typed `string (thunk "foo" 0))))

(check
  (equal?
    (bindings-term->typed-thunk
      (stack
        (binding `t1 `v1)
        (binding `t2 `v2))
      (variable `t1))
    (typed `t1 (thunk `v1 2))))

(check
  (equal?
    (bindings-term->typed-thunk
      (stack
        (binding `t1 `v1)
        (binding `t2 `v2))
      (variable `t2))
    (typed `t2 (thunk `v2 1))))

(with-generate-temporary-seed tmp
  (check
    (equal?
      (term->typed-thunk (abstraction (list `t1 `t2) (variable `t1)))
      (typed
        (function-type (list `t1 `t2) `t1)
        (thunk
          `(lambda (tmp-0 tmp-1) tmp-0)
          0)))))

(check
  (equal?
    (term->typed-thunk
      (application
        (native (function-type (list `t1 `t2) `t3) `fn)
        (list
          (native `t1 `a1)
          (native `t2 `a2))))
    (typed
      `t3
      (thunk `(fn a1 a2) 0))))

(check
  (equal?
    (term->typed-thunk
      (struct `foo
        (list
          (native `t1 `v1)
          (native `t2 `v2))))
    (typed
      (struct `foo (list `t1 `t2))
      (thunk `(list v1 v2) 0))))
