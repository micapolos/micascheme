(import (micascheme) (tico-2))

(check
  (equal?
    (syntax->typed #`(#f))
    (typed
      (term #f (constant #f))
      (any-boolean))))

(check
  (equal?
    (syntax->typed #`(128))
    (typed
      (term 128 (constant 128))
      (any-number))))

(check
  (equal?
    (syntax->typed #`("foo"))
    (typed
      (term "foo" (constant "foo"))
      (any-string))))
