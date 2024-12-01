(library (int)
  (export
    int-mask
    int
    int+
    int-
    int*
    int+1
    int-1)
  (import (scheme) (syntax))

  (define-rule-syntax (int-mask size)
    (syntax-inline (- (bitwise-arithmetic-shift-left 1 size) 1)))

  (define-rule-syntax (int size i)
    (bitwise-and i (int-mask size)))

  (define-rule-syntax (int+ size a b)
    (int size (+ a b)))

  (define-rule-syntax (int- size a b)
    (int size (- a b)))

  (define-rule-syntax (int* size a b)
    (int size (* a b)))

  (define-rule-syntax (int+1 size a)
    (int+ size a 1))

  (define-rule-syntax (int-1 size a)
    (int- size a 1))
)
