(import (micascheme) (tico reference))

(lets
  ($reference
    (reference
      (index 2)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar"))))
  (run
    (check (reference? $reference))
    (check (not (reference? 'foo)))
    (check
      (equal?
        (reference-index-opt $reference)
        (index 2)))
    (check
      (equal?
        (reference-bindings $reference)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar"))))))

(check
  (equal?
    (reference-promote
      (reference #f
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 128))
    (reference #f
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (reference-promote
      (reference
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 2))
    (reference
      (index 3)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (reference-promote
      (reference
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 5))
    (reference
      (index 0)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (reference-promote
      (reference
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 6))
    #f))
