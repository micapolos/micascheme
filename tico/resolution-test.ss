(import (micascheme) (tico resolution))

(lets
  ($resolution
    (resolution
      (index 2)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar"))))
  (run
    (check (resolution? $resolution))
    (check (not (resolution? 'foo)))
    (check
      (equal?
        (resolution-index-opt $resolution)
        (index 2)))
    (check
      (equal?
        (resolution-bindings $resolution)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar"))))))

(check
  (equal?
    (resolution-promote
      (resolution #f
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 128))
    (resolution #f
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 2))
    (resolution
      (index 3)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 5))
    (resolution
      (index 0)
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution
        (index 5)
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      (arity 6))
    #f))
