(import (micascheme) (tico resolution))

(lets
  ($resolution
    (resolution 2
      (list
        (cons 'foo "foo")
        (cons 'bar "bar"))))
  (run
    (check (resolution? $resolution))
    (check (not (resolution? 'foo)))
    (check (equal? (resolution-depth-opt $resolution) 2))
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
      128)
    (resolution #f
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution 5
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      2)
    (resolution 3
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution 5
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      5)
    (resolution 0
      (list
        (cons 'foo "foo")
        (cons 'bar "bar")))))

(check
  (equal?
    (resolution-promote
      (resolution 5
        (list
          (cons 'foo "foo")
          (cons 'bar "bar")))
      6)
    #f))
