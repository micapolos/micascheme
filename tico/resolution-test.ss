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
