(import (micascheme) (tico reference) (tico index) (tico arity))

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

; === reference-promote

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

; === reference-append

(check
  (equal?
    (reference-append
      (reference (index 1) (list))
      (reference (index 5) (list (cons 'foo "foo")))
      (reference (index 3) (list (cons 'bar "bar") (cons 'goo "goo"))))
    (reference
      (index 5)
      (list (cons 'foo "foo") (cons 'bar "bar") (cons 'goo "goo")))))

(check
  (equal?
    (reference-append
      (reference (index 1) (list))
      (reference #f (list (cons 'foo "foo")))
      (reference (index 3) (list (cons 'bar "bar") (cons 'goo "goo"))))
    (reference #f
      (list (cons 'foo "foo") (cons 'bar "bar") (cons 'goo "goo")))))
