(import (micascheme) (tico indexed))

(check
  (equal?
    (term-evaluate (evaluated "foo"))
    (evaluated "foo")))

(check
  (equal?
    (term-evaluate (variable 0))
    (variable 0)))

(check
  (equal?
    (term-evaluate
      (application
        (evaluated string-append)
        (list
          (evaluated "foo")
          (evaluated "bar"))))
    (evaluated "foobar")))

(check
  (equal?
    (term-evaluate (abstraction 2 (evaluated "foo")))
    (abstraction 2 (evaluated "foo"))))

(check
  (equal?
    (term-evaluate
      (abstraction 2
        (application
          (evaluated string-append)
          (list (evaluated "foo") (evaluated "bar")))))
    (abstraction 2 (evaluated "foobar"))))

(check
  (equal?
    (term-evaluate
      (application
        (abstraction 2
          (application
            (evaluated string-append)
            (list (variable 1) (variable 0))))
        (list (evaluated "foo") (evaluated "bar"))))
    (evaluated "foobar")))

(check
  (equal?
    (term-evaluate
      (expansion
        (application
          (evaluated evaluated)
          (list (evaluated "foo")))))
    (evaluated "foo")))
