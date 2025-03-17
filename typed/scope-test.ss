(import (micascheme) (typed scope))

(check (equal? (scope) (stack)))

(check
  (equal?
    (scope (foo "foo") (bar "bar"))
    (stack (cons 'foo "foo") (cons 'bar "bar"))))

(check
  (equal?
    (scope+ (scope (foo "foo") (bar "bar")) 'goo "goo")
    (scope (foo "foo") (bar "bar") (goo "goo"))))

(check
  (equal?
    (scope-ref (scope (foo "foo") (bar "bar")) 'foo)
    (cons 1 "foo")))

(check
  (equal?
    (scope-ref (scope (foo "foo") (bar "bar")) 'bar)
    (cons 0 "bar")))

(check
  (equal?
    (scope-ref (scope (foo "foo") (bar "bar")) 'goo)
    #f))
