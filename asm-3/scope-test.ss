(import
  (micascheme)
  (asm-3 scope))

(define test-scope
  (scope-with
    (foo "foo")
    (bar "bar")))

(check (equal? (scope-ref test-scope #'foo) "foo"))
(check (equal? (scope-ref test-scope #'bar) "bar"))
(check (raises (scope-ref test-scope #'goo)))
