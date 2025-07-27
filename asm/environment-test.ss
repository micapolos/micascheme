(import
  (asm base)
  (asm environment))

(define test-environment
  (environment-with
    (foo "foo")
    (bar "bar")))

(check (equal? (environment-ref test-environment #'foo) "foo"))
(check (equal? (environment-ref test-environment #'bar) "bar"))
(check (raises (environment-ref test-environment #'goo)))
