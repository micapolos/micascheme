(import
  (micascheme)
  (tico dependency)
  (tico packet))

(check
  (equal?
    (dependencies+
      (stack
        (test-dependency d1)
        (test-dependency d2))
      (stack
        (test-dependency d3)
        (test-dependency d4)))
    (stack
      (test-dependency d1)
      (test-dependency d2)
      (test-dependency d3)
      (test-dependency d4))))

(check
  (equal?
    (dependency-lets-datum
      (dependency 'foo (test-packet bar)))
    '(foo 'bar)))

(check
  (equal?
    (dependencies-lets
      (test-dependencies)
      `(string-append v1 v2))
    '(string-append v1 v2)))

(check
  (equal?
    (dependencies-lets
      (test-dependencies d1 d2)
      `(string-append v1 v2))
    '(lets
      (d1 'd1)
      (d2 'd2)
      (string-append v1 v2))))

(check
  (equal?
    (test-dependency foo)
    (dependency 'foo (test-packet foo))))

(check
  (equal?
    (test-dependencies d1 d2)
    (stack
      (test-dependency d1)
      (test-dependency d2))))
