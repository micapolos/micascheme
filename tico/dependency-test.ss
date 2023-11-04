(import
  (micascheme)
  (tico dependency)
  (tico packet))

(check
  (equal?
    (tuple-dependencies
      (list
        (stack
          (test-dependency d1)
          (test-dependency d2))
        (stack
          (test-dependency d3)
          (test-dependency d4))))
    (stack
      (test-dependency d1)
      (test-dependency d2)
      (test-dependency d3)
      (test-dependency d4))))

(check
  (equal?
    (test-dependency foo)
    (dependency 'foo (test-packet foo))))
