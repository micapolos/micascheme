(import
  (micascheme)
  (tico variable)
  (tico dependency)
  (tico packet))

(check
  (equal?
    (variable-lets-datums
      (variable 3
        (stack
          (dependency 'a1 (packet 'b1 "c1"))
          (dependency 'a2 (packet 'b2 "c2")))))
    (list
      '(a1 b1)
      '(a2 b2))))

(check
  (equal?
    (variable-promote
      (variable 3
        (stack
          (test-dependency foo)
          (test-dependency bar)))
      2)
    (variable 1
      (stack
        (test-dependency foo)
        (test-dependency bar)))))

(check
  (equal?
    (variable-promote
      (variable 3
        (stack
          (test-dependency foo)
          (test-dependency bar)))
      3)
    (variable 0
      (stack
        (test-dependency foo)
        (test-dependency bar)))))

(check
  (equal?
    (variable-promote
      (variable 3
        (stack
          (test-dependency foo)
          (test-dependency bar)))
      4)
    #f))

(check
  (equal?
    (variable-index-flatten
      (list 1 3 2))
    3))

(check
  (equal?
    (variable-flatten
      (list
        (variable 1
          (stack
            (test-dependency d1)
            (test-dependency d2)))
        (variable 3
          (stack
            (test-dependency d3)
            (test-dependency d4)))))
    (variable
      (variable-index-flatten (list 1 3))
      (dependencies-flatten
        (list
          (stack
            (test-dependency d1)
            (test-dependency d2))
          (stack
            (test-dependency d3)
            (test-dependency d4)))))))
