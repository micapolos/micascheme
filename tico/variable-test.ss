(import
  (micascheme)
  (tico variable)
  (tico dependency)
  (tico packet))

(check
  (equal?
    (test-variable 3 d1 d2)
    (variable 3
      (stack
        (test-dependency d1)
        (test-dependency d2)))))

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
    (variable-promote (test-variable 3 d1 d2) 2)
    (test-variable 1 d1 d2)))

(check
  (equal?
    (variable-promote (test-variable 3 d1 d2) 3)
    (test-variable 0 d1 d2)))

(check
  (equal?
    (variable-promote (test-variable 3 d1 d2) 4)
    #f))

(check (equal? (variable-index+ 2 4) 4))
(check (equal? (variable-index+ 4 2) 4))
(check (equal? (variable-index+ 2 2) 2))

(check
  (equal?
    (variable+
      (test-variable 2 d1 d2)
      (test-variable 3 d3 d4))
    (test-variable 3 d1 d2 d3 d4)))

(check
  (equal?
    (variable-flatten
      (list
        (test-variable 1 d1 d2)
        (test-variable 3 d3 d4)
        (test-variable 2 d5 d6)))
    (test-variable 3 d1 d2 d3 d4 d5 d6)))

(check
  (equal?
    (variable+dependencies
      (test-variable 1 d1 d2)
      (test-dependencies d3 d4))
    (test-variable 1 d1 d2 d3 d4)))
