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
    (variables-index
      (list
        (variable 1 (stack))
        (variable 3 (stack))
        (variable 2 (stack))))
    3))

; (check
;   (equal?
;     (tuple-variable
;       (list
;         (variable 3
;           (stack
;             (dependency 'v1 (packet '(string-append "foo" "bar") "foobar"))))
;         (variable 5
;           (stack
;             (dependency 'v2 (packet '(+ 1 2) 3))
;             (dependency 'v3 (packet '(+ 4 5) 9))))))
;     (variable 5
;       (stack
;         (dependency 'v1 (packet '(string-append "foo" "bar") "foobar"))
;         (dependency 'v2 (packet '(+ 1 2) 3))
;         (dependency 'v3 (packet '(+ 4 5) 9))))))
