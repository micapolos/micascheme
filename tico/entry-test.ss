(import
  (micascheme)
  (tico entry)
  (tico typing)
  (tico layment)
  (tico variable)
  (tico typing)
  (tico compilation)
  (tico layout)
  (tico type))

(check
  (equal?
    (test-entry e1 e2)
    (entry
      (list
        (test-parameter-typing e1)
        (test-parameter-typing e2))
      (list
        (test-typing e1)
        (test-typing e2)))))

(check
  (equal?
    (with-tmps
      (typings->entry
        (list
          (test-typing t1)
          (test-typing t2))))
    (with-tmps
      (entry
        (list
          (typing-parameter (test-typing t1))
          (typing-parameter (test-typing t2)))
        (list
          (test-typing t1)
          (test-typing t2))))))

(check
  (equal?
    (entry-let
      (empty-stack-typing)
      (entry
        (list
          (parameter-typing (string-type) 's)
          (parameter-typing (number-type) 'n))
        (list
          (literal->typing "foo")
          (literal->typing 128)))
      (lambda ($scope)
        (variable-typing (string-type) 's 1)))
    (typing (string-type)
      (layment (type->layout (string-type))
        (compilation
          '(let ((s "foo") (n 128)) s)
          (variable 1))))))

(check
  (equal?
    (entries-let
      (empty-stack-typing)
      (list
        (entry
          (list
            (parameter-typing (string-type) 's1)
            (parameter-typing (number-type) 'n1))
          (list
            (literal->typing "foo")
            (literal->typing 128)))
        (entry
          (list
            (parameter-typing (string-type) 's2)
            (parameter-typing (number-type) 'n2))
          (list
            (variable-typing (string-type) 's1 1)
            (variable-typing (number-type) 'n1 0))))
      (lambda ($scope)
        (variable-typing (string-type) 's2 1)))
    (typing (string-type)
      (layment (type->layout (string-type))
        (compilation
          '(let ([s1 "foo"] [n1 128]) (let ([s2 s1] [n2 n1]) s2))
          (variable 1))))))
