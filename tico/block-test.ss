(import
  (micascheme)
  (tico block)
  (tico typing)
  (tico type)
  (tico entry))

(check
  (equal?
    (block-let
      (empty-stack-typing)
      (block
        (list
          (entry
            (list
              (parameter-typing (string-type) '$string))
            (list
              (literal->typing "foo")))
          (entry
            (list
              (parameter-typing (number-type) '$number)
              (parameter-typing (string-type) '$string2))
            (list
              (literal->typing 128)
              (variable-typing (string-type) '$string 0))))
        (list
          (variable-typing (number-type) '$number 1)
          (variable-typing (string-type) '$string 2)
          (variable-typing (string-type) '$string2 0)))
      (lambda ($scope $typings)
        (typing-struct 'foo $typings)))
    (entries-let
      (empty-stack-typing)
      (list
        (entry
          (list
            (parameter-typing (string-type) '$string))
          (list
            (literal->typing "foo")))
        (entry
          (list
            (parameter-typing (number-type) '$number)
            (parameter-typing (string-type) '$string2))
          (list
            (literal->typing 128)
            (variable-typing (string-type) '$string 0))))
      (lambda ($scope)
        (typing-struct 'foo
          (list
            (variable-typing (number-type) '$number 1)
            (variable-typing (string-type) '$string 2)
            (variable-typing (string-type) '$string2 0)))))))

(check
  (equal?
    (block-struct 'foo
      (block
        (list
          (test-entry e1 e2)
          (test-entry e3 e4))
        (list
          (test-typing t1)
          (test-typing t2))))
    (block-let
      (empty-stack-typing)
      (block
        (list
          (test-entry e1 e2)
          (test-entry e3 e4))
        (list
          (test-typing t1)
          (test-typing t2)))
      (lambda ($scope $typings)
        (typing-struct 'foo (reverse $typings))))))
