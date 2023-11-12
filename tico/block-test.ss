(import
  (micascheme)
  (tico block)
  (tico typing)
  (tico entry))

(check
  (equal?
    (block-let
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
      (lambda ($typings)
        (typing-struct 'foo $typings)))
    (entries-let
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
      (typing-struct 'foo
        (list
          (variable-typing (number-type) '$number 1)
          (variable-typing (string-type) '$string 2)
          (variable-typing (string-type) '$string2 0))))))
