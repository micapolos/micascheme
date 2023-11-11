(import
  (micascheme)
  (tico entry))

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
      (entry
        (list
          (parameter-typing (string-type) 's)
          (parameter-typing (number-type) 'n))
        (list
          (literal->typing "foo")
          (literal->typing 128)))
      (variable-typing (string-type) 's 1))
    (typing-application
      (typing-abstraction
        (list
          (parameter-typing (string-type) 's)
          (parameter-typing (number-type) 'n))
        (variable-typing (string-type) 's 1))
      (list
        (literal->typing "foo")
        (literal->typing 128)))))

(check
  (equal?
    (entries-let
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
      (variable-typing (string-type) 's2 1))
    (typing-application
      (typing-abstraction
        (list
          (parameter-typing (string-type) 's1)
          (parameter-typing (number-type) 'n1))
        (typing-application
          (typing-abstraction
            (list
              (parameter-typing (string-type) 's2)
              (parameter-typing (number-type) 'n2))
            (variable-typing (string-type) 's2 1))
          (list
            (variable-typing (string-type) 's1 1)
            (variable-typing (number-type) 'n1 0))))
      (list
        (literal->typing "foo")
        (literal->typing 128)))))

