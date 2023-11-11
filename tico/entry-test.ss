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
