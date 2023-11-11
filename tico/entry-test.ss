(import
  (micascheme)
  (tico entry))

(check
  (equal?
    (with-tmps
      (typing->entry
        (test-typing t1)))
    (with-tmps
      (entry
        (typing-parameter (test-typing t1))
        (test-typing t1)))))
