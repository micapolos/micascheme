(import
  (micascheme)
  (sampled))

(check
  (equal?
    (sampled-list 5 (sampled 0))
    (list 0 0 0 0 0)))

(check
  (equal?
    (sampled-list 5 (sampled-iterator 10 add1))
    (list 10 11 12 13 14)))

(check
  (equal?
    (sampled-list 5
      (sampled-bind
        (sampled-iterator 0 add1)
        (lambda ($dt)
          (sampled-iterator 0
            (lambda ($previous)
              (+ $previous $dt))))))
    (list 10 11 12 13 14)))
