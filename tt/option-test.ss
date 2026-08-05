(import
  (tt lang)
  (tt option)
  (tt number))

(check
  (=
    (option-match
      (lambda () -1)
      (lambda ((n number)) (+ n 1))
      (some 10))
    11))

(check
  (=
    (option-match
      (lambda () -1)
      (lambda ((n number)) (+ n 1))
      none)
    -1))
