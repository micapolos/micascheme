(import
  (tt lang)
  (tt option)
  (tt boolean)
  (tt number)
  (tt datum)
  (tt string))

(check (true? (option=? = (some 10) (some 10))))
(check (false? (option=? = (some 10) (some 20))))

(check (datum=? (option->datum number->datum (some 10)) '10))
(check (datum=? (option->datum number->datum none) '#f))

(check (true? (none? none)))
(check (false? (none? (some 10))))

(check
  (true?
    (option=? string=?
      (option-map number->string (some 10))
      (some "10"))))

(check
  (true?
    (option=? string=?
      (option-map number->string none)
      none)))

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
