(import
  (leo2 lang)
  (leo2 normalize)
  (prefix (leo2 base) %)
  (prefix (curry) %))

(print
  (lambda
    (x (native 10))
    (y (native 20))
    (apply (native %curry+) x y)))
