(import
  (leo2 base)
  (leo2 term)
  (leo2 equal)
  (leo2 stdlib))

(check-term=?
  (string-term "foo")
  (evaluated (string-term "foo")))
