(import
  (leo2 base)
  (leo2 term)
  (leo2 equal))

(check-term=?
  (native "foo")
  (evaluated (native "foo")))
