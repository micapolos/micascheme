(import
  (leo2 base)
  (leo2 term)
  (leo2 deduce))

(check-term-deduction
  (deduction-with (native "foo"))
  (deduction (native "foo")))

(check-term-deduction-from-to
  (native "foo")
  (native "foo")
  (deduction (native "foo")))

(check-term-deduction-from-to
  (native "foo")
  (native "bar")
  (deduction #f))
