(import
  (leo2 base)
  (leo2 term)
  (leo2 deduce))

(check-term-deduction
  (deduction ($deduced)
    (values $deduced (native "foo")))
  (deduction (native "foo")))
