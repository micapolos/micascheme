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

(check-term-deduction-from-to
  (native "apple")
  (hole 0)
  (deduction
    (hole 0 (native "apple"))
    (native "apple")))

(check-term-deduction-from-to
  (hole 0)
  (native "banana")
  (deduction
    (hole 0 (native "banana"))
    (native "banana")))

(check-term-deduction-from-to
  (application (native "fn") (hole 0))
  (application (hole 1) (native "arg"))
  (deduction
    (hole 0 (native "arg"))
    (hole 1 (native "fn"))
    (application (native "fn") (native "arg"))))
