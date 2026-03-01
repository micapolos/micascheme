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
  (solution 0)
  (deduction
    (solution 0 (native "apple"))
    (native "apple")))

(check-term-deduction-from-to
  (solution 0)
  (native "banana")
  (deduction
    (solution 0 (native "banana"))
    (native "banana")))

(check-term-deduction-from-to
  (application (native "fn") (solution 0))
  (application (solution 1) (native "arg"))
  (deduction
    (solution 0 (native "arg"))
    (solution 1 (native "fn"))
    (application (native "fn") (native "arg"))))
