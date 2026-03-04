(import
  (leo2 base)
  (leo2 term)
  (leo2 datum)
  (leo2 unpeel))

(check-term-datum=?
  (unpeel? native? (native "foo"))
  (native "foo"))

(check-term-datum=?
  (unpeel? native?
    (evaluated (native "foo")))
  (native "foo"))

(check-term-datum=?
  (unpeel? native?
    (labeled (native "label") (native "foo")))
  (native "foo"))

(check-term-datum=?
  (unpeel? native?
    (evaluated
      (labeled (native "label")
        (evaluated (native "foo")))))
  (native "foo"))

(check (not (unpeel? native? native-type)))
(check (not (unpeel? native? (evaluated native-type))))
