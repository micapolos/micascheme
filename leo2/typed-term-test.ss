(import
  (leo2 base)
  (leo2 term)
  (leo2 typed-term)
  (leo2 datum))

; === term-type

(check-term-datum=?
  (term-type (type 2))
  (type 3))

(check-term-datum=?
  (term-type (typed (native "type") (native "value")))
  (native "type"))

(check-term-datum=?
  (term-type (native "type"))
  unknown)

; === term-value

(check-term-datum=?
  (term-value (type 2))
  (type 2))

(check-term-datum=?
  (term-value (typed (native "type") (native "value")))
  (native "value"))

(check-term-datum=?
  (term-value (native "value"))
  nothing)

; === type-value->term

(check-term-datum=?
  (type-value->term (type 2) (type 1))
  (type 1))

(check-term-datum=?
  (type-value->term (native "type") (native "value"))
  (typed (native "type") (native "value")))
