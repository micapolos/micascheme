(import
  (leo2 base)
  (leo2 term)
  (leo2 typed)
  (leo2 datum))

; === term-type

(check-term-datum=?
  (term-type (type 2))
  (type 3))

(check-term-datum=?
  (term-type (typed (native "type") (native "value")))
  (native "type"))

(check (raises (term-type (native "type"))))

; === term-value

(check-term-datum=?
  (term-value (type 2))
  (type 2))

(check-term-datum=?
  (term-value (typed (native "type") (native "value")))
  (native "value"))

(check (raises (term-value (native "value"))))

; === typed-term

(check-term-datum=?
  (typed-term (type 2) (type 1))
  (type 1))

(check-term-datum=?
  (typed-term (native "type") (native "value"))
  (typed (native "type") (native "value")))
