(import
  (micascheme)
  (tico definition))

(check
  (equal?
    (definition-map
      number->string
      (definition 10 20))
    (definition "10" "20")))
