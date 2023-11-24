(import
  (micascheme)
  (tico packet)
  (tico definition))

(check
  (equal?
    (packet-map
      symbol->string
      (packet
        (list
          (definition 'k1 'v1)
          (definition 'k2 'v2))
        (list 'i1 'i2)))
    (packet
      (list
        (definition "k1" "v1")
        (definition "k2" "v2"))
      (list "i1" "i2"))))
