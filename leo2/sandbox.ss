(import
  (leo2 lang)
  (leo2 lang-church)
  (leo2 untyped)
  (leo2 print))

(print
  (untyped
    (apply
      (apply
        (apply cons (native "foo")) (native "bar"))
      car)))
