(import
  (leo2 lang)
  (leo2 lang-church)
  (leo2 print))

(print
  (apply
    (apply
      (apply cons (native "foo")) (native "bar"))
    car))
