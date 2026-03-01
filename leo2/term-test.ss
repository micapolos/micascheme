(import
  (leo2 base)
  (leo2 term))

(check
  (equal?
    (type+1 (type 12))
    (type 13)))
