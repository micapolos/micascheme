(import
  (micascheme)
  (tico variable))

(check
  (equal?
    (variable-promote (variable 3) 2)
    (variable 1)))

(check
  (equal?
    (variable-promote (variable 3) 3)
    (variable 0)))

(check
  (equal?
    (variable-promote (variable 3) 4)
    #f))

(check (equal? (variable-index+ 2 4) 4))
(check (equal? (variable-index+ 4 2) 4))
(check (equal? (variable-index+ 2 2) 2))

(check
  (equal?
    (variable+
      (variable 2)
      (variable 3))
    (variable 3)))

(check
  (equal?
    (variable-flatten
      (list
        (variable 1)
        (variable 3)
        (variable 2)))
    (variable 3)))
