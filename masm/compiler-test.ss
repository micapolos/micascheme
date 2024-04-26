(import
  (except (micascheme) module)
  (masm compiler)
  (masm model))

(check (equal? (type-size (type (int (i8)))) 1))
(check (equal? (type-size (type (int (i16)))) 2))
(check (equal? (type-size (type (arrow (list) (list)))) 2))

(lets
  ($types
    (stack
      (type (int (i8)))
      (type (int (i16)))
      (type (arrow (list) (list)))))
  (run
    (check (equal? (types-offset $types 0) 0))
    (check (equal? (types-offset $types 1) 2))
    (check (equal? (types-offset $types 2) 4))
    (check (equal? (types-offset $types 3) 5))))

(check
  (equal?
    (types-copy-instr
      (stack
        (type (int (i8)))
        (type (int (i16)))
        (type (int (i8)))
        (type (arrow (list) (list))))
      1 3)
    (copy 2 5 1)))
