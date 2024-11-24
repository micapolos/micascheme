(import
  (micalog syntax)
  (prefix (micascheme) %))

(%check-datum=?
  (micalog-syntax
    (module foo
      (input in)
      (output out (not in))))
  (%syntax
    (module foo
      (%input 1 in)
      (%output 1 out (not 1 in)))))
