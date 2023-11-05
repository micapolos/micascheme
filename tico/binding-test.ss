(import
  (micascheme)
  (tico binding)
  (tico typing)
  (tico type))

(check
  (equal?
    (binding-switch (binding (literal->typing "foo"))
      ((typing? $typing) (cons 'typing $typing))
      ((else $other) (cons 'other $other)))
    (cons 'typing (literal->typing "foo"))))

(check
  (equal?
    (binding-switch (binding (literal->typing "foo"))
      ((typing? $typing) (cons 'typing $typing))
      ((else $other) (cons 'other $other)))
    (cons 'typing (literal->typing "foo"))))
