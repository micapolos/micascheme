(import
  (scheme)
  (list)
  (lets)
  (procedure)
  (boolean)
  (tt elab)
  (tt lookup))

(check
  (elaborate (elab 'foo))
  'foo)

(check
  (elaborate
    (x 'x)
    (elab 'foo))
  (x 'x)
  'foo)

(check
  (elaborate
    (x 'x)
    (elab (x 'x2) 'foo))
  (x 'x2)
  'foo)

(check
  (elaborate (elab-let (elab 'foo)))
  'foo)

(check
  (elaborate
    (elab-let
      (x (elab 10))
      (elab (+ x 20))))
  30)

(check
  (elaborate
    (elab-let
      (x (elab 10))
      (y (elab (+ x 10)))
      (elab (+ x y))))
  30)
