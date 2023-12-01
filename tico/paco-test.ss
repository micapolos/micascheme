(import
  (micascheme)
  (tico paco)
  (tico bimbing))

(lets
  ($paco
    (paco
      (stack
        (bimbing 'foo "foo")
        (bimbing 'bar "bar"))))
  (run
    (check (paco? $paco))
    (check (not (paco? "foo")))
    (check
      (equal?
        (paco-bimbings $paco)
        (stack
          (bimbing 'foo "foo")
          (bimbing 'bar "bar"))))))

(check
  (equal?
    (paco+bimbing
      (paco
        (stack
          (bimbing 'foo "foo")
          (bimbing 'bar "bar")))
      (bimbing 'gar "gar"))
    (paco
      (stack
        (bimbing 'foo "foo")
        (bimbing 'bar "bar")
        (bimbing 'gar "gar")))))
