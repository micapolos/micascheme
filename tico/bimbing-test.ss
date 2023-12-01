(import
  (micascheme)
  (tico bimbing))

(lets
  ($bimbing (bimbing 'foo "foo"))
  (run
    (check (bimbing? $bimbing))
    (check (not (bimbing? "foo")))
    (check (equal? (bimbing-key $bimbing) 'foo))
    (check (equal? (bimbing-value $bimbing) "foo"))))
