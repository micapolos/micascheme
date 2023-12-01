; Rename to packet when original packet is removed
(library (tico paco)
  (export
    paco
    paco?
    paco-bimbings
    paco+bimbing)
  (import
    (micascheme)
    (tico bimbing))

  (data (paco bimbings))

  (define (paco+bimbing $paco $bimbing)
    (lets
      ((paco $bimbings) $paco)
      (paco (push $bimbings $bimbing))))
)
